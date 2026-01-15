use miette::NamedSource;
use tree_sitter::{Node, Range};

use crate::{ErrorKind, extract_from_ranges, range_to_span, session::Session};

// https://developer.mozilla.org/en-US/docs/Web/HTML/Guides/Content_categories#phrasing_content
const PHRASING_CONTENT: &[&str] = &[
    "abbr", "audio", "b", "bdi", "bdo", "br", "button", "canvas", "cite", "code", "data",
    "datalist", "dfn", "em", "embed", "i", "iframe", "img", "input", "kbd", "label", "mark",
    "math", "meter", "noscript", "object", "output", "picture", "progress", "q", "ruby", "s",
    "samp", "script", "select", "slot", "small", "span", "strong", "sub", "sup", "svg", "template",
    "textarea", "time", "u", "var", "video", "wbr",
    //
    // === Conditionally phrasing ===
    //
    // only if it contains only phrasing content:
    "a", "del", "ins", "map",
    //
    // only if it is a descendant of a <map> element:
    "area",
    //
    // only if the `itemprop` attribute is present (exclude them for now):
    // "link", "meta",
];

// https://developer.mozilla.org/en-US/docs/Glossary/Void_element
const VOID_ELEMENTS: &[&str] = &[
    "area", "base", "br", "col", "embed", "hr", "img", "input", "link", "meta", "param", "source",
    "track", "wbr",
];

// Tags that preserve whitespace-only content
const WHITESPACE_SENSITIVE: &[&str] = &["title"];

#[derive(Debug, Clone)]
pub enum HtmlNode {
    Start {
        name: String,
        attr: String,
        range: std::ops::Range<usize>,
        end: Option<usize>,
    },
    Void {
        name: String,
        attr: String,
        range: std::ops::Range<usize>,
    },
    SelfClosing {
        name: String,
        attr: String,
        range: std::ops::Range<usize>,
    },
    End {
        name: String,
        start: usize,
        pair: bool,
    },

    Text {
        text: String,
        range: std::ops::Range<usize>,
    },
    Raw {
        text: String,
        range: std::ops::Range<usize>,
    },

    Doctype {
        text: String,
        start: usize,
    },
    Entity {
        text: String,
        start: usize,
    },
    Comment {
        text: String,
        range: std::ops::Range<usize>,
    },
}

impl HtmlNode {
    // https://github.com/tree-sitter/tree-sitter-html/issues/97
    fn is_void(name: &str) -> bool {
        VOID_ELEMENTS.contains(&name.to_lowercase().as_str())
    }

    pub fn start(&self) -> usize {
        match self {
            Self::Start { range, .. }
            | Self::Void { range, .. }
            | Self::SelfClosing { range, .. }
            | Self::Text { range, .. }
            | Self::Raw { range, .. }
            | Self::Comment { range, .. } => range.start,
            Self::End { start, .. } | Self::Doctype { start, .. } | Self::Entity { start, .. } => {
                *start
            }
        }
    }

    pub fn format(&self) -> String {
        match self {
            Self::Start { name, attr, .. } => format_opening_tag(name, attr),
            Self::Void { name, attr, .. } | Self::SelfClosing { name, attr, .. } => {
                format_self_closing_or_void(name, attr)
            }
            Self::Text { text, .. }
            | Self::Raw { text, .. }
            | Self::Entity { text, .. }
            | Self::Comment { text, .. }
            | Self::Doctype { text, .. } => text.clone(),
            Self::End { name, .. } => format!("</{}>", name),
        }
    }

    pub fn range(&self) -> Option<&std::ops::Range<usize>> {
        match self {
            Self::Start { range, .. }
            | Self::Void { range, .. }
            | Self::SelfClosing { range, .. }
            | Self::Text { range, .. }
            | Self::Raw { range, .. }
            | Self::Comment { range, .. } => Some(range),
            _ => None,
        }
    }

    pub fn is_phrasing(&self) -> bool {
        let (Self::Start { name, .. }
        | Self::Void { name, .. }
        | Self::SelfClosing { name, .. }
        | Self::End { name, .. }) = self
        else {
            return false;
        };

        PHRASING_CONTENT.contains(&name.to_lowercase().as_str())
    }

    pub fn is_ws_sensitive(&self) -> bool {
        let (Self::Start { name, .. } | Self::End { name, .. }) = self else {
            return false;
        };

        WHITESPACE_SENSITIVE.contains(&name.to_lowercase().as_str())
    }

    pub fn indent(&self) -> isize {
        match self {
            Self::Start { end, .. } => end.is_some().into(),
            Self::End { pair: true, .. } => -1,
            _ => 0,
        }
    }
}

pub fn extract_html(
    session: &mut Session,
    root: &Node,
    source: &str,
    ranges: &[Range],
    path: &str,
) -> (Vec<HtmlNode>, Vec<Range>) {
    let mut html_nodes = Vec::new();
    let mut raw_node_ranges = Vec::new();
    let mut stack = Vec::new();

    parse_recursive(
        session,
        root,
        source,
        path,
        ranges,
        &mut html_nodes,
        &mut raw_node_ranges,
        &mut stack,
        0,
    );

    (html_nodes, raw_node_ranges)
}

fn parse_recursive(
    session: &mut Session,
    node: &Node,
    source: &str,
    path: &str,
    ranges: &[Range],
    html_nodes: &mut Vec<HtmlNode>,
    raw_node_ranges: &mut Vec<Range>,
    stack: &mut Vec<(String, Range, usize)>,
    depth: usize,
) {
    if depth > 200 {
        session.emit_error(&ErrorKind::NestingTooDeep);
        return;
    }

    let src_bytes = source.as_bytes();

    match node.kind() {
        "document" => {
            for child in node.children(&mut node.walk()) {
                parse_recursive(
                    session,
                    &child,
                    source,
                    path,
                    ranges,
                    html_nodes,
                    raw_node_ranges,
                    stack,
                    depth + 1,
                );
            }
        }
        "doctype" => {
            let text = node.utf8_text(src_bytes).expect("valid UTF-8").to_string();
            html_nodes.push(HtmlNode::Doctype {
                text,
                start: node.start_byte(),
            });
        }
        "start_tag" => {
            let html_node = parse_start_tag(node, src_bytes);
            if let HtmlNode::Start { name, .. } = &html_node {
                let tag_name_node = node
                    .children(&mut node.walk())
                    .find(|c| c.kind() == "tag_name")
                    .expect("start_tag must have tag_name");

                stack.push((name.clone(), tag_name_node.range(), node.start_byte()));
            }
            html_nodes.push(html_node);
        }
        "end_tag" => {
            let end_tag_node = parse_end_tag(node, src_bytes);
            if let HtmlNode::End { name, .. } = &end_tag_node
                && let Some(pos) = stack
                    .iter()
                    .rposition(|(stack_name, _, _)| stack_name == name)
            {
                stack.truncate(pos);
            }
            html_nodes.push(end_tag_node);
        }
        "self_closing_tag" => {
            html_nodes.push(parse_self_closing_tag(node, src_bytes));
        }
        "erroneous_end_tag" => {
            if let Some(err_end_node) = node
                .children(&mut node.walk())
                .find(|c| c.kind() == "erroneous_end_tag_name")
                && let Some((name, open, _)) = stack.last()
            {
                let found = extract_tag_name(node, src_bytes, "erroneous_end_tag_name");
                let err =
                    erroneous_end_tag(name, &found, err_end_node.range(), *open, source, path);
                session.emit_error(&err);
                let end_tag_node = HtmlNode::End {
                    name: found,
                    start: node.start_byte(),
                    pair: false,
                };
                if let HtmlNode::End { name, .. } = &end_tag_node
                    && let Some(pos) = stack
                        .iter()
                        .rposition(|(stack_name, _, _)| stack_name == name)
                {
                    stack.truncate(pos);
                }
                html_nodes.push(end_tag_node);
            }
        }
        "comment" => {
            let text = node.utf8_text(src_bytes).expect("valid UTF-8").to_string();
            let range = node.start_byte()..node.end_byte();
            html_nodes.push(HtmlNode::Comment { text, range });
        }
        "entity" => {
            let text = node.utf8_text(src_bytes).expect("valid UTF-8").to_string();
            let start = node.start_byte();
            html_nodes.push(HtmlNode::Entity { text, start });
        }
        "text" => {
            let text = extract_from_ranges(node, src_bytes, ranges);
            let range = node.start_byte()..node.end_byte();
            html_nodes.push(HtmlNode::Text { text, range });
        }
        "element" | "script_element" | "style_element" => {
            let start_idx = html_nodes.len();
            let has_end_tag = node
                .child(node.child_count().saturating_sub(1) as u32)
                .is_some_and(|n| n.kind() == "end_tag");
            for child in node.children(&mut node.walk()) {
                parse_recursive(
                    session,
                    &child,
                    source,
                    path,
                    ranges,
                    html_nodes,
                    raw_node_ranges,
                    stack,
                    depth + 1,
                );
            }
            if has_end_tag && let Some(HtmlNode::End { start, .. }) = html_nodes.last() {
                let end_idx = *start;
                if let Some(HtmlNode::Start { end, .. }) = html_nodes.get_mut(start_idx) {
                    *end = Some(end_idx);
                }
            }
        }
        "raw_text" => {
            let text = node.utf8_text(src_bytes).expect("valid UTF-8");
            if !text.trim().is_empty() {
                let range_ts = Range {
                    start_byte: node.start_byte(),
                    end_byte: node.end_byte(),
                    start_point: node.start_position(),
                    end_point: node.end_position(),
                };

                let is_style = stack.last().is_some_and(|(name, _, _)| *name == "style");
                if is_style {
                    raw_node_ranges.push(range_ts);
                } else {
                    let text = text.to_string();
                    let range = node.start_byte()..node.end_byte();
                    html_nodes.push(HtmlNode::Raw { text, range });
                }
            }
        }
        _ => unreachable!(),
    }
}

fn parse_start_tag(node: &Node, source: &[u8]) -> HtmlNode {
    let name = extract_tag_name(node, source, "tag_name");
    let attr = extract_attr(node, source);
    let range = node.start_byte()..node.end_byte();

    if HtmlNode::is_void(&name) {
        HtmlNode::Void { name, attr, range }
    } else {
        HtmlNode::Start {
            name,
            attr,
            end: None,
            range,
        }
    }
}

fn parse_self_closing_tag(node: &Node, source: &[u8]) -> HtmlNode {
    let name = extract_tag_name(node, source, "tag_name");
    let attr = extract_attr(node, source);
    let range = node.start_byte()..node.end_byte();
    HtmlNode::SelfClosing { name, attr, range }
}

fn parse_end_tag(node: &Node, source: &[u8]) -> HtmlNode {
    let name = extract_tag_name(node, source, "tag_name");
    let start = node.start_byte();
    let pair = true;
    HtmlNode::End { name, start, pair }
}

fn extract_tag_name(node: &Node, source: &[u8], kind: &str) -> String {
    node.children(&mut node.walk())
        .find(|c| c.kind() == kind)
        .and_then(|n| n.utf8_text(source).ok())
        .expect("tag name must exist")
        .to_string()
}

fn extract_attr(node: &Node, source: &[u8]) -> String {
    let attr_nodes: Vec<_> = node
        .children(&mut node.walk())
        .filter(|c| c.kind() == "attribute")
        .collect();

    if attr_nodes.is_empty() {
        return String::new();
    }

    let formatted_attrs: Vec<String> = attr_nodes
        .iter()
        .map(|attr_node| {
            let name = attr_node
                .children(&mut attr_node.walk())
                .find(|c| c.kind() == "attribute_name")
                .and_then(|n| n.utf8_text(source).ok())
                .expect("attribute must have name");

            let value = attr_node
                .children(&mut attr_node.walk())
                .find(|c| matches!(c.kind(), "attribute_value" | "quoted_attribute_value"))
                .and_then(|n| n.utf8_text(source).ok())
                .map(strip_quotes);

            format_single_attr(name, value)
        })
        .collect();

    formatted_attrs.join(" ")
}

fn strip_quotes(text: &str) -> &str {
    text.trim_start_matches(['"', '\''])
        .trim_end_matches(['"', '\''])
}

fn format_single_attr(name: &str, value: Option<&str>) -> String {
    match value {
        Some(val) => format!("{}=\"{}\"", name, val),
        None => name.to_string(),
    }
}

fn format_opening_tag(name: &str, attr: &str) -> String {
    if attr.is_empty() {
        format!("<{}>", name)
    } else {
        format!("<{} {}>", name, attr)
    }
}

fn format_self_closing_or_void(name: &str, attr: &str) -> String {
    if attr.is_empty() {
        format!("<{} />", name)
    } else {
        format!("<{} {} />", name, attr)
    }
}

pub fn unpair_crossing_tags(html_nodes: &mut [HtmlNode], crossing_pair_idx: &[(usize, usize)]) {
    for &(start_idx, end_idx) in crossing_pair_idx {
        if let HtmlNode::Start { end, .. } = &mut html_nodes[start_idx] {
            *end = None;
        }
        if let HtmlNode::End { pair, .. } = &mut html_nodes[end_idx] {
            *pair = false;
        }
    }
}

pub fn erroneous_end_tag(
    name: &str,
    found: &str,
    close: Range,
    open: Range,
    source: &str,
    filepath: &str,
) -> ErrorKind {
    let suggestion = if name.is_empty() {
        None
    } else {
        Some(format!("consider using `{}`", name))
    };

    ErrorKind::UnexpectedClosingTag(Box::new(crate::BoxedUnexpectedClosingTag {
        expected: name.to_string(),
        found: found.to_string(),
        src: NamedSource::new(filepath, source.to_string()),
        close_span: range_to_span(&close),
        open_span: range_to_span(&open),
        suggestion,
    }))
}
