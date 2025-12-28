use std::ops;
use tree_sitter::{Node, Range};

use crate::{
    askama::{self, AskamaNode},
    diagnostics,
    draw::Diagnostic,
};

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
        end: Option<usize>,
        range: ops::Range<usize>,
        indent: isize,
    },
    Void {
        name: String,
        attr: String,
        range: ops::Range<usize>,
    },
    SelfClosing {
        name: String,
        attr: String,
        range: ops::Range<usize>,
    },
    End {
        name: String,
        start: usize,
        indent: isize,
    },

    Text {
        text: String,
        range: ops::Range<usize>,
    },
    Raw {
        text: String,
        range: ops::Range<usize>,
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
        range: ops::Range<usize>,
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

    pub fn range(&self) -> Option<&ops::Range<usize>> {
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

    pub fn end(&self) -> Option<usize> {
        match self {
            Self::Start { end, .. } => *end,
            _ => None,
        }
    }
}

pub fn extract_html_nodes(
    root_node: &Node,
    source: &[u8],
    ranges: &[Range],
    diagnostics: &mut Vec<Diagnostic>,
) -> Vec<HtmlNode> {
    let mut html_nodes = Vec::new();
    let mut tag_stack: Vec<(String, Range, usize)> = Vec::new();
    parse_recursive(
        root_node,
        source,
        ranges,
        &mut html_nodes,
        &mut tag_stack,
        diagnostics,
        0,
    );
    html_nodes
}

fn extract_text_from_ranges(node: &Node, source: &[u8], content_ranges: &[Range]) -> String {
    let node_start = node.start_byte();
    let node_end = node.end_byte();

    let mut text_parts = Vec::new();
    for range in content_ranges {
        let range_start = range.start_byte;
        let range_end = range.end_byte;

        if range_start < node_end && range_end > node_start {
            let start = range_start.max(node_start);
            let end = range_end.min(node_end);
            let text_slice = std::str::from_utf8(&source[start..end]).expect("valid UTF-8");
            text_parts.push(text_slice);
        }
    }

    text_parts.join("")
}

fn parse_recursive(
    node: &Node,
    source: &[u8],
    ranges: &[Range],
    html_nodes: &mut Vec<HtmlNode>,
    tag_stack: &mut Vec<(String, Range, usize)>,
    diagnostics: &mut Vec<Diagnostic>,
    depth: usize,
) {
    if depth > 200 {
        diagnostics.push(diagnostics::nesting_too_deep());
        return;
    }

    match node.kind() {
        "document" => {
            for child in node.children(&mut node.walk()) {
                parse_recursive(
                    &child,
                    source,
                    ranges,
                    html_nodes,
                    tag_stack,
                    diagnostics,
                    depth + 1,
                );
            }
        }
        "doctype" => {
            let text = node.utf8_text(source).expect("valid UTF-8").to_string();
            html_nodes.push(HtmlNode::Doctype {
                text,
                start: node.start_byte(),
            });
        }
        "start_tag" => {
            let html_node = parse_start_tag(node, source);
            if let HtmlNode::Start { name, .. } = &html_node {
                let tag_name_node = node
                    .children(&mut node.walk())
                    .find(|c| c.kind() == "tag_name")
                    .expect("start_tag must have tag_name");

                tag_stack.push((name.clone(), tag_name_node.range(), node.start_byte()));
            }

            html_nodes.push(html_node);
        }
        "end_tag" => {
            let end_tag_node = parse_end_tag(node, source);
            if let HtmlNode::End { name, .. } = &end_tag_node
                && let Some(pos) = tag_stack
                    .iter()
                    .rposition(|(stack_name, _, _)| stack_name == name)
            {
                tag_stack.truncate(pos);
            }

            html_nodes.push(end_tag_node);
        }
        "self_closing_tag" => {
            html_nodes.push(parse_self_closing_tag(node, source));
        }
        "erroneous_end_tag" => {
            if let Some(erroneous_end_tag_name) = node
                .children(&mut node.walk())
                .find(|c| c.kind() == "erroneous_end_tag_name")
                && let Some((expected_name, open_name_range, _)) = tag_stack.last()
            {
                let expected = expected_name.clone();
                let found = extract_tag_name(node, source, "erroneous_end_tag_name");
                let open_range = *open_name_range;
                let close_range = erroneous_end_tag_name.range();

                let source_str = std::str::from_utf8(source).expect("valid UTF-8");
                let diagnostic = diagnostics::erroneous_end_tag(
                    expected,
                    &found,
                    open_range,
                    close_range,
                    source_str,
                );
                diagnostics.push(diagnostic);
            }
        }
        "comment" => {
            let text = node.utf8_text(source).expect("valid UTF-8").to_string();
            let range = node.start_byte()..node.end_byte();
            html_nodes.push(HtmlNode::Comment { text, range });
        }
        "entity" => {
            let text = node.utf8_text(source).expect("valid UTF-8").to_string();
            html_nodes.push(HtmlNode::Entity {
                text,
                start: node.start_byte(),
            });
        }
        "text" => {
            let text = extract_text_from_ranges(node, source, ranges);
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
                    &child,
                    source,
                    ranges,
                    html_nodes,
                    tag_stack,
                    diagnostics,
                    depth + 1,
                );
            }
            if has_end_tag {
                let end_idx = html_nodes.len().saturating_sub(1);
                if let Some(HtmlNode::Start { end, .. }) = html_nodes.get_mut(start_idx) {
                    *end = Some(end_idx);
                }
            }
        }
        "raw_text" => {
            let text = node.utf8_text(source).expect("valid UTF-8").to_string();
            if !text.trim().is_empty() {
                let range = node.start_byte()..node.end_byte();
                html_nodes.push(HtmlNode::Raw { text, range });
            }
        }
        _ => unreachable!(),
    }
}

fn parse_start_tag(node: &Node, source: &[u8]) -> HtmlNode {
    let name = extract_tag_name(node, source, "tag_name");
    let attr = extract_attr(node, source);

    if HtmlNode::is_void(&name) {
        HtmlNode::Void {
            name,
            attr,
            range: node.start_byte()..node.end_byte(),
        }
    } else {
        HtmlNode::Start {
            name,
            attr,
            end: None,
            range: node.start_byte()..node.end_byte(),
            indent: 1,
        }
    }
}

fn parse_self_closing_tag(node: &Node, source: &[u8]) -> HtmlNode {
    let name = extract_tag_name(node, source, "tag_name");
    let attr = extract_attr(node, source);

    HtmlNode::SelfClosing {
        name,
        attr,
        range: node.start_byte()..node.end_byte(),
    }
}

fn parse_end_tag(node: &Node, source: &[u8]) -> HtmlNode {
    let name = extract_tag_name(node, source, "tag_name");
    HtmlNode::End {
        name,
        start: node.start_byte(),
        indent: -1,
    }
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

            format_single_attr(name, value.as_deref())
        })
        .collect();

    formatted_attrs.join(" ")
}

fn strip_quotes(text: &str) -> String {
    text.trim_start_matches(['"', '\''])
        .trim_end_matches(['"', '\''])
        .to_string()
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

pub fn format_tag(
    range: &ops::Range<usize>,
    source: &str,
    askama_nodes: &[AskamaNode],
    embed: &[usize],
) -> String {
    format_with_embedded(range, source, askama_nodes, embed, normalize_fragment)
}

pub fn format_opaque(
    range: &ops::Range<usize>,
    source: &str,
    askama_nodes: &[AskamaNode],
    embed: &[usize],
) -> String {
    format_with_embedded(range, source, askama_nodes, embed, str::to_string)
}

fn normalize_fragment(fragment: &str) -> String {
    if let Some(rest) = fragment.strip_suffix('>') {
        format!("{}>", normalize_preserving_ends(rest).trim_end())
    } else {
        normalize_preserving_ends(fragment)
    }
}

fn normalize_preserving_ends(text: &str) -> String {
    let normalized = crate::normalize_ws(text);
    match (
        text.starts_with(char::is_whitespace),
        text.ends_with(char::is_whitespace),
    ) {
        (true, true) => format!(" {} ", normalized),
        (true, false) => format!(" {}", normalized),
        (false, true) => format!("{} ", normalized),
        (false, false) => normalized,
    }
}

fn format_with_embedded(
    range: &ops::Range<usize>,
    source: &str,
    askama_nodes: &[AskamaNode],
    embed: &[usize],
    transform: fn(&str) -> String,
) -> String {
    let mut result = String::new();
    let mut pos = range.start;

    for &idx in embed {
        let node = &askama_nodes[idx];
        if node.start() > pos {
            result.push_str(&transform(&source[pos..node.start()]));
        }
        result.push_str(&askama::format_askama_node(node));
        pos = node.end();
    }

    if pos < range.end {
        result.push_str(&transform(&source[pos..range.end]));
    }

    result
}

pub fn unpair_crossing_tags(html_nodes: &mut [HtmlNode], crossing_pair_idx: &[(usize, usize)]) {
    for &(start_idx, end_idx) in crossing_pair_idx {
        if let HtmlNode::Start { indent, end, .. } = &mut html_nodes[start_idx] {
            *indent = 0;
            *end = None;
        }
        if let HtmlNode::End { indent, .. } = &mut html_nodes[end_idx] {
            *indent = 0;
        }
    }
}
