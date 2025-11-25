use anyhow::Result;
use std::ops;
use tree_sitter::{Node, Range};

use crate::{
    askama::{AskamaNode, format_askama_node},
    config::Config,
};

// https://developer.mozilla.org/en-US/docs/Web/HTML/Guides/Content_categories#phrasing_content
const PHRASING_CONTENT: &[&str] = &[
    "abbr", "audio", "b", "bdi", "bdo", "br", "button", "canvas", "cite", "code", "data",
    "datalist", "dfn", "em", "embed", "i", "iframe", "img", "input", "kbd", "label", "mark",
    "math", "meter", "noscript", "object", "output", "picture", "progress", "q", "ruby", "s",
    "samp", "script", "select", "slot", "small", "span", "strong", "sub", "sup", "svg", "template",
    "textarea", "time", "u", "var", "video", "wbr",
    //
    //  Conditionally phrasing
    //
    "a", "area", "del", "ins", "link", "map", "meta",
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
    StartTag {
        name: String,
        attr: String,
        end_tag_idx: Option<usize>,
        embed_askm: Option<Vec<usize>>,
        range: ops::Range<usize>,
    },
    Void {
        name: String,
        attr: String,
        embed_askm: Option<Vec<usize>>,
        range: ops::Range<usize>,
    },
    SelfClosingTag {
        name: String,
        attr: String,
        embed_askm: Option<Vec<usize>>,
        range: ops::Range<usize>,
    },
    EndTag {
        name: String,
        start: usize,
    },

    Text {
        text: String,
        embed_askm: Option<Vec<usize>>,
        range: ops::Range<usize>,
    },
    RawText {
        text: String,
        embed_askm: Option<Vec<usize>>,
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
        embed_askm: Option<Vec<usize>>,
        range: ops::Range<usize>,
    },

    ErroneousEndTag {
        name: String,
        start: usize,
    },
}

impl HtmlNode {
    // https://github.com/tree-sitter/tree-sitter-html/issues/97
    fn is_void(name: &str) -> bool {
        VOID_ELEMENTS.contains(&name.to_lowercase().as_str())
    }

    pub fn start(&self) -> usize {
        match self {
            Self::StartTag { range, .. }
            | Self::Void { range, .. }
            | Self::SelfClosingTag { range, .. }
            | Self::Text { range, .. }
            | Self::RawText { range, .. }
            | Self::Comment { range, .. } => range.start,
            Self::EndTag { start, .. }
            | Self::ErroneousEndTag { start, .. }
            | Self::Doctype { start, .. }
            | Self::Entity { start, .. } => *start,
        }
    }

    pub fn format(&self) -> String {
        match self {
            Self::StartTag { name, attr, .. } => format_opening_tag(name, attr),
            Self::Void { name, attr, .. } | Self::SelfClosingTag { name, attr, .. } => {
                format_self_closing_or_void(name, attr)
            }
            Self::Text { text, .. }
            | Self::RawText { text, .. }
            | Self::Entity { text, .. }
            | Self::Comment { text, .. }
            | Self::Doctype { text, .. } => text.clone(),

            Self::EndTag { name, .. } | Self::ErroneousEndTag { name, .. } => {
                format!("</{}>", name)
            }
        }
    }

    pub fn range(&self) -> Option<&ops::Range<usize>> {
        match self {
            Self::StartTag { range, .. }
            | Self::Void { range, .. }
            | Self::SelfClosingTag { range, .. }
            | Self::Text { range, .. }
            | Self::RawText { range, .. }
            | Self::Comment { range, .. } => Some(range),
            _ => None,
        }
    }

    pub fn is_phrasing(&self) -> bool {
        let (Self::StartTag { name, .. }
        | Self::Void { name, .. }
        | Self::SelfClosingTag { name, .. }
        | Self::EndTag { name, .. }
        | Self::ErroneousEndTag { name, .. }) = self
        else {
            return false;
        };

        PHRASING_CONTENT.contains(&name.to_lowercase().as_str())
    }

    pub fn is_whitespace_sensitive(&self) -> bool {
        let (Self::StartTag { name, .. }
        | Self::EndTag { name, .. }
        | Self::ErroneousEndTag { name, .. }) = self
        else {
            return false;
        };

        WHITESPACE_SENSITIVE.contains(&name.to_lowercase().as_str())
    }

    pub fn is_text(&self) -> bool {
        matches!(self, Self::Text { .. })
    }

    pub fn is_raw_text(&self) -> bool {
        matches!(self, Self::RawText { .. })
    }

    pub fn is_start_tag_or_void(&self) -> bool {
        matches!(
            self,
            Self::StartTag { .. } | Self::Void { .. } | Self::SelfClosingTag { .. }
        )
    }

    pub fn end_tag_idx(&self) -> Option<usize> {
        match self {
            Self::StartTag { end_tag_idx, .. } => *end_tag_idx,
            _ => None,
        }
    }

    pub fn is_comment(&self) -> bool {
        matches!(self, Self::Comment { .. })
    }

    pub fn embed_askm(&self) -> Option<&[usize]> {
        match self {
            Self::StartTag { embed_askm, .. }
            | Self::Void { embed_askm, .. }
            | Self::SelfClosingTag { embed_askm, .. }
            | Self::Comment { embed_askm, .. }
            | Self::Text { embed_askm, .. }
            | Self::RawText { embed_askm, .. } => embed_askm.as_deref(),
            _ => None,
        }
    }
}

pub fn extract_html_nodes(
    root_node: &Node,
    source: &[u8],
    ranges: &[Range],
    askama_nodes: &[AskamaNode],
) -> Result<Vec<HtmlNode>> {
    let mut html_nodes = Vec::new();
    parse_recursive(root_node, source, ranges, askama_nodes, &mut html_nodes, 0)?;
    Ok(html_nodes)
}

fn find_askama_in_range(
    askama_nodes: &[AskamaNode],
    range: &ops::Range<usize>,
) -> Option<Vec<usize>> {
    let indices: Vec<_> = askama_nodes
        .iter()
        .enumerate()
        .filter_map(|(idx, node)| {
            (node.start() >= range.start && node.end() <= range.end).then_some(idx)
        })
        .collect();

    if indices.is_empty() {
        None
    } else {
        Some(indices)
    }
}

// Extract text only from included content ranges, skipping Askama gaps
fn extract_text_from_ranges(
    node: &Node,
    source: &[u8],
    content_ranges: &[Range],
) -> Result<String> {
    let node_start = node.start_byte();
    let node_end = node.end_byte();

    let mut text_parts = Vec::new();
    for range in content_ranges {
        let range_start = range.start_byte;
        let range_end = range.end_byte;

        if range_start < node_end && range_end > node_start {
            let start = range_start.max(node_start);
            let end = range_end.min(node_end);
            let text_slice = std::str::from_utf8(&source[start..end])?;
            text_parts.push(text_slice);
        }
    }

    Ok(text_parts.join(""))
}

fn parse_recursive(
    node: &Node,
    source: &[u8],
    ranges: &[Range],
    askama_nodes: &[AskamaNode],
    html_nodes: &mut Vec<HtmlNode>,
    depth: usize,
) -> Result<()> {
    // Prevent stack overflow on deeply nested HTML
    if depth > 200 {
        anyhow::bail!("nesting too deep");
    }

    match node.kind() {
        "document" => {
            for child in node.children(&mut node.walk()) {
                parse_recursive(&child, source, ranges, askama_nodes, html_nodes, depth + 1)?;
            }
        }
        "doctype" => {
            let text = node.utf8_text(source)?.to_string();
            html_nodes.push(HtmlNode::Doctype {
                text,
                start: node.start_byte(),
            });
        }
        "start_tag" => {
            let range = node.start_byte()..node.end_byte();
            let embed_askm = find_askama_in_range(askama_nodes, &range);
            html_nodes.push(parse_start_tag(node, source, embed_askm));
        }
        "end_tag" => {
            html_nodes.push(parse_end_tag(node, source));
        }
        "self_closing_tag" => {
            let range = node.start_byte()..node.end_byte();
            let embed_askm = find_askama_in_range(askama_nodes, &range);
            html_nodes.push(parse_self_closing_tag(node, source, embed_askm));
        }
        "erroneous_end_tag" => {
            html_nodes.push(parse_erroneous_end_tag(node, source));
        }
        "comment" => {
            let text = node.utf8_text(source)?.to_string();
            let range = node.start_byte()..node.end_byte();
            let embed_askm = find_askama_in_range(askama_nodes, &range);
            html_nodes.push(HtmlNode::Comment {
                text,
                embed_askm,
                range,
            });
        }
        "entity" => {
            let text = node.utf8_text(source)?.to_string();
            html_nodes.push(HtmlNode::Entity {
                text,
                start: node.start_byte(),
            });
        }
        "text" => {
            let text = extract_text_from_ranges(node, source, ranges)?;
            let range = node.start_byte()..node.end_byte();
            let embed_askm = find_askama_in_range(askama_nodes, &range);
            html_nodes.push(HtmlNode::Text {
                text,
                embed_askm,
                range,
            });
        }
        "element" | "script_element" | "style_element" => {
            let start_idx = html_nodes.len();

            let has_end_tag = node
                .child(node.child_count().saturating_sub(1))
                .is_some_and(|n| n.kind() == "end_tag");

            for child in node.children(&mut node.walk()) {
                parse_recursive(&child, source, ranges, askama_nodes, html_nodes, depth + 1)?;
            }

            if has_end_tag {
                let end_idx = html_nodes.len().saturating_sub(1);

                if let Some(HtmlNode::StartTag { end_tag_idx, .. }) = html_nodes.get_mut(start_idx)
                {
                    *end_tag_idx = Some(end_idx);
                }
            }
        }
        "raw_text" => {
            let text = node.utf8_text(source)?;
            if !text.trim().is_empty() {
                let range = node.start_byte()..node.end_byte();
                let embed_askm = find_askama_in_range(askama_nodes, &range);
                html_nodes.push(HtmlNode::RawText {
                    text: text.to_string(),
                    embed_askm,
                    range,
                });
            }
        }
        _ => unreachable!(),
    }
    Ok(())
}

fn parse_start_tag(node: &Node, source: &[u8], embed_askm: Option<Vec<usize>>) -> HtmlNode {
    let tag_name = extract_tag_name(node, source, "tag_name");
    let attr = extract_attr(node, source);

    if HtmlNode::is_void(&tag_name) {
        HtmlNode::Void {
            name: tag_name,
            attr,
            embed_askm,
            range: node.start_byte()..node.end_byte(),
        }
    } else {
        HtmlNode::StartTag {
            name: tag_name,
            attr,
            end_tag_idx: None,
            embed_askm,
            range: node.start_byte()..node.end_byte(),
        }
    }
}

fn parse_self_closing_tag(node: &Node, source: &[u8], embed_askm: Option<Vec<usize>>) -> HtmlNode {
    let tag_name = extract_tag_name(node, source, "tag_name");
    let attr = extract_attr(node, source);

    HtmlNode::SelfClosingTag {
        name: tag_name,
        attr,
        embed_askm,
        range: node.start_byte()..node.end_byte(),
    }
}

fn parse_end_tag(node: &Node, source: &[u8]) -> HtmlNode {
    let tag_name = extract_tag_name(node, source, "tag_name");
    HtmlNode::EndTag {
        name: tag_name,
        start: node.start_byte(),
    }
}

fn parse_erroneous_end_tag(node: &Node, source: &[u8]) -> HtmlNode {
    let tag_name = extract_tag_name(node, source, "erroneous_end_tag_name");
    HtmlNode::ErroneousEndTag {
        name: tag_name,
        start: node.start_byte(),
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

// Reconstruct tag content with formatted Askama expressions
pub fn reconstruct_tag(
    range: &ops::Range<usize>,
    source: &str,
    askama_nodes: &[AskamaNode],
    embed_askm: &[usize],
    config: &Config,
) -> String {
    let mut result = String::new();
    let mut current_pos = range.start;

    for &idx in embed_askm {
        let askama = &askama_nodes[idx];
        if askama.start() > current_pos {
            let fragment = &source[current_pos..askama.start()];
            let normalized = normalize_fragment(fragment);
            result.push_str(&normalized);
        }

        let formatted = format_askama_node(config, askama);
        result.push_str(&formatted);

        current_pos = askama.end();
    }

    if current_pos < range.end {
        let fragment = &source[current_pos..range.end];
        let normalized = normalize_fragment(fragment);
        result.push_str(&normalized);
    }

    result
}

// Reconstruct comment content with formatted Askama expressions
pub fn reconstruct_comment(
    range: &ops::Range<usize>,
    source: &str,
    askama_nodes: &[AskamaNode],
    embed_askm: &[usize],
    config: &Config,
) -> String {
    let mut result = String::new();
    let mut current_pos = range.start;

    for &idx in embed_askm {
        let askama = &askama_nodes[idx];
        if askama.start() > current_pos {
            result.push_str(&source[current_pos..askama.start()]);
        }

        let formatted = format_askama_node(config, askama);
        result.push_str(&formatted);

        current_pos = askama.end();
    }

    if current_pos < range.end {
        result.push_str(&source[current_pos..range.end]);
    }

    result
}

// Normalize tag whitespace, preserving the closing delimiter
fn normalize_fragment(fragment: &str) -> String {
    if let Some(rest) = fragment.strip_suffix('>') {
        let normalized = normalize_preserving_ends(rest);
        format!("{}>", normalized.trim_end())
    } else if let Some(rest) = fragment.strip_suffix("/>") {
        let normalized = normalize_preserving_ends(rest);
        format!("{}/>", normalized.trim_end())
    } else {
        normalize_preserving_ends(fragment)
    }
}

// Normalize whitespace preserving leading/trailing spaces
fn normalize_preserving_ends(text: &str) -> String {
    let has_leading = text.starts_with(char::is_whitespace);
    let has_trailing = text.ends_with(char::is_whitespace);
    let normalized = crate::normalize_whitespace(text);

    match (has_leading, has_trailing) {
        (true, true) => format!(" {} ", normalized),
        (true, false) => format!(" {}", normalized),
        (false, true) => format!("{} ", normalized),
        (false, false) => normalized,
    }
}
