use anyhow::Result;
use std::ops;
use tree_sitter::{Node, Range};

use crate::{
    askama::{AskamaNode, format_askama_node},
    config::Config,
};

#[derive(Debug, Clone)]
pub enum HtmlNode {
    StartTag {
        name: String,
        attr: String,
        end_tag_idx: Option<usize>,
        range: ops::Range<usize>,
    },
    Void {
        name: String,
        attr: String,
        range: ops::Range<usize>,
    },
    SelfClosingTag {
        name: String,
        attr: String,
        range: ops::Range<usize>,
    },
    EndTag {
        name: String,
        start: usize,
    },

    Text {
        text: String,
        range: ops::Range<usize>,
    },
    RawText {
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

    ErroneousEndTag {
        name: String,
        start: usize,
    },
}

impl HtmlNode {
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

    fn is_void_element(&self) -> bool {
        matches!(self, Self::Void { .. })
    }

    fn is_void_element_name(name: &str) -> bool {
        matches!(
            name.to_lowercase().as_str(),
            "area"
                | "base"
                | "br"
                | "col"
                | "embed"
                | "hr"
                | "img"
                | "input"
                | "link"
                | "meta"
                | "param"
                | "source"
                | "track"
                | "wbr"
        )
    }

    pub fn to_string(&self) -> String {
        match self {
            Self::StartTag { name, attr, .. } => format_opening_tag(name, attr),
            Self::Void { name, attr, .. } | Self::SelfClosingTag { name, attr, .. } => {
                format_self_closing_tag(name, attr)
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
}

pub fn is_inline_tag_name(name: &str) -> bool {
    matches!(
        name.to_lowercase().as_str(),
        "a" | "abbr"
            | "acronym"
            | "b"
            | "bdi"
            | "bdo"
            | "big"
            | "br"
            | "button"
            | "cite"
            | "code"
            | "dfn"
            | "em"
            | "i"
            | "img"
            | "input"
            | "kbd"
            | "label"
            | "map"
            | "mark"
            | "meter"
            | "noscript"
            | "object"
            | "output"
            | "progress"
            | "q"
            | "ruby"
            | "s"
            | "samp"
            | "script"
            | "select"
            | "small"
            | "span"
            | "strong"
            | "sub"
            | "sup"
            | "textarea"
            | "time"
            | "tt"
            | "u"
            | "var"
            | "wbr"
    )
}

pub fn extract_html_nodes(
    root_node: &Node,
    source: &[u8],
    content_ranges: &[Range],
) -> Result<Vec<HtmlNode>> {
    let mut html_nodes = Vec::new();
    parse_html_node_recursive(root_node, source, content_ranges, &mut html_nodes, 0)?;
    Ok(html_nodes)
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

fn parse_html_node_recursive(
    node: &Node,
    source: &[u8],
    content_ranges: &[Range],
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
                parse_html_node_recursive(&child, source, content_ranges, html_nodes, depth + 1)?;
            }
        }
        "doctype" => {
            let text = node.utf8_text(source)?.to_string();
            html_nodes.push(HtmlNode::Doctype {
                text,
                start: node.start_byte(),
            });
        }
        "start_tag" => html_nodes.push(parse_start_tag(node, source)),
        "end_tag" => html_nodes.push(parse_end_tag(node, source)),
        "self_closing_tag" => html_nodes.push(parse_self_closing_tag(node, source)),
        "erroneous_end_tag" => html_nodes.push(parse_erroneous_end_tag(node, source)),
        "comment" => {
            let text = node.utf8_text(source)?.to_string();
            let normalized = format_comment(&text);
            html_nodes.push(HtmlNode::Comment {
                text: normalized,
                range: node.start_byte()..node.end_byte(),
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
            let text = extract_text_from_ranges(node, source, content_ranges)?;
            html_nodes.push(HtmlNode::Text {
                text,
                range: node.start_byte()..node.end_byte(),
            });
        }
        "element" | "script_element" | "style_element" => {
            let start_tag_idx = html_nodes.len();

            for child in node.children(&mut node.walk()) {
                parse_html_node_recursive(&child, source, content_ranges, html_nodes, depth + 1)?;
            }

            let is_void_or_self_closing = html_nodes
                .get(start_tag_idx)
                .is_some_and(HtmlNode::is_void_element);

            let mut elem_end_tag_idx = if is_void_or_self_closing {
                start_tag_idx
            } else {
                html_nodes.len().saturating_sub(1)
            };

            // Validate that start and end tags match (detect unclosed elements)
            if !is_void_or_self_closing {
                let start_tag_name = html_nodes.get(start_tag_idx).and_then(|n| match n {
                    HtmlNode::StartTag { name, .. } => Some(name.as_str()),
                    _ => None,
                });
                let end_tag_name = html_nodes.get(elem_end_tag_idx).and_then(|n| match n {
                    HtmlNode::EndTag { name, .. } | HtmlNode::ErroneousEndTag { name, .. } => {
                        Some(name.as_str())
                    }
                    _ => None,
                });

                if let (Some(start_name), Some(end_name)) = (start_tag_name, end_tag_name)
                    && start_name != end_name
                {
                    // Treat unclosed elements like void elements (point end to start)
                    elem_end_tag_idx = start_tag_idx;
                }
            }

            // Attach metadata
            if let Some(HtmlNode::StartTag { end_tag_idx, .. }) = html_nodes.get_mut(start_tag_idx)
            {
                *end_tag_idx = Some(elem_end_tag_idx);
            }
        }
        "raw_text" => {
            let text = node.utf8_text(source)?;
            if !text.trim().is_empty() {
                html_nodes.push(HtmlNode::RawText {
                    text: text.to_string(),
                    range: node.start_byte()..node.end_byte(),
                });
            }
        }
        _ => unreachable!(),
    }
    Ok(())
}

fn parse_start_tag(node: &Node, source: &[u8]) -> HtmlNode {
    let tag_name = extract_tag_name(node, source, "tag_name");
    let attr = extract_attr(node, source);

    // Check if this is a void element and create the appropriate variant
    if HtmlNode::is_void_element_name(&tag_name) {
        HtmlNode::Void {
            name: tag_name,
            attr,
            range: node.start_byte()..node.end_byte(),
        }
    } else {
        HtmlNode::StartTag {
            name: tag_name,
            attr,
            end_tag_idx: None,
            range: node.start_byte()..node.end_byte(),
        }
    }
}

fn parse_self_closing_tag(node: &Node, source: &[u8]) -> HtmlNode {
    let tag_name = extract_tag_name(node, source, "tag_name");
    let attr = extract_attr(node, source);

    HtmlNode::SelfClosingTag {
        name: tag_name,
        attr,
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

fn format_self_closing_tag(name: &str, attr: &str) -> String {
    if attr.is_empty() {
        format!("<{} />", name)
    } else {
        format!("<{} {} />", name, attr)
    }
}

fn format_comment(content: &str) -> String {
    let open = "<!--";
    let close = "-->";

    let inner = &content[open.len()..content.len() - close.len()];
    let normalized = crate::normalize_whitespace(inner);

    if normalized.is_empty() {
        format!("{}{}", open, close)
    } else {
        format!("{} {} {}", open, normalized, close)
    }
}

// Reconstruct tag content with formatted Askama expressions
pub fn reconstruct_tag(
    range: &ops::Range<usize>,
    source: &str,
    askama_nodes: &[AskamaNode],
    config: &Config,
) -> String {
    let mut result = String::new();
    let mut current_pos = range.start;

    let askama_in_range: Vec<_> = askama_nodes
        .iter()
        .filter(|a| a.start() >= range.start && a.end() <= range.end)
        .collect();

    for askama in askama_in_range {
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
    config: &Config,
) -> String {
    let mut result = String::new();
    let mut current_pos = range.start;

    let askama_in_range: Vec<_> = askama_nodes
        .iter()
        .filter(|a| a.start() >= range.start && a.end() <= range.end)
        .collect();

    for askama in askama_in_range {
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
    let trimmed = text.trim();
    if trimmed.is_empty() {
        return if text.is_empty() {
            String::new()
        } else {
            " ".to_string()
        };
    }

    let has_leading = text.starts_with(char::is_whitespace);
    let has_trailing = text.ends_with(char::is_whitespace);
    let normalized = crate::normalize_whitespace(trimmed);

    match (has_leading, has_trailing) {
        (true, true) => format!(" {} ", normalized),
        (true, false) => format!(" {}", normalized),
        (false, true) => format!("{} ", normalized),
        (false, false) => normalized,
    }
}
