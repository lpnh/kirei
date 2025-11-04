use anyhow::Result;
use tree_sitter::{Node, Range};

#[derive(Debug, Clone)]
pub enum HtmlNode {
    StartTag {
        name: String,
        attr: String,
        end_tag_idx: Option<usize>,
        start_byte: usize,
        end_byte: usize,
    },
    Void {
        name: String,
        attr: String,
        start_byte: usize,
        end_byte: usize,
    },
    SelfClosingTag {
        name: String,
        attr: String,
        start_byte: usize,
        end_byte: usize,
    },
    EndTag {
        name: String,
        start_byte: usize,
        end_byte: usize,
    },

    Text {
        text: String,
        start_byte: usize,
        end_byte: usize,
    },
    RawText {
        text: String,
        start_byte: usize,
        end_byte: usize,
    },

    Doctype {
        text: String,
        start_byte: usize,
        end_byte: usize,
    },
    Entity {
        text: String,
        start_byte: usize,
        end_byte: usize,
    },
    Comment {
        text: String,
        start_byte: usize,
        end_byte: usize,
    },

    ErroneousEndTag {
        name: String,
        start_byte: usize,
        end_byte: usize,
    },
}

impl HtmlNode {
    pub fn start_byte(&self) -> usize {
        match self {
            Self::StartTag { start_byte, .. }
            | Self::Void { start_byte, .. }
            | Self::SelfClosingTag { start_byte, .. }
            | Self::EndTag { start_byte, .. }
            | Self::ErroneousEndTag { start_byte, .. }
            | Self::Text { start_byte, .. }
            | Self::RawText { start_byte, .. }
            | Self::Doctype { start_byte, .. }
            | Self::Entity { start_byte, .. }
            | Self::Comment { start_byte, .. } => *start_byte,
        }
    }

    pub fn end_byte(&self) -> usize {
        match self {
            Self::StartTag { end_byte, .. }
            | Self::Void { end_byte, .. }
            | Self::SelfClosingTag { end_byte, .. }
            | Self::EndTag { end_byte, .. }
            | Self::ErroneousEndTag { end_byte, .. }
            | Self::Text { end_byte, .. }
            | Self::RawText { end_byte, .. }
            | Self::Doctype { end_byte, .. }
            | Self::Entity { end_byte, .. }
            | Self::Comment { end_byte, .. } => *end_byte,
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

    // Get the tag name if this is any kind of tag
    fn get_tag_name(&self) -> Option<&str> {
        match self {
            Self::StartTag { name, .. }
            | Self::Void { name, .. }
            | Self::EndTag { name, .. }
            | Self::SelfClosingTag { name, .. }
            | Self::ErroneousEndTag { name, .. } => Some(name),
            _ => None,
        }
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

pub fn parse_html_tree_with_ranges(
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
                start_byte: node.start_byte(),
                end_byte: node.end_byte(),
            });
        }
        "start_tag" => html_nodes.push(parse_start_tag(node, source, content_ranges)),
        "end_tag" => html_nodes.push(parse_end_tag(node, source)),
        "self_closing_tag" => html_nodes.push(parse_self_closing_tag(node, source, content_ranges)),
        "erroneous_end_tag" => html_nodes.push(parse_erroneous_end_tag(node, source)),
        "comment" => {
            let text = node.utf8_text(source)?.to_string();
            let normalized = format_comment(&text);
            html_nodes.push(HtmlNode::Comment {
                text: normalized,
                start_byte: node.start_byte(),
                end_byte: node.end_byte(),
            });
        }
        "entity" => {
            let text = node.utf8_text(source)?.to_string();
            html_nodes.push(HtmlNode::Entity {
                text,
                start_byte: node.start_byte(),
                end_byte: node.end_byte(),
            });
        }
        "text" => {
            let text = extract_text_from_ranges(node, source, content_ranges)?;
            html_nodes.push(HtmlNode::Text {
                text,
                start_byte: node.start_byte(),
                end_byte: node.end_byte(),
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

            // Validate that start and end tags match (detect malformed HTML)
            if !is_void_or_self_closing {
                let start_tag_name = html_nodes.get(start_tag_idx).and_then(|n| n.get_tag_name());
                let end_tag_name = html_nodes
                    .get(elem_end_tag_idx)
                    .and_then(|n| n.get_tag_name());

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
                    start_byte: node.start_byte(),
                    end_byte: node.end_byte(),
                });
            }
        }
        _ => {
            // For nodes representing a syntax error...
            if node.child_count() > 0 {
                for child in node.children(&mut node.walk()) {
                    parse_html_node_recursive(
                        &child,
                        source,
                        content_ranges,
                        html_nodes,
                        depth + 1,
                    )?;
                }
            } else {
                // Fallback to HtmlNode::Text
                let text = node.utf8_text(source)?.to_string();
                html_nodes.push(HtmlNode::Text {
                    text,
                    start_byte: node.start_byte(),
                    end_byte: node.end_byte(),
                });
            }
        }
    }
    Ok(())
}

fn parse_start_tag(node: &Node, source: &[u8], content_ranges: &[Range]) -> HtmlNode {
    let tag_name = extract_tag_name(node, source, "tag_name");
    let attr = extract_attr(node, source, content_ranges);

    // Check if this is a void element and create the appropriate variant
    if HtmlNode::is_void_element_name(&tag_name) {
        HtmlNode::Void {
            name: tag_name,
            attr,
            start_byte: node.start_byte(),
            end_byte: node.end_byte(),
        }
    } else {
        HtmlNode::StartTag {
            name: tag_name,
            attr,
            end_tag_idx: None,
            start_byte: node.start_byte(),
            end_byte: node.end_byte(),
        }
    }
}

fn parse_self_closing_tag(node: &Node, source: &[u8], content_ranges: &[Range]) -> HtmlNode {
    let tag_name = extract_tag_name(node, source, "tag_name");
    let attr = extract_attr(node, source, content_ranges);

    HtmlNode::SelfClosingTag {
        name: tag_name,
        attr,
        start_byte: node.start_byte(),
        end_byte: node.end_byte(),
    }
}

fn parse_end_tag(node: &Node, source: &[u8]) -> HtmlNode {
    let tag_name = extract_tag_name(node, source, "tag_name");
    HtmlNode::EndTag {
        name: tag_name,
        start_byte: node.start_byte(),
        end_byte: node.end_byte(),
    }
}

fn parse_erroneous_end_tag(node: &Node, source: &[u8]) -> HtmlNode {
    let tag_name = extract_tag_name(node, source, "erroneous_end_tag_name");
    HtmlNode::ErroneousEndTag {
        name: tag_name,
        start_byte: node.start_byte(),
        end_byte: node.end_byte(),
    }
}

fn extract_tag_name(node: &Node, source: &[u8], kind: &str) -> String {
    node.children(&mut node.walk())
        .find(|c| c.kind() == kind)
        .and_then(|n| n.utf8_text(source).ok())
        .unwrap_or("")
        .to_string()
}

fn extract_attr(node: &Node, source: &[u8], content_ranges: &[Range]) -> String {
    let attr_nodes: Vec<_> = node
        .children(&mut node.walk())
        .filter(|c| c.kind() == "attribute")
        .collect();

    if attr_nodes.is_empty() {
        return String::new();
    }

    // Check for malformed attributes
    let has_errors = attr_nodes.iter().any(|attr| contains_error_recursive(attr));

    // If any error return source as is (but respecting content ranges)
    if has_errors && let (Some(first), Some(last)) = (attr_nodes.first(), attr_nodes.last()) {
        let start = first.start_byte();
        let end = last.end_byte();

        // Extract only from content ranges
        if !content_ranges.is_empty() {
            let mut result = String::new();
            for range in content_ranges {
                if range.start_byte < end && range.end_byte > start {
                    let text_start = range.start_byte.max(start);
                    let text_end = range.end_byte.min(end);
                    if text_start < text_end {
                        result.push_str(
                            std::str::from_utf8(&source[text_start..text_end]).unwrap_or(""),
                        );
                    }
                }
            }
            return result;
        }

        return std::str::from_utf8(&source[start..end])
            .unwrap_or("")
            .to_string();
    }

    // Parse otherwise
    let formatted_attrs: Vec<String> = attr_nodes
        .iter()
        .filter_map(|attr_node| {
            let mut name = None;
            let mut value = None;

            for child in attr_node.children(&mut attr_node.walk()) {
                match child.kind() {
                    "attribute_name" => {
                        name = extract_text_from_node(&child, source, content_ranges);
                    }
                    "attribute_value" | "quoted_attribute_value" => {
                        if let Some(text) = extract_text_from_node(&child, source, content_ranges) {
                            value = Some(strip_quotes(&text));
                        }
                    }
                    _ => {}
                }
            }

            name.map(|n| format_single_attr(&n, value.as_deref()))
        })
        .collect();

    formatted_attrs.join(" ")
}

// Extract text from a node respecting content ranges
fn extract_text_from_node(node: &Node, source: &[u8], content_ranges: &[Range]) -> Option<String> {
    let start = node.start_byte();
    let end = node.end_byte();

    if content_ranges.is_empty() {
        return node.utf8_text(source).ok().map(String::from);
    }

    // Extract text only from content ranges
    let mut result = String::new();
    for range in content_ranges {
        if range.start_byte < end && range.end_byte > start {
            let text_start = range.start_byte.max(start);
            let text_end = range.end_byte.min(end);
            if text_start < text_end
                && let Ok(text) = std::str::from_utf8(&source[text_start..text_end])
            {
                result.push_str(text);
            }
        }
    }

    if result.is_empty() {
        None
    } else {
        Some(result)
    }
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

fn contains_error_recursive(node: &Node) -> bool {
    if node.is_error() {
        return true;
    }
    node.children(&mut node.walk())
        .any(|child| contains_error_recursive(&child))
}
