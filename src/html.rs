use anyhow::Result;
use tree_sitter::Node;

use crate::askama::{self, AskamaNode};

#[derive(Debug, Clone)]
pub enum HtmlNode {
    StartTag {
        name: String,
        attributes: Vec<Attribute>,
        end_tag_idx: Option<usize>,
    },
    Void {
        name: String,
        attributes: Vec<Attribute>,
    },
    SelfClosingTag {
        name: String,
        attributes: Vec<Attribute>,
    },
    EndTag {
        name: String,
    },

    Doctype(String),
    Entity(String),
    Text(String),
    RawText(String),
    Comment(String),

    ErroneousEndTag {
        name: String,
    },
}

#[derive(Debug, Clone)]
pub struct Attribute {
    name: String,
    value: Option<String>,
}

impl Attribute {
    fn to_string(&self) -> String {
        match &self.value {
            Some(val) => {
                // If value already has quotes (from quoted_attribute_value), use as-is
                if (val.starts_with('"') && val.ends_with('"'))
                    || (val.starts_with('\'') && val.ends_with('\''))
                {
                    format!("{}={}", self.name, val)
                } else {
                    // Unquoted value, add quotes
                    format!("{}=\"{}\"", self.name, val)
                }
            }
            None => self.name.clone(),
        }
    }

    // Replace Askama placeholder in attribute name and value
    fn replace_placeholder(mut self, askama_nodes: &[AskamaNode]) -> Self {
        // Process placeholder in attribute name
        for (idx, askama_node) in askama_nodes.iter().enumerate() {
            let placeholder = askama_node.placeholder(idx);
            if self.name.contains(&placeholder) {
                let askama_str = askama::fmt_node_for_attr_or_raw_text(askama_node);
                self.name = self.name.replace(&placeholder, &askama_str);
            }
        }

        // Process placeholder in attribute value
        self.value = self.value.map(|mut val| {
            for (idx, askama_node) in askama_nodes.iter().enumerate() {
                let placeholder = askama_node.placeholder(idx);
                if val.contains(&placeholder) {
                    let askama_str = askama::fmt_node_for_attr_or_raw_text(askama_node);
                    val = val.replace(&placeholder, &askama_str);
                }
            }
            val
        });

        self
    }
}

impl HtmlNode {
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

    // Elements that can be displayed inline
    pub fn is_inline_level(&self) -> bool {
        // Text, raw text and entities are always inline
        if matches!(self, Self::Text(_) | Self::RawText(_) | Self::Entity(_)) {
            return true;
        }
        // Comments and doctypes are usually not inline
        if matches!(self, Self::Comment(_) | Self::Doctype(_)) {
            return false;
        }

        self.get_tag_name().is_some_and(|name| {
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
        })
    }

    // Check if this is a closing tag
    pub fn is_closing_tag(&self) -> bool {
        matches!(self, Self::EndTag { .. } | Self::ErroneousEndTag { .. })
    }

    // Get the tag name if this is any kind of tag
    fn get_tag_name(&self) -> Option<&str> {
        match self {
            Self::StartTag { name, .. }
            | Self::Void { name, .. }
            | Self::EndTag { name }
            | Self::SelfClosingTag { name, .. }
            | Self::ErroneousEndTag { name } => Some(name),
            _ => None,
        }
    }

    pub fn to_string(&self) -> String {
        match self {
            Self::StartTag {
                name, attributes, ..
            } => {
                if attributes.is_empty() {
                    format!("<{}>", name)
                } else {
                    let attrs_str = attributes
                        .iter()
                        .map(Attribute::to_string)
                        .collect::<Vec<_>>()
                        .join(" ");
                    format!("<{} {}>", name, attrs_str)
                }
            }
            Self::Void { name, attributes } | Self::SelfClosingTag { name, attributes } => {
                if attributes.is_empty() {
                    format!("<{} />", name)
                } else {
                    let attrs_str = attributes
                        .iter()
                        .map(Attribute::to_string)
                        .collect::<Vec<_>>()
                        .join(" ");
                    format!("<{} {} />", name, attrs_str)
                }
            }
            Self::Text(text)
            | Self::RawText(text)
            | Self::Entity(text)
            | Self::Comment(text)
            | Self::Doctype(text) => text.clone(),

            Self::EndTag { name } | Self::ErroneousEndTag { name } => format!("</{}>", name),
        }
    }

    // Replace Askama placeholder in this HTML node
    pub fn replace_placeholder(self, askama_nodes: &[AskamaNode]) -> Self {
        match self {
            Self::StartTag {
                name,
                attributes,
                end_tag_idx,
            } => Self::StartTag {
                name,
                attributes: attributes
                    .into_iter()
                    .map(|attr| attr.replace_placeholder(askama_nodes))
                    .collect(),
                end_tag_idx,
            },
            Self::Void { name, attributes } => Self::Void {
                name,
                attributes: attributes
                    .into_iter()
                    .map(|attr| attr.replace_placeholder(askama_nodes))
                    .collect(),
            },
            Self::SelfClosingTag { name, attributes } => Self::SelfClosingTag {
                name,
                attributes: attributes
                    .into_iter()
                    .map(|attr| attr.replace_placeholder(askama_nodes))
                    .collect(),
            },
            // Other variants don't have attributes, return unchanged
            other => other,
        }
    }
}

pub fn parse_html_tree(root_node: &Node, source: &[u8]) -> Result<Vec<HtmlNode>> {
    let mut html_nodes = Vec::new();
    // The top-level call now returns a count, which we can ignore.
    let _ = parse_html_node_recursive(root_node, source, &mut html_nodes, 0)?;
    Ok(html_nodes)
}

fn parse_html_node(node: &Node, source: &[u8]) -> Result<Option<HtmlNode>> {
    match node.kind() {
        "doctype" => parse_doctype(node, source),
        "start_tag" => Ok(Some(parse_start_tag(node, source))),
        "end_tag" => Ok(Some(parse_end_tag(node, source))),
        "self_closing_tag" => Ok(Some(parse_self_closing_tag(node, source))),
        "erroneous_end_tag" => Ok(Some(parse_erroneous_end_tag(node, source))),
        "comment" => parse_comment(node, source),
        _ => unreachable!(),
    }
}

fn parse_start_tag(node: &Node, source: &[u8]) -> HtmlNode {
    let tag_name = node
        .children(&mut node.walk())
        .find(|c| c.kind() == "tag_name")
        .and_then(|n| n.utf8_text(source).ok())
        .unwrap_or("")
        .to_string();

    let attributes = extract_attributes(node, source);

    // Check if this is a void element and create the appropriate variant
    if HtmlNode::is_void_element_name(&tag_name) {
        HtmlNode::Void {
            name: tag_name,
            attributes,
        }
    } else {
        HtmlNode::StartTag {
            name: tag_name,
            attributes,
            end_tag_idx: None,
        }
    }
}

fn parse_self_closing_tag(node: &Node, source: &[u8]) -> HtmlNode {
    let tag_name = node
        .children(&mut node.walk())
        .find(|c| c.kind() == "tag_name")
        .and_then(|n| n.utf8_text(source).ok())
        .unwrap_or("")
        .to_string();

    let attributes = extract_attributes(node, source);

    HtmlNode::SelfClosingTag {
        name: tag_name,
        attributes,
    }
}

fn parse_end_tag(node: &Node, source: &[u8]) -> HtmlNode {
    let tag_name = node
        .children(&mut node.walk())
        .find(|c| c.kind() == "tag_name")
        .and_then(|n| n.utf8_text(source).ok())
        .unwrap_or("")
        .to_string();

    HtmlNode::EndTag { name: tag_name }
}

fn parse_comment(node: &Node, source: &[u8]) -> Result<Option<HtmlNode>> {
    let text = node.utf8_text(source)?;
    Ok(Some(HtmlNode::Comment(text.to_string())))
}

fn parse_doctype(node: &Node, source: &[u8]) -> Result<Option<HtmlNode>> {
    let text = node.utf8_text(source)?;
    Ok(Some(HtmlNode::Doctype(text.to_string())))
}

fn parse_erroneous_end_tag(node: &Node, source: &[u8]) -> HtmlNode {
    let tag_name = node
        .children(&mut node.walk())
        .find(|c| c.kind() == "erroneous_end_tag_name")
        .and_then(|n| n.utf8_text(source).ok())
        .unwrap_or("")
        .to_string();

    HtmlNode::ErroneousEndTag { name: tag_name }
}

fn extract_attributes(node: &Node, source: &[u8]) -> Vec<Attribute> {
    node.children(&mut node.walk())
        .filter(|c| c.kind() == "attribute")
        .filter_map(|attr_node| {
            let mut name = None;
            let mut value = None;

            for child in attr_node.children(&mut attr_node.walk()) {
                match child.kind() {
                    "attribute_name" => {
                        name = child.utf8_text(source).ok().map(|s| s.to_string());
                    }
                    "attribute_value" | "quoted_attribute_value" => {
                        value = Some(child.utf8_text(source).ok()?.to_string());
                    }
                    _ => {}
                }
            }

            name.map(|n| Attribute { name: n, value })
        })
        .collect()
}

fn parse_html_node_recursive(
    node: &Node,
    source: &[u8],
    html_nodes: &mut Vec<HtmlNode>,
    depth: usize,
) -> Result<usize> {
    // Prevent stack overflow on deeply nested HTML
    if depth > 200 {
        anyhow::bail!("nesting too deep");
    }

    let mut curr_chars_count = 0;

    match node.kind() {
        "document" | "script_element" | "style_element" => {
            for child in node.children(&mut node.walk()) {
                curr_chars_count +=
                    parse_html_node_recursive(&child, source, html_nodes, depth + 1)?;
            }
        }
        "doctype" | "start_tag" | "end_tag" | "self_closing_tag" | "erroneous_end_tag"
        | "comment" => {
            if let Some(html_node) = parse_html_node(node, source)? {
                // Count the actual characters in the tag markup
                let tag_text = node.utf8_text(source)?;
                curr_chars_count = tag_text.chars().count();
                html_nodes.push(html_node);
            }
        }
        "entity" => {
            let entity = node.utf8_text(source)?;
            curr_chars_count = entity.chars().count();
            html_nodes.push(HtmlNode::Entity(entity.to_string()));
        }
        "text" => {
            let text = node.utf8_text(source)?;
            if !text.trim().is_empty() {
                curr_chars_count = text.chars().count();
                html_nodes.push(HtmlNode::Text(text.to_string()));
            }
        }
        "element" => {
            let start_tag_idx = html_nodes.len();

            let mut total_child_chars = 0;

            for child in node.children(&mut node.walk()) {
                total_child_chars +=
                    parse_html_node_recursive(&child, source, html_nodes, depth + 1)?;
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
            curr_chars_count = total_child_chars;
        }
        "raw_text" => {
            let text = node.utf8_text(source)?;
            if !text.trim().is_empty() {
                curr_chars_count = text.chars().count();
                html_nodes.push(HtmlNode::RawText(text.to_string()));
            }
        }
        _ => {
            // For nodes representing a syntax error...
            if node.child_count() > 0 {
                // Recurse and aggregate char counts from children if any
                for child in node.children(&mut node.walk()) {
                    curr_chars_count +=
                        parse_html_node_recursive(&child, source, html_nodes, depth + 1)?;
                }
            } else {
                // Fallback to HtmlNode::Text
                let text = node.utf8_text(source)?;
                if !text.trim().is_empty() {
                    curr_chars_count = text.chars().count();
                    html_nodes.push(HtmlNode::Text(text.to_string()));
                }
            }
        }
    }
    Ok(curr_chars_count)
}
