use miette::NamedSource;
use std::borrow::Cow;
use tree_sitter::{Node, Range};

use crate::{ErrorKind, askama::AskamaNode, range_to_span, session::Session};

#[derive(Debug, Clone)]
pub enum CssNode<'a> {
    RuleSet {
        selector: Cow<'a, str>,
        range: std::ops::Range<usize>,
        end: Option<usize>,
    },
    End {
        start: usize,
    },
    Declaration {
        property_name: Cow<'a, str>,
        value_range: std::ops::Range<usize>,
        full_text: Cow<'a, str>,
        range: std::ops::Range<usize>,
    },
    AtRule {
        text: Cow<'a, str>,
        range: std::ops::Range<usize>,
        end: Option<usize>,
    },
    Comment {
        text: &'a str,
        range: std::ops::Range<usize>,
    },
}

impl CssNode<'_> {
    pub fn range(&self) -> Option<&std::ops::Range<usize>> {
        match self {
            Self::RuleSet { range, .. }
            | Self::Declaration { range, .. }
            | Self::AtRule { range, .. }
            | Self::Comment { range, .. } => Some(range),
            Self::End { .. } => None,
        }
    }

    pub fn start(&self) -> usize {
        match self {
            Self::RuleSet { range, .. }
            | Self::Declaration { range, .. }
            | Self::AtRule { range, .. }
            | Self::Comment { range, .. } => range.start,
            Self::End { start } => *start,
        }
    }

    pub fn text(&self) -> Cow<'_, str> {
        match self {
            Self::RuleSet { selector, .. } => selector.clone(),
            Self::Declaration { full_text, .. } => full_text.clone(),
            Self::AtRule { text, .. } => text.clone(),
            Self::Comment { text, .. } => Cow::Borrowed(text),
            Self::End { .. } => Cow::Borrowed("}"),
        }
    }
}

pub fn extract_css_nodes<'a>(
    session: &mut Session,
    root: &Node,
    source: &'a str,
    ranges: &[Range],
    path: &str,
) -> Vec<CssNode<'a>> {
    let mut css_nodes = Vec::new();

    if ranges.is_empty() {
        return css_nodes;
    }

    let src_bytes = source.as_bytes();
    let mut cursor = root.walk();

    for child in root.children(&mut cursor) {
        parse_css_node(
            &child,
            src_bytes,
            ranges,
            &mut css_nodes,
            session,
            source,
            path,
        );
    }

    css_nodes
}

fn parse_css_node<'a>(
    node: &Node,
    source: &'a [u8],
    ranges: &[Range],
    css_nodes: &mut Vec<CssNode<'a>>,
    session: &mut Session,
    source_str: &str,
    path: &str,
) {
    if node.is_error() || node.is_missing() {
        check_css_error(node, session, source_str, path);
        return;
    }

    match node.kind() {
        "stylesheet" => {
            for child in node.children(&mut node.walk()) {
                parse_css_node(&child, source, ranges, css_nodes, session, source_str, path);
            }
        }
        "rule_set" => {
            let selector_text = extract_selector(node, source, ranges);
            let range = node.start_byte()..node.end_byte();

            let formatted_selector = format!("{} {{", selector_text.trim());

            css_nodes.push(CssNode::RuleSet {
                selector: Cow::Owned(formatted_selector),
                range: range.clone(),
                end: Some(range.end),
            });

            for child in node.children(&mut node.walk()) {
                if child.kind() == "block" {
                    for grandchild in child.children(&mut child.walk()) {
                        if grandchild.kind() == "declaration" {
                            let property_name = extract_property_name(&grandchild, source, ranges);
                            let value_range = extract_value_range(&grandchild);
                            let full_text = extract_text_from_ranges(&grandchild, source, ranges);
                            let decl_range = grandchild.start_byte()..grandchild.end_byte();
                            css_nodes.push(CssNode::Declaration {
                                property_name: Cow::Owned(property_name),
                                value_range,
                                full_text: Cow::Owned(full_text.trim().to_string()),
                                range: decl_range,
                            });
                        }
                    }
                    break;
                }
            }

            css_nodes.push(CssNode::End { start: range.end });
        }
        "declaration" => {
            let property_name = extract_property_name(node, source, ranges);
            let value_range = extract_value_range(node);
            let full_text = extract_text_from_ranges(node, source, ranges);
            let range = node.start_byte()..node.end_byte();
            css_nodes.push(CssNode::Declaration {
                property_name: Cow::Owned(property_name),
                value_range,
                full_text: Cow::Owned(full_text.trim().to_string()),
                range,
            });
        }
        "comment" | "js_comment" => {
            let text = node.utf8_text(source).expect("valid UTF-8");
            let range = node.start_byte()..node.end_byte();
            css_nodes.push(CssNode::Comment { text, range });
        }
        "import_statement"
        | "media_statement"
        | "charset_statement"
        | "namespace_statement"
        | "keyframes_statement"
        | "supports_statement"
        | "scope_statement"
        | "at_rule" => {
            let mut at_rule_text = String::new();
            let mut has_block = false;

            for child in node.children(&mut node.walk()) {
                if child.kind() == "block" {
                    has_block = true;
                } else {
                    if let Ok(text) = child.utf8_text(source) {
                        at_rule_text.push_str(text);
                        at_rule_text.push(' ');
                    }
                }
            }

            if has_block {
                at_rule_text.push('{');
            }

            let range = node.start_byte()..node.end_byte();

            css_nodes.push(CssNode::AtRule {
                text: Cow::Owned(at_rule_text.trim().to_string()),
                range: range.clone(),
                end: if has_block { Some(range.end) } else { None },
            });

            if has_block {
                for child in node.children(&mut node.walk()) {
                    if child.kind() == "block" {
                        for grandchild in child.children(&mut child.walk()) {
                            parse_css_node(
                                &grandchild,
                                source,
                                ranges,
                                css_nodes,
                                session,
                                source_str,
                                path,
                            );
                        }
                        break;
                    }
                }
                css_nodes.push(CssNode::End { start: range.end });
            }
        }
        _ => {
            for child in node.children(&mut node.walk()) {
                parse_css_node(&child, source, ranges, css_nodes, session, source_str, path);
            }
        }
    }
}

fn extract_selector(node: &Node, source: &[u8], _ranges: &[Range]) -> String {
    for child in node.children(&mut node.walk()) {
        if child.kind() == "selectors" {
            return child.utf8_text(source).unwrap_or("").to_string();
        }
    }
    String::new()
}

fn extract_property_name(node: &Node, source: &[u8], ranges: &[Range]) -> String {
    for child in node.children(&mut node.walk()) {
        if child.kind() == "property_name" {
            return extract_text_from_ranges(&child, source, ranges)
                .trim()
                .to_string();
        }
    }
    String::new()
}

fn extract_value_range(node: &Node) -> std::ops::Range<usize> {
    let mut colon_pos = None;
    let mut semicolon_pos = None;

    for child in node.children(&mut node.walk()) {
        match child.kind() {
            ":" => colon_pos = Some(child.end_byte()),
            ";" => {
                semicolon_pos = Some(child.start_byte());
                break;
            }
            "important" => {
                if semicolon_pos.is_none() {
                    semicolon_pos = Some(child.start_byte());
                }
            }
            _ => {}
        }
    }

    let start = colon_pos.unwrap_or(node.start_byte());
    let end = semicolon_pos.unwrap_or(node.end_byte());
    start..end
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

pub fn format_css_node(
    range: std::ops::Range<usize>,
    source: &str,
    askama_nodes: &[AskamaNode],
    embedded_indices: &[usize],
) -> String {
    let mut result = String::new();
    let mut pos = range.start;

    for &idx in embedded_indices {
        if let Some(askama_node) = askama_nodes.get(idx) {
            if pos < askama_node.start() {
                result.push_str(&source[pos..askama_node.start()]);
            }

            result.push_str(&crate::askama::format_askama_node(askama_node));
            pos = askama_node.end();
        }
    }

    if pos < range.end {
        result.push_str(&source[pos..range.end]);
    }

    result
}

fn check_css_error(node: &Node, session: &mut Session, source: &str, path: &str) {
    let message = if node.is_missing() {
        format!("expected {} here", node.kind())
    } else {
        "due to this".to_string()
    };

    let err = ErrorKind::SyntaxError(Box::new(crate::BoxedSyntaxError {
        lang: "CSS".to_string(),
        src: NamedSource::new(path, source.to_string()),
        span: range_to_span(&node.range()),
        message,
    }));

    session.emit_error(&err);
}
