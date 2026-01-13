use miette::NamedSource;
use std::borrow::Cow;
use tree_sitter::{Node, Range};

use crate::{ErrorKind, extract_from_ranges, range_to_span, session::Session};

#[derive(Debug, Clone)]
pub enum CssNode<'a> {
    RuleSet {
        selector: Cow<'a, str>,
        range: std::ops::Range<usize>,
        end: Option<usize>,
    },
    End(usize),
    Declaration {
        content: Cow<'a, str>,
        range: std::ops::Range<usize>,
    },
    AtRule {
        content: Cow<'a, str>,
        range: std::ops::Range<usize>,
        end: Option<usize>,
    },
    Comment {
        content: &'a str,
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
            Self::End(start) => *start,
        }
    }

    pub fn content(&self) -> Cow<'_, str> {
        match self {
            Self::RuleSet { selector, .. } => selector.clone(),
            Self::Declaration { content, .. } | Self::AtRule { content, .. } => content.clone(),
            Self::Comment { content, .. } => Cow::Borrowed(content),
            Self::End { .. } => Cow::Borrowed("}"),
        }
    }
}

pub fn extract_css<'a>(
    session: &mut Session,
    root: &Node,
    src: &'a str,
    ranges: &[Range],
    path: &str,
) -> Vec<CssNode<'a>> {
    let mut css_nodes = Vec::new();

    if ranges.is_empty() {
        return css_nodes;
    }

    let mut cursor = root.walk();

    for child in root.children(&mut cursor) {
        parse_css_recursive(&child, ranges, &mut css_nodes, session, src, path);
    }

    css_nodes
}

fn parse_css_recursive<'a>(
    node: &Node,
    ranges: &[Range],
    css_nodes: &mut Vec<CssNode<'a>>,
    sess: &mut Session,
    src: &'a str,
    path: &str,
) {
    if node.is_error() || node.is_missing() {
        check_css_error(node, sess, src, path);
        return;
    }

    match node.kind() {
        "rule_set" => {
            let selector_text = extract_selector(node, src.as_bytes(), ranges);
            let range = node.start_byte()..node.end_byte();

            let mut start = None;
            let mut end = None;

            for child in node.children(&mut node.walk()) {
                if child.kind() == "block" {
                    for g_child in child.children(&mut child.walk()) {
                        let start_byte = g_child.start_byte();
                        let end_byte = g_child.end_byte();
                        let kind = g_child.kind();
                        if kind == "{" {
                            start = Some(end_byte);
                        } else if kind == "}" {
                            end = Some(start_byte);
                        } else if kind == "declaration" {
                            let content = extract_from_ranges(&g_child, src.as_bytes(), ranges);
                            css_nodes.push(CssNode::Declaration {
                                content: Cow::Owned(content),
                                range: start_byte..end_byte,
                            });
                        } else {
                            parse_css_recursive(&g_child, ranges, css_nodes, sess, src, path);
                        }
                    }
                    break;
                }
            }

            css_nodes.push(CssNode::RuleSet {
                selector: Cow::Owned(format!("{} {{", selector_text)),
                range: range.start..start.unwrap_or(range.start),
                end: Some(end.unwrap_or(range.end)),
            });
            css_nodes.push(CssNode::End(end.unwrap_or(range.end)));
        }
        "declaration" => {
            let content = Cow::Owned(
                extract_from_ranges(node, src.as_bytes(), ranges)
                    .trim()
                    .to_string(),
            );
            let range = node.start_byte()..node.end_byte();
            css_nodes.push(CssNode::Declaration { content, range });
        }
        "comment" | "js_comment" => {
            let content = node.utf8_text(src.as_bytes()).expect("valid UTF-8");
            let range = node.start_byte()..node.end_byte();
            css_nodes.push(CssNode::Comment { content, range });
        }
        "import_statement"
        | "media_statement"
        | "charset_statement"
        | "namespace_statement"
        | "keyframes_statement"
        | "supports_statement"
        | "scope_statement"
        | "at_rule" => {
            let mut content = String::new();
            let mut has_block = false;
            let range = node.start_byte()..node.end_byte();

            let mut start = None;
            let mut end = None;

            for child in node.children(&mut node.walk()) {
                if child.kind() == "block" {
                    has_block = true;
                    for g_child in child.children(&mut child.walk()) {
                        if g_child.kind() == "{" {
                            start = Some(g_child.end_byte());
                        } else if g_child.kind() == "}" {
                            end = Some(g_child.start_byte());
                        } else {
                            parse_css_recursive(&g_child, ranges, css_nodes, sess, src, path);
                        }
                    }
                } else if let Ok(text) = child.utf8_text(src.as_bytes()) {
                    content.push_str(text);
                    content.push(' ');
                }
            }

            if has_block {
                content.push('{');
            }

            let content = Cow::Owned(content.trim().to_string());

            if has_block {
                css_nodes.push(CssNode::AtRule {
                    content,
                    range: range.start..start.unwrap(),
                    end,
                });
                css_nodes.push(CssNode::End(end.unwrap()));
            } else {
                css_nodes.push(CssNode::AtRule {
                    content,
                    range: range.start..start.unwrap(),
                    end: None,
                });
            }
        }
        _ => {
            for child in node.children(&mut node.walk()) {
                parse_css_recursive(&child, ranges, css_nodes, sess, src, path);
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

fn check_css_error(node: &Node, session: &mut Session, source: &str, path: &str) {
    let message = if node.is_missing() {
        format!("expected {} here", node.kind())
    } else {
        "due to this".to_string()
    };

    session.emit_error(&ErrorKind::SyntaxError(Box::new(crate::BoxedSyntaxError {
        lang: "CSS".to_string(),
        src: NamedSource::new(path, source.to_string()),
        span: range_to_span(&node.range()),
        message,
    })));
}
