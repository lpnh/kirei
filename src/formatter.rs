use tree_sitter::{Node, Parser};

use crate::Config;
use crate::restoration::format_node;
use crate::types::*;

pub(crate) fn format_template_only(html: &str, nodes: &[AskamaNode], config: &Config) -> String {
    let mut result = Vec::new();
    let mut indent = 0;

    for token in tokenize(html) {
        if let Some(idx) = extract_placeholder_index(token) {
            if let Some(node) = nodes.get(idx) {
                let (pre, post) = node.indent_delta();
                indent = (indent + pre).max(0);
                result.push(format!(
                    "{}{}",
                    " ".repeat((indent * config.indent_size as i32).try_into().unwrap()),
                    format_node(node, indent.try_into().unwrap(), config)
                ));
                indent = (indent + post).max(0);
            }
        } else if !token.trim().is_empty() {
            for line in token.lines() {
                let trimmed = line.trim();
                if !trimmed.is_empty() {
                    result.push(format!(
                        "{}{}",
                        " ".repeat((indent * config.indent_size as i32).try_into().unwrap()),
                        trimmed
                    ));
                }
            }
        }
    }

    result.join("\n")
}

fn tokenize(input: &str) -> Vec<&str> {
    let mut tokens = Vec::new();
    let mut pos = 0;

    while pos < input.len() {
        if let Some(start) = input[pos..].find(ASKAMA_TOKEN) {
            if start > 0 {
                tokens.push(&input[pos..pos + start]);
                pos += start;
            }

            if let Some(end) = input[pos..].find(ASKAMA_END_TOKEN) {
                tokens.push(&input[pos..pos + end + ASKAMA_END_TOKEN.len()]);
                pos += end + ASKAMA_END_TOKEN.len();
            } else {
                break;
            }
        } else {
            tokens.push(&input[pos..]);
            break;
        }
    }

    tokens
}

pub(crate) fn extract_placeholder_index(token: &str) -> Option<usize> {
    for prefix in &[ASKAMA_CTRL_TOKEN, ASKAMA_EXPR_TOKEN, ASKAMA_COMMENT_TOKEN] {
        if let Some(rest) = token.strip_prefix(prefix)
            && let Some(idx_str) = rest.strip_suffix(ASKAMA_END_TOKEN)
        {
            return idx_str.parse().ok();
        }
    }
    None
}

pub(crate) fn format_template_with_html(
    html_parser: &mut Parser,
    html: &str,
    nodes: &[AskamaNode],
    config: &Config,
) -> Result<String, Box<dyn std::error::Error>> {
    let tree = html_parser.parse(html, None).ok_or("HTML parse failed")?;
    let node = &tree.root_node();
    let source = html.as_bytes();
    let mut result = String::new();
    let mut current_indent = 0;

    format_html_with_indent(
        node,
        source,
        nodes,
        config,
        &mut current_indent,
        &mut result,
    )?;

    Ok(result)
}

fn format_html_with_indent(
    node: &Node,
    source: &[u8],
    nodes: &[AskamaNode],
    config: &Config,
    indent: &mut i32,
    result: &mut String,
) -> Result<(), Box<dyn std::error::Error>> {
    match node.kind() {
        "document" | "fragment" => {
            let mut cursor = node.walk();
            for child in node.children(&mut cursor) {
                format_html_with_indent(&child, source, nodes, config, indent, result)?;
            }
        }
        "element" => {
            format_element(node, source, nodes, config, indent, result)?;
        }
        "text" => {
            let text = node.utf8_text(source)?;
            for token in tokenize(text) {
                if let Some(idx) = extract_placeholder_index(token) {
                    if let Some(node) = nodes.get(idx) {
                        let (pre, post) = node.indent_delta();
                        *indent = (*indent + pre).max(0);
                        let node_prefix = " ".repeat((*indent as usize) * config.indent_size);
                        result.push_str(&format!("{}{}\n", node_prefix, token.trim()));
                        *indent = (*indent + post).max(0);
                    }
                } else if !token.trim().is_empty() {
                    let current_prefix = " ".repeat((*indent as usize) * config.indent_size);
                    result.push_str(&format!("{}{}\n", current_prefix, token.trim()));
                }
            }
        }
        _ => {
            let current_prefix = " ".repeat((*indent as usize) * config.indent_size);
            result.push_str(&format!("{}{}\n", current_prefix, node.utf8_text(source)?));
        }
    }

    Ok(())
}

fn format_element(
    node: &Node,
    source: &[u8],
    nodes: &[AskamaNode],
    config: &Config,
    indent: &mut i32,
    result: &mut String,
) -> Result<(), Box<dyn std::error::Error>> {
    let prefix = " ".repeat((*indent as usize) * config.indent_size);

    let mut cursor = node.walk();
    let children: Vec<Node> = node.children(&mut cursor).collect();

    let start_tag = children.iter().find(|n| n.kind() == "start_tag");
    let end_tag = children.iter().find(|n| n.kind() == "end_tag");
    let content: Vec<&Node> = children
        .iter()
        .filter(|n| n.kind() != "start_tag" && n.kind() != "end_tag")
        .collect();

    if let Some(start) = start_tag {
        let start_text = start.utf8_text(source)?;

        if should_inline(&content, source, nodes, config) {
            if let Some(end) = end_tag {
                let content_text = content[0].utf8_text(source)?.trim();
                result.push_str(&format!(
                    "{}{}{}{}\n",
                    prefix,
                    start_text,
                    content_text,
                    end.utf8_text(source)?
                ));
            }
        } else {
            result.push_str(&format!("{}{}\n", prefix, start_text));
            *indent += 1;
            for child in content {
                format_html_with_indent(child, source, nodes, config, indent, result)?;
            }
            *indent -= 1;

            if let Some(end) = end_tag {
                result.push_str(&format!("{}{}\n", prefix, end.utf8_text(source)?));
            }
        }
    }

    Ok(())
}

fn should_inline(content: &[&Node], source: &[u8], _nodes: &[AskamaNode], config: &Config) -> bool {
    content.len() == 1
        && content[0].kind() == "text"
        && content[0]
            .utf8_text(source)
            .map(|t| !t.contains('\n') && t.len() < config.max_line_length / 2)
            .unwrap_or(false)
}
