use std::fmt::Write;
use textwrap::{Options, wrap};
use tree_sitter::{Node, Parser};

use tree_sitter_askama::LANGUAGE as ASKAMA_LANGUAGE;
use tree_sitter_html::LANGUAGE as HTML_LANGUAGE;

use crate::{
    extraction::extract_nodes,
    restoration::{format_node, restore_nodes},
    types::*,
};

pub(crate) struct Config {
    pub indent_size: usize,
    pub max_line_length: usize,
}

pub struct AskamaFormatter {
    askama_parser: Parser,
    html_parser: Parser,
    config: Config,
}

impl Default for AskamaFormatter {
    fn default() -> Self {
        Self::new().expect("Failed to create formatter")
    }
}

impl AskamaFormatter {
    fn new() -> Result<Self, Box<dyn std::error::Error>> {
        let mut askama_parser = Parser::new();
        askama_parser.set_language(&ASKAMA_LANGUAGE.into())?;

        let mut html_parser = Parser::new();
        html_parser.set_language(&HTML_LANGUAGE.into())?;

        Ok(Self {
            askama_parser,
            html_parser,
            config: Config {
                indent_size: 4,
                max_line_length: 80,
            },
        })
    }

    pub fn format(&mut self, source: &str) -> Result<String, Box<dyn std::error::Error>> {
        // 1: Use tree-sitter-askama to parse the input
        let ast_tree = self
            .askama_parser
            .parse(source, None)
            .ok_or("Parse failed")?;

        // 2: Split the AST into html content String and AskamaNode
        let (html, nodes) = extract_nodes(source, &ast_tree.root_node())?;

        // 3: Format the html String with placeholders (AskamaNode)
        let formatted = if is_just_template(&html, &nodes) {
            format_template_only(&html, &nodes, &self.config)
        } else {
            format_template_with_html(&mut self.html_parser, &html, &nodes, &self.config)?
        };

        // 4: Restore the original Askama template code
        Ok(restore_nodes(&formatted, &nodes, &self.config))
    }
}

pub(crate) fn format_template_only(html: &str, nodes: &[AskamaNode], config: &Config) -> String {
    let mut indent_level = 0;
    let mut result = String::new();

    format_token_stream(
        &tokenize(html),
        nodes,
        config,
        &mut indent_level,
        &mut result,
    );

    result
}

// Process a stream of text and Askama tokens
fn format_token_stream(
    tokens: &[&str],
    nodes: &[AskamaNode],
    config: &Config,
    indent_level: &mut i32,
    result: &mut String,
) {
    let significant_tokens: Vec<_> = tokens.iter().filter(|s| !s.trim().is_empty()).collect();
    let mut token_iter = significant_tokens.iter().peekable();

    while let Some(token) = token_iter.next() {
        if let Some(idx1) = extract_placeholder_index(token) {
            // Look ahead for an empty block pair
            if let Some(next_token) = token_iter.peek()
                && let Some(idx2) = extract_placeholder_index(next_token)
                && let (Some(node1), Some(node2)) = (nodes.get(idx1), nodes.get(idx2))
                && are_empty_block_pair(node1, node2)
            {
                let formatted_opening = format_node(node1, 0, config);
                let formatted_closing = format_node(node2, 0, config);
                let current_indent = indent_str(*indent_level, config);
                writeln!(
                    result,
                    "{}{}{}",
                    current_indent, formatted_opening, formatted_closing
                )
                .ok();
                token_iter.next(); // Consume the next token
                continue;
            }

            // Regular Askama node processing
            if let Some(node) = nodes.get(idx1) {
                let (pre, post) = node.indent_delta();
                *indent_level = (*indent_level + pre).max(0);
                let current_indent = indent_str(*indent_level, config);
                let formatted_node = format_node(node, *indent_level as usize, config);
                writeln!(result, "{}{}", current_indent, formatted_node).ok();
                *indent_level = (*indent_level + post).max(0);
            }
        } else if !token.trim().is_empty() {
            // Text content processing
            let prefix = indent_str(*indent_level, config);
            let wrapped_lines =
                wrap_text_with_indent(token.trim(), &prefix, config.max_line_length);
            for line in wrapped_lines {
                writeln!(result, "{}", line).ok();
            }
        }
    }
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
    let mut result = String::new();
    let mut current_indent = 0;

    format_html_with_indent(
        &tree.root_node(),
        html.as_bytes(),
        nodes,
        config,
        &mut current_indent,
        &mut result,
    )?;

    Ok(result.trim_end().to_string())
}

pub(crate) fn wrap_text_with_indent(text: &str, indent: &str, max_length: usize) -> Vec<String> {
    let available_width = max_length.saturating_sub(indent.len());
    let options = Options::new(available_width);
    wrap(text, &options)
        .into_iter()
        .map(|line| format!("{}{}", indent, line))
        .collect()
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
            for child in node.children(&mut node.walk()) {
                format_html_with_indent(&child, source, nodes, config, indent, result)?;
            }
        }
        "element" => {
            format_element(node, source, nodes, config, indent, result)?;
        }
        "text" => {
            let text = node.utf8_text(source)?;
            format_token_stream(&tokenize(text), nodes, config, indent, result);
        }
        _ => {
            writeln!(
                result,
                "{}{}",
                indent_str(*indent, config),
                node.utf8_text(source)?
            )
            .ok();
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
    let prefix = indent_str(*indent, config);

    if let Some(child) = node.child(0).filter(|c| c.kind() == "self_closing_tag") {
        writeln!(result, "{}{}", prefix, child.utf8_text(source)?).ok();
        return Ok(());
    }

    let children: Vec<Node> = node.children(&mut node.walk()).collect();
    let maybe_start_tag = children.iter().find(|n| n.kind() == "start_tag");
    let end_tag = children.iter().find(|n| n.kind() == "end_tag");
    let content: Vec<&Node> = children
        .iter()
        .filter(|n| !matches!(n.kind(), "start_tag" | "end_tag" | "self_closing_tag"))
        .collect();

    let Some(start_tag) = maybe_start_tag else {
        return Ok(());
    };

    let start_text = start_tag.utf8_text(source)?;
    let is_void = is_void_element(start_tag, source)?;

    if !is_void && should_inline(&content, source, config) {
        let content_text = content[0].utf8_text(source)?.trim();
        let end_text = end_tag.map_or(Ok(""), |n| n.utf8_text(source))?;
        writeln!(
            result,
            "{}{}{}{}",
            prefix, start_text, content_text, end_text
        )
        .ok();
    } else {
        writeln!(result, "{}{}", prefix, start_text).ok();
        if !content.is_empty() {
            *indent += 1;
            for child in content {
                format_html_with_indent(child, source, nodes, config, indent, result)?;
            }
            *indent -= 1;
        }
        if !is_void && let Some(end) = end_tag {
            writeln!(result, "{}{}", prefix, end.utf8_text(source)?).ok();
        }
    }

    Ok(())
}

fn indent_str(indent_level: i32, config: &Config) -> String {
    " ".repeat((indent_level as usize) * config.indent_size)
}

fn is_just_template(html: &str, nodes: &[AskamaNode]) -> bool {
    let mut stripped_html = html.to_string();
    for (i, _) in nodes.iter().enumerate() {
        stripped_html = stripped_html.replace(&nodes[i].placeholder(i), "");
    }
    stripped_html.trim().is_empty()
}

fn is_void_element(start_tag: &Node, source: &[u8]) -> Result<bool, Box<dyn std::error::Error>> {
    if let Some(tag_name_node) = start_tag
        .children(&mut start_tag.walk())
        .find(|c| c.kind() == "tag_name")
    {
        let tag_name = tag_name_node.utf8_text(source)?.to_lowercase();
        return Ok(matches!(
            tag_name.as_str(),
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
        ));
    }
    Ok(false)
}

fn are_empty_block_pair(opening: &AskamaNode, closing: &AskamaNode) -> bool {
    if let (Some(open_type), Some(close_type)) =
        (opening.get_block_type(), closing.get_block_type())
        && opening.is_opening_block()
        && closing.is_closing_block()
    {
        return close_type == format!("end{}", open_type);
    }
    false
}

fn should_inline(content: &[&Node], source: &[u8], config: &Config) -> bool {
    if content.len() == 1
        && content[0].kind() == "text"
        && let Ok(text) = content[0].utf8_text(source)
    {
        if text.contains(ASKAMA_TOKEN) {
            return false;
        }
        return !text.contains('\n') && text.len() < config.max_line_length / 2;
    }
    false
}
