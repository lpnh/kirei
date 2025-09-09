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

fn is_just_template(html: &str, nodes: &[AskamaNode]) -> bool {
    // Check if content is just Askama template (no HTML node)
    let mut test = html.to_string();
    for (i, _) in nodes.iter().enumerate() {
        test = test.replace(&nodes[i].placeholder(i), "");
    }
    test.trim().is_empty()
}

pub(crate) fn format_template_only(html: &str, nodes: &[AskamaNode], config: &Config) -> String {
    let mut result = Vec::new();
    let mut indent_size = 0;

    for token in tokenize(html) {
        if let Some(idx) = extract_placeholder_index(token) {
            if let Some(node) = nodes.get(idx) {
                let (pre, post) = node.indent_delta();
                indent_size = (indent_size + pre).max(0);
                result.push(format!(
                    "{}{}",
                    " ".repeat(
                        (indent_size * config.indent_size as i32)
                            .try_into()
                            .unwrap()
                    ),
                    format_node(node, indent_size.try_into().unwrap(), config)
                ));
                indent_size = (indent_size + post).max(0);
            }
        } else if !token.trim().is_empty() {
            let indent = "";
            let wrapped = wrap_text_with_indent(token.trim(), indent, config.max_line_length);
            result.extend(wrapped);
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
            format_text_with_indent(text, nodes, config, indent, result)?;
        }
        _ => {
            let current_indent = " ".repeat((*indent as usize) * config.indent_size);
            result.push_str(&format!("{}{}\n", current_indent, node.utf8_text(source)?));
        }
    }

    Ok(())
}

fn format_text_with_indent(
    text: &str,
    nodes: &[AskamaNode],
    config: &Config,
    indent: &mut i32,
    result: &mut String,
) -> Result<(), Box<dyn std::error::Error>> {
    for token in tokenize(text) {
        if let Some(idx) = extract_placeholder_index(token) {
            if let Some(node) = nodes.get(idx) {
                let (pre, post) = node.indent_delta();
                *indent = (*indent + pre).max(0);
                let node_prefix = " ".repeat((*indent as usize) * config.indent_size);

                // Format the node with proper indentation
                let formatted_node = format_node(node, (*indent).try_into().unwrap_or(0), config);
                result.push_str(&format!("{}{}\n", node_prefix, formatted_node));

                // Apply post-delta for subsequent content
                *indent = (*indent + post).max(0);
            }
        } else if !token.trim().is_empty() {
            // Handle text content with wrapping, using current indent
            let wrapped_lines = wrap_text_content(token, (*indent).try_into().unwrap_or(0), config);
            for line in wrapped_lines {
                result.push_str(&line);
                result.push('\n');
            }
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
    // Handle self-closing tags
    if node.child_count() == 1
        && let Some(child) = node.child(0)
        && child.kind() == "self_closing_tag"
    {
        let prefix = " ".repeat((*indent as usize) * config.indent_size);
        result.push_str(&format!("{}{}\n", prefix, child.utf8_text(source)?));
        return Ok(());
    }

    // Consolidate all node parsing and data extraction at the beginning
    let mut cursor = node.walk();
    let children: Vec<Node> = node.children(&mut cursor).collect();

    let start_tag = children.iter().find(|n| n.kind() == "start_tag");
    let end_tag = children.iter().find(|n| n.kind() == "end_tag");
    let content: Vec<&Node> = children
        .iter()
        .filter(|n| {
            n.kind() != "start_tag" && n.kind() != "end_tag" && n.kind() != "self_closing_tag"
        })
        .collect();

    // If there's no start tag, do NOT format this element
    let start = match start_tag {
        Some(s) => s,
        None => return Ok(()),
    };

    let start_text = start.utf8_text(source)?;
    let is_void = is_void_element(start, source)?;
    let prefix = " ".repeat((*indent as usize) * config.indent_size);

    // Separates the "inline" case from all others
    if !is_void && should_inline(&content, source, nodes, config) {
        // Format as a single, inline element
        let content_text = content
            .first()
            .map_or(Ok(""), |n| n.utf8_text(source))?
            .trim();
        let end_text = end_tag.map_or(Ok(""), |n| n.utf8_text(source))?;

        result.push_str(&format!(
            "{}{}{}{}\n",
            prefix, start_text, content_text, end_text
        ));
    } else {
        // Format as a multi-line block (also handles void elements)
        result.push_str(&format!("{}{}\n", prefix, start_text));

        // Recursively format content, if any exists
        if !content.is_empty() {
            *indent += 1;
            for child in &content {
                format_html_with_indent(child, source, nodes, config, indent, result)?;
            }
            *indent -= 1;
        }

        // Add the closing tag only for non-void elements
        if !is_void && let Some(end) = end_tag {
            result.push_str(&format!("{}{}\n", prefix, end.utf8_text(source)?));
        }
    }

    Ok(())
}

fn is_void_element(start_tag: &Node, source: &[u8]) -> Result<bool, Box<dyn std::error::Error>> {
    let mut cursor = start_tag.walk();
    for child in start_tag.children(&mut cursor) {
        if child.kind() == "tag_name" {
            let tag_name = child.utf8_text(source)?.to_lowercase();
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
    }
    Ok(false)
}

fn should_inline(content: &[&Node], source: &[u8], _nodes: &[AskamaNode], config: &Config) -> bool {
    content.len() == 1
        && content[0].kind() == "text"
        && content[0]
            .utf8_text(source)
            .map(|t| !t.contains('\n') && t.len() < config.max_line_length / 2)
            .unwrap_or(false)
}

fn wrap_text_content(text: &str, indent: usize, config: &Config) -> Vec<String> {
    let prefix = " ".repeat(indent * config.indent_size);
    let mut result = Vec::new();

    for line in text.lines() {
        let trimmed = line.trim();
        if trimmed.is_empty() {
            continue;
        }

        let wrapped = wrap_text_with_indent(trimmed, &prefix, config.max_line_length);
        result.extend(wrapped);
    }

    result
}
