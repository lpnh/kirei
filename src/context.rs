use std::fmt::Write;
use textwrap::{Options, wrap};
use tree_sitter::Node;

use crate::{formatter::Config, restoration::format_node, types::*};

pub(crate) struct FormattingContext<'a> {
    nodes: &'a [AskamaNode],
    config: &'a Config,
    indent_level: i32,
    output: String,
}

impl<'a> FormattingContext<'a> {
    pub fn new(nodes: &'a [AskamaNode], config: &'a Config) -> Self {
        Self {
            nodes,
            config,
            indent_level: 0,
            output: String::new(),
        }
    }

    pub fn finish(self) -> String {
        self.output
    }

    pub fn format_token_stream(&mut self, tokens: &[String]) {
        let significant_tokens: Vec<_> = tokens.iter().filter(|s| !s.trim().is_empty()).collect();
        let mut token_iter = significant_tokens.iter().peekable();

        while let Some(token) = token_iter.next() {
            if let Some(idx) = extract_placeholder_index(token) {
                // Look ahead for an empty block pair
                if let Some(next_token) = token_iter.peek()
                    && let Some(next_idx) = extract_placeholder_index(next_token)
                    && let (Some(node1), Some(node2)) =
                        (self.nodes.get(idx), self.nodes.get(next_idx))
                    && are_empty_block_pair(node1, node2)
                {
                    self.format_empty_block_pair(node1, node2);
                    token_iter.next(); // Consume the next token
                    continue;
                }

                // Regular Askama node processing
                if let Some(node) = self.nodes.get(idx) {
                    self.format_askama_node(node);
                }
            } else if !token.trim().is_empty() {
                // Text content processing
                self.write_wrapped_text(token.trim());
            }
        }
    }

    pub fn format_html_node(
        &mut self,
        node: &Node,
        source: &[u8],
    ) -> Result<(), Box<dyn std::error::Error>> {
        match node.kind() {
            "document" | "fragment" => {
                for child in node.children(&mut node.walk()) {
                    self.format_html_node(&child, source)?;
                }
            }
            "element" => {
                self.format_element(node, source)?;
            }
            "text" => {
                let text = node.utf8_text(source)?;
                let tokens = tokenize(text);
                self.format_token_stream(&tokens);
            }
            _ => {
                self.write_line(node.utf8_text(source)?);
            }
        }
        Ok(())
    }

    fn format_askama_node(&mut self, node: &AskamaNode) {
        let (pre, post) = node.indent_delta();
        self.indent_level = (self.indent_level + pre).max(0);

        let formatted_node = format_node(node, self.indent_level as usize, self.config);
        self.write_line(&formatted_node);

        self.indent_level = (self.indent_level + post).max(0);
    }

    fn format_empty_block_pair(&mut self, opening: &AskamaNode, closing: &AskamaNode) {
        let formatted_opening = format_node(opening, 0, self.config);
        let formatted_closing = format_node(closing, 0, self.config);
        self.write_line(&format!("{}{}", formatted_opening, formatted_closing));
    }

    fn format_element(
        &mut self,
        node: &Node,
        source: &[u8],
    ) -> Result<(), Box<dyn std::error::Error>> {
        if let Some(child) = node.child(0).filter(|c| c.kind() == "self_closing_tag") {
            self.write_line(child.utf8_text(source)?);
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

        if !is_void && should_inline(&content, source, self.config) {
            let content_text = content[0].utf8_text(source)?.trim();
            let end_text = end_tag.map_or(Ok(""), |n| n.utf8_text(source))?;
            self.write_line(&format!("{}{}{}", start_text, content_text, end_text));
        } else {
            self.write_line(start_text);
            if !content.is_empty() {
                self.indent_level += 1;
                for child in content {
                    self.format_html_node(child, source)?;
                }
                self.indent_level -= 1;
            }
            if !is_void && let Some(end) = end_tag {
                self.write_line(end.utf8_text(source)?);
            }
        }

        Ok(())
    }

    // Core writing methods
    fn write_line(&mut self, content: &str) {
        let indented_line = format!("{}{}", self.indent_str(), content);
        writeln!(self.output, "{}", indented_line).ok();
    }

    fn write_wrapped_text(&mut self, text: &str) {
        let prefix = self.indent_str();
        let wrapped_lines = wrap_text_with_indent(text, &prefix, self.config.max_line_length);
        for line in wrapped_lines {
            writeln!(self.output, "{}", line).ok();
        }
    }

    fn indent_str(&self) -> String {
        " ".repeat((self.indent_level as usize) * self.config.indent_size)
    }
}

pub(crate) fn tokenize(input: &str) -> Vec<String> {
    let mut tokens = Vec::new();
    let mut pos = 0;

    while pos < input.len() {
        if let Some(start) = input[pos..].find(ASKAMA_TOKEN) {
            if start > 0 {
                tokens.push(input[pos..pos + start].to_string());
                pos += start;
            }

            if let Some(end) = input[pos..].find(ASKAMA_END_TOKEN) {
                tokens.push(input[pos..pos + end + ASKAMA_END_TOKEN.len()].to_string());
                pos += end + ASKAMA_END_TOKEN.len();
            } else {
                break;
            }
        } else {
            tokens.push(input[pos..].to_string());
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

pub(crate) fn wrap_text_with_indent(text: &str, indent: &str, max_length: usize) -> Vec<String> {
    let available_width = max_length.saturating_sub(indent.len());
    let options = Options::new(available_width);
    wrap(text, &options)
        .into_iter()
        .map(|line| format!("{}{}", indent, line))
        .collect()
}

fn are_empty_block_pair(opening: &AskamaNode, closing: &AskamaNode) -> bool {
    if let (Some((Block::Open, open_type)), Some((Block::Close, close_type))) =
        (opening.get_block_info(), closing.get_block_info())
    {
        return open_type == close_type;
    }
    false
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
