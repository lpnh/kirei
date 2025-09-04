#![allow(clippy::uninlined_format_args)]
use std::fmt::Write as _;
use tree_sitter::{Node, Parser};
use tree_sitter_askama::LANGUAGE as ASKAMA_LANGUAGE;
use tree_sitter_html::LANGUAGE as HTML_LANGUAGE;

mod helper;
mod types;
use types::*;

pub struct AskamaFormatter {
    askama_parser: Parser,
    html_parser: Parser,
    indent_size: usize,
    max_inline_length: usize,
}

impl AskamaFormatter {
    pub fn new() -> Result<Self, Box<dyn std::error::Error>> {
        let mut askama_parser = Parser::new();
        askama_parser.set_language(&ASKAMA_LANGUAGE.into())?;

        let mut html_parser = Parser::new();
        html_parser.set_language(&HTML_LANGUAGE.into())?;

        Ok(Self {
            askama_parser,
            html_parser,
            indent_size: 2,
            max_inline_length: 80,
        })
    }

    #[allow(dead_code)]
    fn with_indent_size(mut self, size: usize) -> Self {
        self.indent_size = size;
        self
    }

    #[allow(dead_code)]
    fn with_max_inline_length(mut self, length: usize) -> Self {
        self.max_inline_length = length;
        self
    }

    pub fn format(&mut self, source: &str) -> Result<String, Box<dyn std::error::Error>> {
        if source.trim().is_empty() {
            return Ok(String::new());
        }

        // Parse the source with our Askama parser first
        let askama_tree = self
            .askama_parser
            .parse(source, None)
            .ok_or("Failed to parse with Askama parser")?;
        let root = askama_tree.root_node();

        if root.has_error() {
            return Err("Askama parse error: Invalid syntax found".into());
        }

        // Replace Askama nodes with temporary placeholder tokens
        let (html_with_placeholders, placeholders) = self.extract_askama_nodes(source, &root)?;

        // Format the HTML structure while preserving our placeholders
        let formatted_output = self.try_format(&html_with_placeholders, &placeholders)?;

        // Put the original Askama nodes back where they belong
        let result = self.restore_askama_nodes(&formatted_output, &placeholders);

        Ok(result)
    }

    fn extract_askama_nodes(
        &self,
        source: &str,
        root: &Node,
    ) -> Result<(String, Vec<AskamaNode>), Box<dyn std::error::Error>> {
        let mut result = String::new();
        let mut placeholders = Vec::new();
        let mut last_end = 0;

        // Walk through each child node in the AST
        let mut cursor = root.walk();
        for child in root.children(&mut cursor) {
            let start = child.start_byte();
            let end = child.end_byte();

            // Grab any text that comes before this node
            if start > last_end {
                let text_content = &source[last_end..start];
                result.push_str(text_content);
            }

            match child.kind() {
                "control_tag" => {
                    let tag_text = child.utf8_text(source.as_bytes())?;
                    let open_delimiter = child.child(0).unwrap().kind().to_string();
                    let close_delimiter = child.child(2).unwrap().kind().to_string();
                    let inner = helper::extract_inner_content(tag_text, CTRL_OPEN, CTRL_CLOSE);
                    let tag_type = helper::get_tag_type(child);

                    let placeholder = format!(
                        "{}{}{}",
                        ASKAMA_CTRL_TOKEN,
                        placeholders.len(),
                        ASKAMA_END_TOKEN
                    );

                    placeholders.push(AskamaNode::Control {
                        inner,
                        open_delimiter,
                        close_delimiter,
                        tag_type,
                    });
                    result.push_str(&placeholder);
                }
                "render_expression" => {
                    let expr_text = child.utf8_text(source.as_bytes())?;
                    let inner = helper::extract_inner_content(expr_text, EXPR_OPEN, EXPR_CLOSE);
                    let open_delimiter = child.child(0).unwrap().kind().to_string();
                    let close_delimiter = child.child(2).unwrap().kind().to_string();
                    let placeholder = format!(
                        "{}{}{}",
                        ASKAMA_EXPR_TOKEN,
                        placeholders.len(),
                        ASKAMA_END_TOKEN
                    );
                    placeholders.push(AskamaNode::Expression {
                        inner,
                        open_delimiter,
                        close_delimiter,
                    });
                    result.push_str(&placeholder);
                }
                "comment" => {
                    let comment_text = child.utf8_text(source.as_bytes())?;
                    let open_delimiter = child.child(0).unwrap().kind().to_string();
                    let close_delimiter = child.child(1).unwrap().kind().to_string();
                    let inner =
                        helper::extract_inner_content(comment_text, COMMENT_OPEN, COMMENT_CLOSE);
                    let placeholder = format!(
                        "{}{}{}",
                        ASKAMA_COMMENT_TOKEN,
                        placeholders.len(),
                        ASKAMA_END_TOKEN
                    );
                    placeholders.push(AskamaNode::Comment {
                        inner,
                        open_delimiter,
                        close_delimiter,
                    });
                    result.push_str(&placeholder);
                }
                // Everything else gets added as-is
                _ => {
                    let text = child.utf8_text(source.as_bytes())?;
                    result.push_str(text);
                }
            }
            last_end = end;
        }

        // Don't forget any text that comes after the last node
        if last_end < source.len() {
            let text_content = &source[last_end..];
            result.push_str(text_content);
        }

        Ok((result, placeholders))
    }

    fn try_format(
        &mut self,
        html: &str,
        placeholders: &[AskamaNode],
    ) -> Result<String, Box<dyn std::error::Error>> {
        // If we only have template tags and no actual HTML
        // use the formatter that understands the template syntax
        {
            let mut s = html.to_string();
            for i in 0..placeholders.len() {
                let expr = format!("{}{}{}", ASKAMA_EXPR_TOKEN, i, ASKAMA_END_TOKEN);
                let ctrl = format!("{}{}{}", ASKAMA_CTRL_TOKEN, i, ASKAMA_END_TOKEN);
                let comment = format!("{}{}{}", ASKAMA_COMMENT_TOKEN, i, ASKAMA_END_TOKEN);
                s = s.replace(&expr, "");
                s = s.replace(&ctrl, "");
                s = s.replace(&comment, "");
            }
            if s.trim().is_empty() {
                // Pure template content - handle it specially
                return Ok(self.format_template(html, placeholders).trim().to_string());
            }
        }

        // Try to parse as HTML
        let html_tree = self
            .html_parser
            .parse(html, None)
            .ok_or("Failed to parse HTML")?;
        let root = html_tree.root_node();

        // If HTML parsing fails, fall back to basic indentation
        if root.has_error() {
            let res = html
                .lines()
                .map(|line| {
                    let trimmed = line.trim();
                    if trimmed.is_empty() {
                        String::new()
                    } else {
                        format!("{}{}", " ".repeat(self.indent_size), trimmed)
                    }
                })
                .collect::<Vec<_>>()
                .join("\n");

            return Ok(res);
        }

        // Format the HTML with proper structure
        let formatted = self.format_html_node(&root, html.as_bytes(), 0, placeholders)?;

        Ok(formatted.trim().to_string())
    }

    fn format_template(&self, html: &str, placeholders: &[AskamaNode]) -> String {
        // Break down the input into tokens (placeholders and regular text)
        let mut tokens: Vec<&str> = Vec::new();
        let s = html;
        let mut i = 0usize;
        while i < s.len() {
            if let Some(rest) = s.get(i..) {
                if let Some(stripped) = rest.strip_prefix(ASKAMA_TOKEN) {
                    // Found a placeholder - find where it ends
                    if let Some(second_underscore_pos) = stripped.find(ASKAMA_END_TOKEN) {
                        let end =
                            i + ASKAMA_TOKEN.len() + second_underscore_pos + ASKAMA_END_TOKEN.len();
                        if end <= s.len() {
                            tokens.push(&s[i..end]);
                            i = end;
                            continue;
                        }
                    }
                    // Couldn't find proper end, treat as regular text
                    tokens.push(&s[i..]);
                    break;
                } else if let Some(placeholder_start) = rest.find(ASKAMA_TOKEN) {
                    // Add text before the next placeholder
                    let end = i + placeholder_start;
                    if end > i {
                        tokens.push(&s[i..end]);
                    }
                    i = end;
                } else {
                    // No more placeholders found
                    tokens.push(&s[i..]);
                    break;
                }
            } else {
                break;
            }
        }

        // Build the formatted output with proper indentation
        let mut lines: Vec<String> = Vec::new();
        let mut indent_level: i32 = 0;

        for token in tokens {
            helper::format_template_block(
                token,
                &mut indent_level,
                self.indent_size,
                placeholders,
                |formatted_line| {
                    lines.push(formatted_line.to_string());
                },
            );
        }

        lines.join("\n")
    }

    fn format_html_node(
        &self,
        node: &Node,
        source: &[u8],
        indent_level: usize,
        placeholders: &[AskamaNode],
    ) -> Result<String, Box<dyn std::error::Error>> {
        let mut result = String::new();
        let indent = " ".repeat(indent_level * self.indent_size);

        match node.kind() {
            "document" | "fragment" => {
                // Process children while tracking dynamic indent changes from control tags
                let mut cursor = node.walk();
                let mut current_indent: i32 = indent_level as i32;
                for child in node.children(&mut cursor) {
                    if child.kind() == "text" {
                        let text_content = child.utf8_text(source)?;
                        let trimmed = text_content.trim();
                        if !trimmed.is_empty() {
                            if trimmed.contains(ASKAMA_TOKEN) {
                                // Handle mixed text/placeholder content
                                let (s, delta) = self.format_text_with_placeholders(
                                    trimmed,
                                    placeholders,
                                    current_indent as usize,
                                );
                                result.push_str(&s);
                                current_indent = (current_indent + delta).max(0);
                            } else if helper::is_askama_token(trimmed) {
                                writeln!(
                                    result,
                                    "{}{}",
                                    " ".repeat(current_indent as usize * self.indent_size),
                                    trimmed
                                )
                                .unwrap();
                            } else {
                                // Regular text content
                                for line in trimmed.lines() {
                                    let trimmed_line = line.trim();
                                    if !trimmed_line.is_empty() {
                                        writeln!(
                                            result,
                                            "{}{}",
                                            " ".repeat(current_indent as usize * self.indent_size),
                                            trimmed_line
                                        )
                                        .unwrap();
                                    }
                                }
                            }
                        }
                    } else {
                        // Non-text nodes use current indent level
                        let child_str = self.format_html_node(
                            &child,
                            source,
                            current_indent as usize,
                            placeholders,
                        )?;
                        result.push_str(&child_str);
                    }
                }
            }
            "doctype" => {
                writeln!(result, "{}{}", indent, node.utf8_text(source)?).unwrap();
            }
            "element" => {
                result.push_str(&self.format_html_element(
                    node,
                    source,
                    indent_level,
                    placeholders,
                )?);
            }
            "text" => {
                let text_content = node.utf8_text(source)?;
                let trimmed = text_content.trim();
                if !trimmed.is_empty() {
                    if trimmed.contains(ASKAMA_TOKEN) {
                        // Text with embedded placeholders
                        let (s, _delta) =
                            self.format_text_with_placeholders(trimmed, placeholders, indent_level);
                        result.push_str(&s);
                    } else if helper::is_askama_token(trimmed) {
                        writeln!(result, "{}{}", indent, trimmed).unwrap();
                    } else {
                        // Plain text content
                        for line in trimmed.lines() {
                            let trimmed_line = line.trim();
                            if !trimmed_line.is_empty() {
                                writeln!(result, "{}{}", indent, trimmed_line).unwrap();
                            }
                        }
                    }
                }
            }
            _ => {
                // Handle other node types generically
                let content = node.utf8_text(source)?;
                if !content.trim().is_empty() {
                    writeln!(result, "{}{}", indent, content).unwrap();
                }
            }
        }

        Ok(result)
    }

    fn format_html_element(
        &self,
        node: &Node,
        source: &[u8],
        indent_level: usize,
        placeholders: &[AskamaNode],
    ) -> Result<String, Box<dyn std::error::Error>> {
        let mut result = String::new();
        let indent = " ".repeat(indent_level * self.indent_size);

        // Find start_tag, end_tag, and content
        let mut cursor = node.walk();
        let children: Vec<Node> = node.children(&mut cursor).collect();

        let start_tag = children.iter().find(|n| n.kind() == "start_tag");
        let end_tag = children.iter().find(|n| n.kind() == "end_tag");
        let content_children: Vec<&Node> = children
            .iter()
            .filter(|n| n.kind() != "start_tag" && n.kind() != "end_tag")
            .collect();

        // Decide between inline and block formatting
        let should_inline = self.should_be_inline(&content_children, source)?;

        if let Some(start) = start_tag {
            if should_inline && content_children.len() == 1 {
                // Keep it on one line
                let content = content_children[0].utf8_text(source)?.trim();
                if let Some(end) = end_tag {
                    writeln!(
                        result,
                        "{}{}{}{}",
                        indent,
                        start.utf8_text(source)?,
                        content,
                        end.utf8_text(source)?
                    )
                    .unwrap();
                } else {
                    // Self-closing tag
                    writeln!(result, "{}{}", indent, start.utf8_text(source)?).unwrap();
                }
            } else {
                // Multi-line block format
                writeln!(result, "{}{}", indent, start.utf8_text(source)?).unwrap();

                // Format content with tracking for dynamic indent changes
                let mut current_indent: i32 = (indent_level + 1) as i32;

                for child in content_children {
                    match child.kind() {
                        "text" => {
                            let text_content = child.utf8_text(source)?;
                            let trimmed = text_content.trim();
                            if !trimmed.is_empty() {
                                if trimmed.contains(ASKAMA_TOKEN) {
                                    let (s, delta) = self.format_text_with_placeholders(
                                        trimmed,
                                        placeholders,
                                        current_indent as usize,
                                    );
                                    result.push_str(&s);
                                    current_indent = (current_indent + delta).max(0);
                                } else if helper::is_askama_token(trimmed) {
                                    writeln!(
                                        result,
                                        "{}{}",
                                        " ".repeat(current_indent as usize * self.indent_size),
                                        trimmed
                                    )
                                    .unwrap();
                                } else {
                                    result.push_str(&self.format_html_node(
                                        child,
                                        source,
                                        current_indent as usize,
                                        placeholders,
                                    )?);
                                }
                            }
                        }
                        _ => {
                            // Format other child nodes with current indent
                            result.push_str(&self.format_html_node(
                                child,
                                source,
                                current_indent as usize,
                                placeholders,
                            )?);
                        }
                    }
                }

                // Close the element
                if let Some(end) = end_tag {
                    writeln!(result, "{}{}", indent, end.utf8_text(source)?).unwrap();
                }
            }
        }

        Ok(result)
    }

    fn should_be_inline(
        &self,
        content: &[&Node],
        source: &[u8],
    ) -> Result<bool, Box<dyn std::error::Error>> {
        if content.len() != 1 {
            return Ok(false);
        }

        let node = content[0];
        if node.kind() != "text" {
            return Ok(false);
        }

        let text = node.utf8_text(source)?.trim();
        // Placeholders can always be inline regardless of length
        if helper::is_askama_token(text) {
            return Ok(true);
        }
        Ok(!text.contains('\n') && text.len() <= self.max_inline_length)
    }

    fn restore_askama_nodes(&self, formatted_html: &str, placeholders: &[AskamaNode]) -> String {
        let mut result = formatted_html.to_string();

        // Process placeholders in reverse order to avoid replacement conflicts
        let mut indexed_placeholders: Vec<(usize, &AskamaNode)> =
            placeholders.iter().enumerate().collect();
        indexed_placeholders.sort_by(|a, b| b.0.cmp(&a.0));

        for (i, placeholder) in indexed_placeholders {
            let placeholder_marker = match placeholder {
                AskamaNode::Control { .. } => {
                    format!("{}{}{}", ASKAMA_CTRL_TOKEN, i, ASKAMA_END_TOKEN)
                }
                AskamaNode::Expression { .. } => {
                    format!("{}{}{}", ASKAMA_EXPR_TOKEN, i, ASKAMA_END_TOKEN)
                }
                AskamaNode::Comment { .. } => {
                    format!("{}{}{}", ASKAMA_COMMENT_TOKEN, i, ASKAMA_END_TOKEN)
                }
            };

            // Replace the placeholder with the original Askama syntax
            if result.contains(&placeholder_marker) {
                result = result.replace(&placeholder_marker, &placeholder.to_string());
            }
        }

        result
    }

    // Returns formatted text and the net change in indentation level
    fn format_text_with_placeholders(
        &self,
        text: &str,
        placeholders: &[AskamaNode],
        indent_level: usize,
    ) -> (String, i32) {
        // Break text into tokens (placeholders and regular text chunks)
        let mut tokens: Vec<&str> = Vec::new();
        let s = text;
        let mut i = 0usize;
        while i < s.len() {
            if let Some(rest) = s.get(i..) {
                if rest.starts_with(ASKAMA_TOKEN) {
                    // Try to find the complete placeholder
                    if let Some(end_rel) = rest.find(ASKAMA_END_TOKEN) {
                        let end = i + end_rel + ASKAMA_END_TOKEN.len();
                        if end <= s.len() {
                            tokens.push(&s[i..end]);
                            i = end;
                            continue;
                        }
                    }
                    // Fallback if we can't find the end
                    tokens.push(&s[i..]);
                    break;
                } else if let Some(next_placeholder_pos) = rest.find(ASKAMA_TOKEN) {
                    let end = i + next_placeholder_pos;
                    if end > i {
                        tokens.push(&s[i..end]);
                    }
                    i = end;
                } else {
                    // No more placeholders
                    tokens.push(&s[i..]);
                    break;
                }
            } else {
                break;
            }
        }

        let mut result = String::new();
        let mut lvl = indent_level as i32;
        let start_lvl = lvl;

        for token in tokens {
            helper::format_template_block(
                token,
                &mut lvl,
                self.indent_size,
                placeholders,
                |formatted_line| {
                    writeln!(result, "{}", formatted_line).unwrap();
                },
            );
        }

        let net_delta = lvl - start_lvl;
        (result, net_delta)
    }
}
