use std::fmt::Write;
use textwrap::{Options, wrap};
use tree_sitter::Node;

use crate::{config::Config, types::*};

pub(crate) struct LayoutEngine<'a> {
    nodes: &'a [AskamaNode],
    config: &'a Config,
    indent_level: i32,
    output: String,
}

impl<'a> LayoutEngine<'a> {
    pub fn new(nodes: &'a [AskamaNode], config: &'a Config) -> Self {
        Self {
            nodes,
            config,
            indent_level: 0,
            output: String::new(),
        }
    }

    pub fn finish(self) -> String {
        self.output.trim_end().to_string()
    }

    // Main entry point for processing tokens (template syntax and raw text)
    pub fn process_tokens(&mut self, tokens: &[Token]) {
        let mut i = 0;
        while i < tokens.len() {
            match &tokens[i] {
                Token::Placeholder(idx) => {
                    if let Some(node) = self.nodes.get(*idx) {
                        if let Some(skip) = self.try_special_patterns(node, &tokens[i..]) {
                            i += skip;
                            continue;
                        }
                        self.process_node(node);
                    }
                }
                Token::Text(text) => {
                    self.write_text(text);
                }
            }
            i += 1;
        }
    }

    // Main entry point for processing HTML nodes with placeholders
    pub fn process_html(
        &mut self,
        node: &Node,
        source: &[u8],
    ) -> Result<(), Box<dyn std::error::Error>> {
        match node.kind() {
            "document" | "fragment" => {
                for child in node.children(&mut node.walk()) {
                    self.process_html(&child, source)?;
                }
            }
            "element" => self.process_element(node, source)?,
            "text" | "entity" => {
                let text = node.utf8_text(source)?;
                let tokens = tokenize(text);
                self.process_tokens(&tokens);
            }
            _ => self.write_line(node.utf8_text(source)?),
        }
        Ok(())
    }

    // Restore placeholders with formatted template content (for final output)
    pub fn restore_placeholders(html: &str, nodes: &[AskamaNode], config: &Config) -> String {
        let mut result = html.to_string();

        // Process in reverse to avoid index shifting
        for (idx, node) in nodes.iter().enumerate().rev() {
            let placeholder = node.placeholder(idx);
            if let Some(pos) = result.find(&placeholder) {
                let indent = Self::detect_indent(&result, pos, config);
                let formatted = Self::format_askama_node(node, indent, config);
                result = result.replace(&placeholder, &formatted);
            }
        }
        result
    }

    // === NODE PROCESSING ===

    fn process_node(&mut self, node: &AskamaNode) {
        let (pre, post) = node.indent_delta();
        self.indent_level = (self.indent_level + pre).max(0);

        let formatted = Self::format_askama_node(node, self.indent_level as usize, self.config);

        if node.prefers_inline() && !self.output.ends_with('\n') {
            write!(self.output, "{}", formatted).ok();
        } else {
            self.write_line(&formatted);
        }

        self.indent_level = (self.indent_level + post).max(0);
    }

    fn try_special_patterns(&mut self, node: &AskamaNode, tokens: &[Token]) -> Option<usize> {
        // Empty block pair optimization
        if let Some(consumed) = self.try_empty_block_pair(tokens) {
            return Some(consumed);
        }

        // Inline match/when pattern
        if let Some(consumed) = self.try_inline_match_pattern(node, tokens) {
            return Some(consumed);
        }

        None
    }

    fn try_empty_block_pair(&mut self, tokens: &[Token]) -> Option<usize> {
        if tokens.len() >= 2
            && let (Token::Placeholder(idx1), Token::Placeholder(idx2)) = (&tokens[0], &tokens[1])
            && let (Some(node1), Some(node2)) = (self.nodes.get(*idx1), self.nodes.get(*idx2))
            && Self::is_empty_block_pair(node1, node2)
        {
            let open_fmt = Self::format_askama_node(node1, 0, self.config);
            let close_fmt = Self::format_askama_node(node2, 0, self.config);
            self.write_line(&format!("{}{}", open_fmt, close_fmt));
            return Some(2);
        }
        None
    }

    // Still hacky, but better than nothing
    fn try_inline_match_pattern(&mut self, node: &AskamaNode, tokens: &[Token]) -> Option<usize> {
        if let Some((Block::Inner, BlockType::Match)) = node.get_block_info() {
            let mut inline_content = String::new();
            let mut tokens_consumed = 0;

            // Collect everything inline until we hit another when/endmatch block
            // This handles the parsing artifacts by treating everything as one line
            for token in &tokens[1..] {
                match token {
                    Token::Text(text) => {
                        // Replace any newlines with spaces to force inline
                        let inline_text = text.replace('\n', " ");
                        inline_content.push_str(&inline_text);
                    }
                    Token::Placeholder(idx) => {
                        if let Some(inner_node) = self.nodes.get(*idx) {
                            // Check if this is another when/endmatch block - if so, stop
                            if let Some((Block::Inner, BlockType::Match)) =
                                inner_node.get_block_info()
                            {
                                break;
                            }
                            if let Some((Block::Close, BlockType::Match)) =
                                inner_node.get_block_info()
                            {
                                break;
                            }
                            if inner_node.is_expr() {
                                let formatted_expr =
                                    Self::format_askama_node(inner_node, 0, self.config);
                                inline_content.push_str(&formatted_expr);
                            } else {
                                break;
                            }
                        } else {
                            break;
                        }
                    }
                }
                tokens_consumed += 1;
            }

            let formatted_when = Self::format_askama_node(node, 0, self.config);
            let combined_line = format!("{} {}", formatted_when, inline_content.trim_start());

            // Check if we should use inline format
            if !inline_content.trim().is_empty()
                && !combined_line.contains('\n')
                && combined_line.len() < self.config.max_line_length
            {
                if !self.output.ends_with('\n') && !self.output.is_empty() {
                    self.output.push('\n');
                }
                write!(
                    self.output,
                    "{}{}",
                    self.indent_str(),
                    combined_line.trim_end()
                )
                .ok();
                return Some(1 + tokens_consumed);
            }
        }
        None
    }

    // === HTML PROCESSING ===

    fn process_element(
        &mut self,
        node: &Node,
        source: &[u8],
    ) -> Result<(), Box<dyn std::error::Error>> {
        // Handle self-closing tags directly
        if let Some(child) = node.child(0).filter(|c| c.kind() == "self_closing_tag") {
            self.write_line(child.utf8_text(source)?);
            return Ok(());
        }

        let children: Vec<Node> = node.children(&mut node.walk()).collect();
        let Some(start_tag) = children.iter().find(|n| n.kind() == "start_tag") else {
            // If there’s no start tag, this is not an element node
            return Ok(());
        };
        let end_tag = children.iter().find(|n| n.kind() == "end_tag");
        let content: Vec<&Node> = children
            .iter()
            .filter(|n| !matches!(n.kind(), "start_tag" | "end_tag" | "self_closing_tag"))
            .collect();

        let start_text = start_tag.utf8_text(source)?;
        let is_void = Self::is_void_element(start_tag, source)?;

        // Try inlining content if possible
        if !is_void && let Some(content_text) = self.try_inline_content(&content, source) {
            let end_text = end_tag.map_or(Ok(""), |n| n.utf8_text(source))?;
            self.write_line(&format!("{}{}{}", start_text, content_text, end_text));
            return Ok(());
        }

        // Fallback: write start tag + indented multiline content + end tag
        self.write_line(start_text);
        if !content.is_empty() {
            self.indent_level += 1;
            for child in content {
                self.process_html(child, source)?;
            }
            self.indent_level -= 1;
        }
        if !is_void && let Some(end) = end_tag {
            self.write_line(end.utf8_text(source)?);
        }

        Ok(())
    }

    // Try to inline if it is simple and short enough
    // Returns the normalized inline text or None otherwise
    fn try_inline_content(&self, content: &[&Node], source: &[u8]) -> Option<String> {
        if content.is_empty() {
            return None;
        }

        // Only inline if all nodes are text or entity
        if !content
            .iter()
            .all(|n| matches!(n.kind(), "text" | "entity"))
        {
            return None;
        }

        let combined_text = normalize_inline_text(content, source);
        if combined_text.is_empty() {
            return None;
        }

        if combined_text.contains(ASKAMA_TOKEN) {
            // NOTE: currently allowing Askama expressions to be inline
            // return None;
        }

        if combined_text.len() < self.config.max_line_length / 2 {
            Some(combined_text)
        } else {
            None
        }
    }

    // === ASKMA NODE FORMATTING (Static methods for reuse) ===

    fn format_askama_node(node: &AskamaNode, indent: usize, config: &Config) -> String {
        match node {
            AskamaNode::Control { dlmts, inner, .. } => {
                Self::format_control(dlmts, inner, indent, config)
            }
            AskamaNode::Expression { dlmts, inner } => {
                Self::format_expression(dlmts, inner, config)
            }
            AskamaNode::Comment { dlmts, inner } => {
                Self::format_comment(dlmts, inner, indent, config)
            }
        }
    }

    fn format_control(
        dlmts: &(String, String),
        inner: &str,
        indent: usize,
        config: &Config,
    ) -> String {
        if inner.is_empty() {
            return format!("{}{}", dlmts.0, dlmts.1);
        }

        let total_len = dlmts.0.len() + inner.len() + dlmts.1.len() + 2;
        let needs_multiline = inner.contains('\n') || total_len > config.max_line_length * 3 / 2;

        if needs_multiline {
            Self::format_multiline(&dlmts.0, &dlmts.1, inner, indent, config)
        } else {
            format!("{} {} {}", dlmts.0, inner.trim(), dlmts.1)
        }
    }

    fn format_expression(dlmts: &(String, String), inner: &str, config: &Config) -> String {
        if inner.is_empty() {
            return format!("{}{}", dlmts.0, dlmts.1);
        }

        let total_len = dlmts.0.len() + inner.len() + dlmts.1.len() + 2;

        if inner.contains('\n') && inner.lines().count() > 1 {
            format!("{}\n{}\n{}", dlmts.0, inner.trim(), dlmts.1)
        } else if total_len > config.max_line_length * 2 {
            format!("{}\n{}\n{}", dlmts.0, inner.trim(), dlmts.1)
        } else {
            format!("{} {} {}", dlmts.0, inner.trim(), dlmts.1)
        }
    }

    fn format_comment(
        dlmts: &(String, String),
        inner: &str,
        indent: usize,
        config: &Config,
    ) -> String {
        if inner.is_empty() {
            return format!("{}{}", dlmts.0, dlmts.1);
        }

        let total_len = dlmts.0.len() + inner.len() + dlmts.1.len() + 2;

        if !inner.contains('\n') && total_len <= config.max_line_length {
            format!("{} {} {}", dlmts.0, inner.trim(), dlmts.1)
        } else {
            Self::format_multiline(&dlmts.0, &dlmts.1, inner, indent, config)
        }
    }

    fn format_multiline(
        open: &str,
        close: &str,
        inner: &str,
        indent: usize,
        config: &Config,
    ) -> String {
        let base_indent = " ".repeat(indent * config.indent_size);
        let inner_indent = " ".repeat((indent + 1) * config.indent_size);

        let formatted_content = if inner.contains('\n') {
            inner
                .lines()
                .filter(|line| !line.trim().is_empty())
                .map(|line| format!("{}{}", inner_indent, line.trim()))
                .collect::<Vec<_>>()
                .join("\n")
        } else {
            format!("{}{}", inner_indent, inner.trim())
        };

        format!("{}\n{}\n{}{}", open, formatted_content, base_indent, close)
    }

    // === OUTPUT HELPERS ===

    fn write_line(&mut self, content: &str) {
        if !self.output.ends_with('\n') && !self.output.is_empty() {
            self.output.push('\n');
        }
        writeln!(self.output, "{}{}", self.indent_str(), content).ok();
    }

    fn write_text(&mut self, text: &str) {
        let trimmed = text.trim();
        if trimmed.is_empty() {
            return;
        }

        if trimmed.contains('\n') || trimmed.len() > self.config.max_line_length {
            let prefix = self.indent_str();
            let wrapped_lines =
                Self::wrap_text_with_indent(trimmed, &prefix, self.config.max_line_length);

            if !self.output.is_empty() && !self.output.ends_with('\n') {
                self.output.push('\n');
            }

            for line in wrapped_lines {
                writeln!(self.output, "{}", line).ok();
            }
        } else if self.output.ends_with('\n') || self.output.is_empty() {
            self.write_line(trimmed);
        } else {
            write!(self.output, "{}", trimmed).ok();
        }
    }

    fn indent_str(&self) -> String {
        " ".repeat((self.indent_level as usize) * self.config.indent_size)
    }

    // === UTILITY FUNCTIONS ===

    fn detect_indent(text: &str, pos: usize, config: &Config) -> usize {
        let line_start = text[..pos].rfind('\n').map_or(0, |p| p + 1);
        let line_part = &text[line_start..pos];
        let leading_spaces = line_part.len() - line_part.trim_start().len();
        leading_spaces / config.indent_size
    }

    fn is_empty_block_pair(opening: &AskamaNode, closing: &AskamaNode) -> bool {
        matches!(
            (opening.get_block_info(), closing.get_block_info()),
            (Some((Block::Open, open_type)), Some((Block::Close, close_type)))
                if open_type == close_type
        )
    }

    fn is_void_element(
        start_tag: &Node,
        source: &[u8],
    ) -> Result<bool, Box<dyn std::error::Error>> {
        let tag_name = start_tag
            .children(&mut start_tag.walk())
            .find(|c| c.kind() == "tag_name")
            .map(|node| node.utf8_text(source))
            .transpose()?
            .map(str::to_lowercase)
            .unwrap_or_default();

        Ok(matches!(
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
        ))
    }

    fn wrap_text_with_indent(text: &str, indent: &str, max_length: usize) -> Vec<String> {
        let available_width = max_length.saturating_sub(indent.len());
        wrap(text, Options::new(available_width))
            .into_iter()
            .map(|line| format!("{}{}", indent, line))
            .collect()
    }
}

// === TOKEN PROCESSING ===

#[derive(Debug, Clone)]
pub(crate) enum Token {
    // Replace Askama Nodes
    Placeholder(usize),
    // Raw text string not enclosed by any HTML tag
    Text(String),
}

pub(crate) fn tokenize(input: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut pos = 0;

    while pos < input.len() {
        if let Some(start) = input[pos..].find(ASKAMA_TOKEN) {
            if start > 0 {
                tokens.push(Token::Text(input[pos..pos + start].to_string()));
                pos += start;
            }

            if let Some(end) = input[pos..].find(ASKAMA_END_TOKEN) {
                let placeholder = &input[pos..pos + end + ASKAMA_END_TOKEN.len()];
                if let Some(idx) = extract_placeholder_index(placeholder) {
                    tokens.push(Token::Placeholder(idx));
                }
                pos += end + ASKAMA_END_TOKEN.len();
            } else {
                break;
            }
        } else {
            tokens.push(Token::Text(input[pos..].to_string()));
            break;
        }
    }
    tokens
}

fn extract_placeholder_index(token: &str) -> Option<usize> {
    [ASKAMA_CTRL_TOKEN, ASKAMA_EXPR_TOKEN, ASKAMA_COMMENT_TOKEN]
        .iter()
        .find_map(|&prefix| {
            token
                .strip_prefix(prefix)?
                .strip_suffix(ASKAMA_END_TOKEN)?
                .parse()
                .ok()
        })
}

fn normalize_inline_text(content: &[&Node], source: &[u8]) -> String {
    let mut result = String::new();
    let mut first = true;

    for node in content {
        if let Ok(text) = node.utf8_text(source) {
            for word in text.split_whitespace() {
                if !first {
                    result.push(' ');
                }
                result.push_str(word);
                first = false;
            }
        }
    }

    result
}
