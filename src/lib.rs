use tree_sitter::{Node, Parser};
use tree_sitter_askama::LANGUAGE as ASKAMA_LANGUAGE;
use tree_sitter_html::LANGUAGE as HTML_LANGUAGE;

pub enum Placeholder {
    Expression(String),
    Control(String),
    Comment(String),
}

impl Placeholder {
    fn to_string(&self) -> String {
        match self {
            Placeholder::Expression(s) => s.clone(),
            Placeholder::Control(s) => s.clone(),
            Placeholder::Comment(s) => s.clone(),
        }
    }
}

pub struct AskamaFormatter {
    pub askama_parser: Parser,
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
            max_inline_length: 50,
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

        // Step 1: Parse with Askama parser
        let askama_tree = self
            .askama_parser
            .parse(source, None)
            .ok_or("Failed to parse with Askama parser")?;
        let root = askama_tree.root_node();

        if root.has_error() {
            return Err("Askama parse error: Invalid syntax found".into());
        }

        // Step 2: Reconstruct HTML by replacing Askama expressions with placeholders
        let (html_with_placeholders, placeholders) = self.extract_expressions(source, &root)?;

        // Step 3: Parse and format the HTML
        let formatted_html = self.format_html(&html_with_placeholders)?;

        // Step 4: Restore Askama expressions
        let final_result = self.restore_expressions(&formatted_html, &placeholders);

        Ok(final_result)
    }

    pub fn extract_expressions(
        &self,
        source: &str,
        node: &Node,
    ) -> Result<(String, Vec<Placeholder>), Box<dyn std::error::Error>> {
        let mut result = String::new();
        let mut placeholders = Vec::new();

        self.extract_expressions_recursive(source, node, &mut result, &mut placeholders)?;

        Ok((result, placeholders))
    }

    fn extract_expressions_recursive(
        &self,
        source: &str,
        node: &Node,
        result: &mut String,
        placeholders: &mut Vec<Placeholder>,
    ) -> Result<(), Box<dyn std::error::Error>> {
        let mut cursor = node.walk();

        for child in node.children(&mut cursor) {
            match child.kind() {
                "content" => {
                    let text = child.utf8_text(source.as_bytes())?;
                    result.push_str(text);
                }
                "control_tag" => {
                    let tag_text = child.utf8_text(source.as_bytes())?;
                    let placeholder = format!("__CTRL_{}__", placeholders.len());
                    placeholders.push(Placeholder::Control(self.trim_trim(tag_text)));
                    result.push_str(&placeholder);
                }
                "render_expression" => {
                    let expr_text = child.utf8_text(source.as_bytes())?;
                    let placeholder = format!("__EXPR_{}__", placeholders.len());
                    placeholders.push(Placeholder::Expression(self.trim_trim(expr_text)));
                    result.push_str(&placeholder);
                }
                "comment" => {
                    let comment_text = child.utf8_text(source.as_bytes())?;
                    let placeholder = format!("__COMMENT_{}__", placeholders.len());
                    placeholders.push(Placeholder::Comment(self.trim_trim(comment_text)));
                    result.push_str(&placeholder);
                }
                _ => {
                    // For other nodes, recursively process children
                    self.extract_expressions_recursive(source, &child, result, placeholders)?;
                }
            }
        }

        Ok(())
    }

    pub fn trim_trim(&self, expr: &str) -> String {
        // For now, just return the expression as-is, no formatting
        expr.to_string()
    }

    fn format_html(&mut self, html: &str) -> Result<String, Box<dyn std::error::Error>> {
        // Parse HTML
        let html_tree = self
            .html_parser
            .parse(html, None)
            .ok_or("Failed to parse HTML")?;
        let root = html_tree.root_node();

        if root.has_error() {
            // Fallback if HTML is invalid
            return Ok(self.basic_indent(html));
        }

        // Format HTML with proper indentation, starting from the root
        let mut formatted = self.format_html_node(&root, html.as_bytes(), 0)?;

        // Trim trailing newlines for clean output
        while formatted.ends_with('\n') || formatted.ends_with('\r') {
            formatted.pop();
        }

        // Convert __ii__ markers to actual indentation
        formatted = self.convert_markers_to_spaces(&formatted);

        Ok(formatted)
    }

    fn format_html_node(
        &self,
        node: &Node,
        source: &[u8],
        indent_level: usize,
    ) -> Result<String, Box<dyn std::error::Error>> {
        let mut result = String::new();
        let indent = "__ii__".repeat(indent_level);

        if !node.is_named() {
            return Ok(String::new());
        }

        match node.kind() {
            "document" => {
                // For document, process children without adding indentation to the document itself
                let mut cursor = node.walk();
                for child in node.children(&mut cursor) {
                    // Document children start at indent level 0
                    result.push_str(&self.format_html_node(&child, source, 0)?);
                }
            }
            "doctype" => {
                // DOCTYPE should be at the current indent level
                result.push_str(&format!("{}{}\n", indent, node.utf8_text(source)?));
            }
            "element" => {
                // Iterate through children to find start_tag and end_tag
                let mut cursor = node.walk();
                let children: Vec<Node> = node.children(&mut cursor).collect();

                // Find start_tag (should be first)
                let start_tag = children.iter().find(|child| child.kind() == "start_tag");
                // Find end_tag (should be last)
                let end_tag = children.iter().find(|child| child.kind() == "end_tag");

                if let Some(start) = start_tag {
                    // Check if this element should be formatted inline
                    let content_children: Vec<&Node> = children
                        .iter()
                        .filter(|child| child.kind() != "start_tag" && child.kind() != "end_tag")
                        .collect();

                    let should_be_inline = content_children.len() == 1
                        && content_children[0].kind() == "text"
                        && !content_children[0]
                            .utf8_text(source)
                            .unwrap_or("")
                            .trim()
                            .contains('\n')
                        && content_children[0]
                            .utf8_text(source)
                            .unwrap_or("")
                            .trim()
                            .len()
                            < self.max_inline_length;

                    if should_be_inline {
                        // Inline element: put everything on one line
                        let text_content =
                            content_children[0].utf8_text(source).unwrap_or("").trim();

                        if let Some(end) = end_tag {
                            result.push_str(&format!(
                                "{}{}{}{}\n",
                                indent,
                                start.utf8_text(source)?,
                                text_content,
                                end.utf8_text(source)?
                            ));
                        } else {
                            result.push_str(&format!(
                                "{}{}{}\n",
                                indent,
                                start.utf8_text(source)?,
                                text_content
                            ));
                        }
                    } else {
                        // Block element: multi-line format
                        result.push_str(&format!("{}{}\n", indent, start.utf8_text(source)?));

                        // Process all children that are not start_tag or end_tag with increased indentation
                        for child in &children {
                            if child.kind() != "start_tag" && child.kind() != "end_tag" {
                                result.push_str(&self.format_html_node(
                                    child,
                                    source,
                                    indent_level + 1, // Increment indentation for children
                                )?);
                            }
                        }

                        // Add the end tag with current indentation if it exists
                        if let Some(end) = end_tag {
                            result.push_str(&format!("{}{}\n", indent, end.utf8_text(source)?));
                        }
                    }
                } else {
                    // Fallback for self-closing or malformed elements
                    result.push_str(&format!("{}{}\n", indent, node.utf8_text(source)?));
                }
            }
            "text" => {
                let text_content = node.utf8_text(source)?;
                let trimmed_text = text_content.trim();
                if !trimmed_text.is_empty() {
                    // Check if this text contains placeholders (Askama expressions)
                    if self.contains_placeholders(trimmed_text) {
                        // Split by lines and indent each line properly
                        for line in trimmed_text.lines() {
                            let line_trimmed = line.trim();
                            if !line_trimmed.is_empty() {
                                result.push_str(&format!("{}{}\n", indent, line_trimmed));
                            }
                        }
                    } else {
                        result.push_str(&format!("{}{}\n", indent, trimmed_text));
                    }
                }
                // Skip empty text nodes
            }
            _ => {
                // For any other node types, include them with indentation
                result.push_str(&format!("{}{}\n", indent, node.utf8_text(source)?));
            }
        }

        Ok(result)
    }

    fn contains_placeholders(&self, text: &str) -> bool {
        text.contains("__EXPR_") || text.contains("__CTRL_") || text.contains("__COMMENT_")
    }

    fn convert_markers_to_spaces(&self, text: &str) -> String {
        text.lines()
            .map(|line| {
                let mut indent_count = 0;
                let mut remaining = line;

                // Count __ii__ prefixes
                while let Some(stripped) = remaining.strip_prefix("__ii__") {
                    indent_count += 1;
                    remaining = stripped;
                }

                // Return with proper indentation
                if remaining.trim().is_empty() {
                    String::new() // Empty line
                } else {
                    format!(
                        "{}{}",
                        " ".repeat(indent_count * self.indent_size),
                        remaining
                    )
                }
            })
            .collect::<Vec<_>>()
            .join("\n")
    }

    fn restore_expressions(&self, formatted_html: &str, placeholders: &[Placeholder]) -> String {
        let mut result = formatted_html.to_string();

        for (i, placeholder) in placeholders.iter().enumerate() {
            let placeholder_marker = match placeholder {
                Placeholder::Expression(_) => format!("__EXPR_{}__", i),
                Placeholder::Control(_) => format!("__CTRL_{}__", i),
                Placeholder::Comment(_) => format!("__COMMENT_{}__", i),
            };

            if result.contains(&placeholder_marker) {
                result = result.replace(&placeholder_marker, &placeholder.to_string());
            }
        }

        result
    }

    fn basic_indent(&self, html: &str) -> String {
        let indented = html
            .lines()
            .map(|line| {
                if line.trim().is_empty() {
                    line.to_string()
                } else {
                    format!("__ii__{}", line.trim())
                }
            })
            .collect::<Vec<_>>()
            .join("\n");

        self.convert_markers_to_spaces(&indented)
    }
}
