#![allow(clippy::uninlined_format_args)]

mod extraction;
pub mod formatter;
mod restoration;
pub mod types;

use tree_sitter::Parser;
use tree_sitter_askama::LANGUAGE as ASKAMA_LANGUAGE;
use tree_sitter_html::LANGUAGE as HTML_LANGUAGE;

use crate::extraction::extract_nodes;
use crate::formatter::{format_template_only, format_template_with_html};
use crate::restoration::restore_nodes;
use crate::types::AskamaNode;

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
    // Check if content is purely Askama template (no HTML structure)
    let mut test = html.to_string();
    for (i, _) in nodes.iter().enumerate() {
        test = test.replace(&nodes[i].placeholder(i), "");
    }
    test.trim().is_empty()
}
