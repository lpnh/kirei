use tree_sitter::Parser;
use tree_sitter_askama::LANGUAGE as ASKAMA_LANGUAGE;
use tree_sitter_html::LANGUAGE as HTML_LANGUAGE;

use crate::{
    config::Config,
    extraction::extract_nodes,
    layout_engine::{LayoutEngine, tokenize},
    types::AskamaNode,
};

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
    pub fn new() -> Result<Self, Box<dyn std::error::Error>> {
        let mut askama_parser = Parser::new();
        askama_parser.set_language(&ASKAMA_LANGUAGE.into())?;

        let mut html_parser = Parser::new();
        html_parser.set_language(&HTML_LANGUAGE.into())?;

        Ok(Self {
            askama_parser,
            html_parser,
            config: Config::default(),
        })
    }

    pub fn format(&mut self, source: &str) -> Result<String, Box<dyn std::error::Error>> {
        // 1. Use tree-sitter-askama to parse the input
        let ast_tree = self
            .askama_parser
            .parse(source, None)
            .ok_or("Parse failed")?;

        // 2. Transmute the original AST into a single String + AskamaNode vector
        // html -> Raw input with tokens (placeholders) replacing the Askama syntax
        // nodes -> All nodes containing the Askama syntax that have been replaced
        let (html, nodes) = extract_nodes(source, &ast_tree.root_node())?;

        // 3. Avoid giving the HTML parser a string with no HTML at all
        let formatted = if self.is_template_only(&html, &nodes) {
            self.format_template_only(&html, &nodes)
        } else {
            self.format_with_html(&html, &nodes)?
        };

        // 4. Restore the original Askama code, but now formatted
        Ok(LayoutEngine::restore_placeholders(
            &formatted,
            &nodes,
            &self.config,
        ))
    }

    fn is_template_only(&self, html: &str, nodes: &[AskamaNode]) -> bool {
        let mut stripped = html.to_string();
        for (i, node) in nodes.iter().enumerate() {
            stripped = stripped.replace(&node.placeholder(i), "");
        }
        stripped.trim().is_empty()
    }

    fn format_template_only(&self, html: &str, nodes: &[AskamaNode]) -> String {
        let mut engine = LayoutEngine::new(nodes, &self.config);
        let tokens = tokenize(html);
        engine.process_tokens(&tokens);
        engine.finish()
    }

    fn format_with_html(
        &mut self,
        html: &str,
        nodes: &[AskamaNode],
    ) -> Result<String, Box<dyn std::error::Error>> {
        let tree = self
            .html_parser
            .parse(html, None)
            .ok_or("HTML parse failed")?;

        let mut engine = LayoutEngine::new(nodes, &self.config);
        engine.process_html(&tree.root_node(), html.as_bytes())?;
        Ok(engine.finish())
    }
}
