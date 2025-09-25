use tree_sitter::Parser;
use tree_sitter_askama::LANGUAGE as ASKAMA_LANGUAGE;
use tree_sitter_html::LANGUAGE as HTML_LANGUAGE;

use crate::{
    config::Config,
    extraction::extract_nodes,
    layout_engine::LayoutEngine,
    types::{AskamaNode, tokenize},
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

        let mut engine = LayoutEngine::new(&nodes, &self.config);

        // 3. Avoid giving the HTML parser a string with no HTML at all
        if self.is_template_only(&html, &nodes) {
            let tokens = tokenize(&html, &nodes);
            engine.process_tokens(&tokens);
        } else {
            let tree = self
                .html_parser
                .parse(&html, None)
                .ok_or("HTML parse failed")?;
            engine.process_html(&tree.root_node(), html.as_bytes())?;
        }

        // 4. Return the formatted output
        Ok(engine.restore_placeholders())
    }

    fn is_template_only(&self, html: &str, nodes: &[AskamaNode]) -> bool {
        let mut stripped = html.to_string();
        for (i, node) in nodes.iter().enumerate() {
            stripped = stripped.replace(&node.placeholder(i), "");
        }
        stripped.trim().is_empty()
    }
}
