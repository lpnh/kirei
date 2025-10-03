use anyhow::{Context, Result};
use tree_sitter::Parser;
use tree_sitter_askama::LANGUAGE as ASKAMA_LANGUAGE;
use tree_sitter_html::LANGUAGE as HTML_LANGUAGE;

use crate::{askama, config::Config, html, sakura_tree::SakuraTree, wire, woodcut};

pub struct AskamaFormatter {
    askama_parser: Parser,
    html_parser: Parser,
    config: Config,
}

impl Default for AskamaFormatter {
    fn default() -> Self {
        Self::new().expect("Failed to initialize kirei")
    }
}

impl AskamaFormatter {
    pub fn new() -> Result<Self> {
        let mut askama_parser = Parser::new();
        askama_parser
            .set_language(&ASKAMA_LANGUAGE.into())
            .context("Failed to set Askama language")?;

        let mut html_parser = Parser::new();
        html_parser
            .set_language(&HTML_LANGUAGE.into())
            .context("Failed to set HTML language")?;

        Ok(Self {
            askama_parser,
            html_parser,
            config: Config::default(),
        })
    }

    pub fn format(&mut self, source: &str) -> Result<String> {
        // 1. Use tree-sitter-askama to parse the input
        let ast_tree = self
            .askama_parser
            .parse(source, None)
            .context("Failed to parse Askama")?;

        // 2. Extract and replace Askama nodes with placeholder
        let (html, askama_nodes) = askama::extract_nodes(source, &ast_tree.root_node())
            .context("Failed to extract Askama nodes")?;

        // 3. Use tree-sitter-html to parse the restored input + placeholder
        let html_tree = self
            .html_parser
            .parse(&html, None)
            .context("Failed to parse HTML")?;
        let html_nodes = html::parse_html_tree(&html_tree.root_node(), html.as_bytes())
            .context("Failed to extract HTML nodes")?;

        // 4. Grow a SakuraTree
        let mut sakura_tree = SakuraTree::grow(&askama_nodes, &html_nodes, self.config.clone());

        // 5. Wire
        wire::wire(&mut sakura_tree);

        // 6. Print
        Ok(woodcut::print(&sakura_tree))
    }
}
