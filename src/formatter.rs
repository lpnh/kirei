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

        // 2. Check for any syntax error
        if ast_tree.root_node().has_error() {
            // Return the source as is
            return Ok(source.to_string());
        }

        // 3. Extract Askama nodes and content node ranges
        let (askama_nodes, content_node_ranges) =
            askama::extract_askama_nodes(&ast_tree.root_node(), source)
                .context("Failed to extract Askama nodes")?;

        // 4. Use tree-sitter-html to parse the included ranges
        if !content_node_ranges.is_empty() {
            self.html_parser
                .set_included_ranges(&content_node_ranges)
                .context("Failed to set included ranges")?;
        }
        let html_tree = self
            .html_parser
            .parse(source, None)
            .context("Failed to parse HTML")?;

        // 5. Extract Html nodes
        let html_nodes = html::extract_html_nodes(
            &html_tree.root_node(),
            source.as_bytes(),
            &content_node_ranges,
        )
        .context("Failed to extract HTML nodes")?;

        // 6. Grow a SakuraTree
        let mut sakura_tree = SakuraTree::grow(&askama_nodes, &html_nodes, source, &self.config);

        // 7. Wire
        wire::wire(&mut sakura_tree);

        // 8. Print
        Ok(woodcut::print(&sakura_tree))
    }
}
