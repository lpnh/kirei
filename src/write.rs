use tree_sitter::{Node, Parser};
use tree_sitter_askama::LANGUAGE as ASKAMA_LANGUAGE;
use tree_sitter_html::LANGUAGE as HTML_LANGUAGE;

use crate::{
    askama,
    config::Config,
    error::{KireiError, OrMsg},
    html,
    sakura_tree::SakuraTree,
};

fn find_error_node<'a>(node: &Node<'a>) -> Option<Node<'a>> {
    if node.is_error() {
        return Some(*node);
    }
    for child in node.children(&mut node.walk()) {
        if let Some(err) = find_error_node(&child) {
            return Some(err);
        }
    }
    None
}

pub struct Kirei {
    askama_parser: Parser,
    html_parser: Parser,
    config: Config,
}

impl Default for Kirei {
    fn default() -> Self {
        Self::new().expect("failed to initialize kirei")
    }
}

impl Kirei {
    pub fn new() -> Result<Self, KireiError> {
        let mut askama_parser = Parser::new();
        askama_parser
            .set_language(&ASKAMA_LANGUAGE.into())
            .or_msg("failed to set Askama language")?;

        let mut html_parser = Parser::new();
        html_parser
            .set_language(&HTML_LANGUAGE.into())
            .or_msg("failed to set HTML language")?;

        Ok(Self {
            askama_parser,
            html_parser,
            config: Config::default(),
        })
    }

    pub fn write(&mut self, source: &str) -> Result<String, KireiError> {
        let ast_tree = self
            .askama_parser
            .parse(source, None)
            .or_msg("failed to parse Askama")?;

        if ast_tree.root_node().has_error()
            && let Some(error_node) = find_error_node(&ast_tree.root_node())
        {
            return Err(KireiError::AskamaSyntaxError {
                error_range: error_node.range(),
            });
        }

        let (askama_nodes, content_node_ranges) =
            askama::extract_askama_nodes(&ast_tree.root_node(), source)
                .or_msg("failed to extract Askama nodes")?;

        if !content_node_ranges.is_empty() {
            self.html_parser
                .set_included_ranges(&content_node_ranges)
                .or_msg("failed to set included ranges")?;
        }
        let html_tree = self
            .html_parser
            .parse(source, None)
            .or_msg("failed to parse HTML")?;

        if html_tree.root_node().has_error()
            && let Some(error_node) = find_error_node(&html_tree.root_node())
        {
            return Err(KireiError::HtmlSyntaxError {
                error_range: error_node.range(),
            });
        }

        let html_nodes = html::extract_html_nodes(
            &html_tree.root_node(),
            source.as_bytes(),
            &content_node_ranges,
            &askama_nodes,
        )?;

        let sakura_tree = SakuraTree::grow(&askama_nodes, &html_nodes, source, &self.config);

        Ok(sakura_tree.print())
    }
}
