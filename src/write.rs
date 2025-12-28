use tree_sitter::Parser;
use tree_sitter_askama::LANGUAGE as ASKAMA_LANGUAGE;
use tree_sitter_html::LANGUAGE as HTML_LANGUAGE;

use crate::{askama, check, config::Config, draw::Diagnostic, html, sakura_tree::SakuraTree};

pub struct Kirei {
    askama_parser: Parser,
    html_parser: Parser,
    config: Config,
}

impl Default for Kirei {
    fn default() -> Self {
        Self::new()
    }
}

impl Kirei {
    pub fn new() -> Self {
        let mut askama_parser = Parser::new();
        askama_parser
            .set_language(&ASKAMA_LANGUAGE.into())
            .expect("failed to set Askama language");

        let mut html_parser = Parser::new();
        html_parser
            .set_language(&HTML_LANGUAGE.into())
            .expect("failed to set HTML language");

        Self {
            askama_parser,
            html_parser,
            config: Config::default(),
        }
    }

    pub fn write(&mut self, source: &str) -> (String, Vec<Diagnostic>) {
        let mut diagnostics = Vec::new();

        let ast_tree = self
            .askama_parser
            .parse(source, None)
            .expect("failed to parse Askama");

        if ast_tree.root_node().has_error() {
            if let Some(diagnostic) = Diagnostic::syntax_error(&ast_tree.root_node(), "Askama") {
                diagnostics.push(diagnostic);
            }
            return (String::new(), diagnostics);
        }

        let (askama_nodes, content_node_ranges) =
            askama::extract_askama_nodes(&ast_tree.root_node(), source);

        if !content_node_ranges.is_empty() {
            self.html_parser
                .set_included_ranges(&content_node_ranges)
                .expect("failed to set included ranges");
        }
        let html_tree = self
            .html_parser
            .parse(source, None)
            .expect("failed to parse HTML");

        if html_tree.root_node().has_error() {
            if let Some(diagnostic) = Diagnostic::syntax_error(&html_tree.root_node(), "HTML") {
                diagnostics.push(diagnostic);
            }
            return (String::new(), diagnostics);
        }

        let mut html_nodes = html::extract_html_nodes(
            &html_tree.root_node(),
            source.as_bytes(),
            &content_node_ranges,
            &mut diagnostics,
        );

        diagnostics.extend(check::crossing_control_boundary(
            &mut html_nodes,
            &askama_nodes,
            source,
        ));

        let sakura_tree = SakuraTree::grow(&askama_nodes, &html_nodes, source, &self.config);

        (sakura_tree.print(), diagnostics)
    }
}
