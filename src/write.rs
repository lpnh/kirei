use tree_sitter::{Node, Parser};
use tree_sitter_askama::LANGUAGE as ASKAMA_LANGUAGE;
use tree_sitter_html::LANGUAGE as HTML_LANGUAGE;

use crate::{
    askama, check,
    config::Config,
    diagnostics::{Annotation, Diagnostic},
    html,
    noted::Noted,
    sakura_tree::SakuraTree,
};

fn syntax_error(root_node: &Node, kind: &str) -> Option<Diagnostic> {
    Diagnostic::find_error_node(root_node).map(|error_node| {
        if error_node.is_missing() {
            return missing_syntax(&error_node);
        }

        let range = Diagnostic::refine_error_range(&error_node);
        Diagnostic::error(format!("failed to parse {}", kind)).with_label(
            range,
            "",
            Annotation::Primary,
        )
    })
}

fn missing_syntax(node: &Node) -> Diagnostic {
    let node_kind = node.kind();
    let parent = node.parent();

    let (message, label) =
        if let ("identifier", Some("block_statement")) = (node_kind, parent.map(|p| p.kind())) {
            (
                "missing block name".to_string(),
                "block tag requires a name".to_string(),
            )
        } else {
            let parent_context = parent
                .map(|p| format!(" in {}", p.kind()))
                .unwrap_or_default();
            (
                format!("missing {}{}", node_kind, parent_context),
                format!("expected {} here", node_kind),
            )
        };
    Diagnostic::error(message).with_label(node.range(), &label, Annotation::Primary)
}

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

    pub fn write(&mut self, source: &str) -> Noted<String> {
        let mut diagnostics = Vec::new();

        let ast_tree = self
            .askama_parser
            .parse(source, None)
            .expect("failed to parse Askama");

        if ast_tree.root_node().has_error() {
            if let Some(diagnostic) = syntax_error(&ast_tree.root_node(), "Askama") {
                diagnostics.push(diagnostic);
            }
            return Noted::with_diagnostics(String::new(), diagnostics);
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
            if let Some(diagnostic) = syntax_error(&html_tree.root_node(), "HTML") {
                diagnostics.push(diagnostic);
            }
            return Noted::with_diagnostics(String::new(), diagnostics);
        }

        let noted_html = html::extract_html_nodes(
            &html_tree.root_node(),
            source.as_bytes(),
            &content_node_ranges,
        );
        let mut html_nodes = noted_html.value;
        diagnostics.extend(noted_html.diagnostics);

        let noted_pair_indices = check::element_across_control(&html_nodes, &askama_nodes, source);
        diagnostics.extend(noted_pair_indices.diagnostics);
        html::unpair_crossing_tags(&mut html_nodes, &noted_pair_indices.value);

        let sakura_tree = SakuraTree::grow(&askama_nodes, &html_nodes, source, &self.config);

        Noted::with_diagnostics(sakura_tree.print(), diagnostics)
    }
}
