use tree_sitter::Parser;
use tree_sitter_askama::LANGUAGE as ASKAMA_LANGUAGE;
use tree_sitter_html::LANGUAGE as HTML_LANGUAGE;

use crate::{
    askama,
    config::Config,
    diagnostics::{Noted, element_across_control, syntax_error},
    html,
    sakura_tree::SakuraTree,
};

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

    pub fn write(&mut self, source: &str, filepath: &str) -> Noted<String> {
        let mut errors = Vec::new();
        let mut warnings = Vec::new();

        let ast_tree = self
            .askama_parser
            .parse(source, None)
            .expect("failed to parse Askama");

        if ast_tree.root_node().has_error() {
            if let Some(error) = syntax_error(&ast_tree.root_node(), "Askama", source, filepath) {
                errors.push(error);
            }
            return Noted::err(errors, warnings);
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

        if html_tree.root_node().has_error()
            && let Some(error) = syntax_error(&html_tree.root_node(), "HTML", source, filepath)
        {
            errors.push(error);
        }

        if !errors.is_empty() {
            return Noted::err(errors, warnings);
        }

        let noted_html = html::extract_html_nodes(
            &html_tree.root_node(),
            source.as_bytes(),
            &content_node_ranges,
            filepath,
        );

        errors.extend(noted_html.errors);
        warnings.extend(noted_html.warnings);

        if !errors.is_empty() {
            return Noted::err(errors, warnings);
        }

        let mut html_nodes = noted_html
            .value
            .expect("html nodes should be present without errors");

        let (crossing_indices, crossing_warnings) =
            element_across_control(&html_nodes, &askama_nodes, source, filepath);
        warnings.extend(crossing_warnings);
        html::unpair_crossing_tags(&mut html_nodes, &crossing_indices);

        let sakura_tree = SakuraTree::grow(&askama_nodes, &html_nodes, source, &self.config);
        let output = sakura_tree.print();

        Noted::ok(output, warnings)
    }

    pub fn annotate(&mut self, source: String, filepath: &str) -> NotedFile {
        let result = self.write(&source, filepath);

        NotedFile { source, result }
    }
}

pub struct NotedFile {
    source: String,
    pub result: Noted<String>,
}

impl NotedFile {
    pub fn needs_formatting(&self) -> bool {
        self.result
            .value
            .as_ref()
            .is_some_and(|output| &self.source != output)
    }

    pub fn formatted_output(&self) -> Option<String> {
        self.result.value.as_ref().map(|output| {
            let mut out = output.clone();
            if !out.is_empty() && !out.ends_with('\n') {
                out.push('\n');
            }
            out
        })
    }
}
