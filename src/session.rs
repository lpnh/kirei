use miette::{Diagnostic, GraphicalReportHandler};
use std::{fs, path::PathBuf};
use tree_sitter::Parser;
use tree_sitter_askama::LANGUAGE as ASKAMA_LANGUAGE;
use tree_sitter_html::LANGUAGE as HTML_LANGUAGE;

use crate::{
    KireiError, askama,
    config::Config,
    diagnostics::{Noted, element_across_control, syntax_error},
    html,
    sakura_tree::SakuraTree,
};

pub struct Session {
    askama_parser: Parser,
    html_parser: Parser,
    config: Config,
    results: Vec<NotedFile>,
}

struct NotedFile {
    path: PathBuf,
    source: String,
    result: Noted<String>,
}

impl NotedFile {
    fn requires_formatting(&self) -> bool {
        self.result
            .value
            .as_ref()
            .is_some_and(|output| &self.source != output)
    }
}

pub enum SessionMode {
    Write,
    Check,
    ListDifferent,
    Stdout,
}

impl Default for Session {
    fn default() -> Self {
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
            results: Vec::new(),
        }
    }
}

impl Session {
    pub fn format(&mut self, source: &str, filepath: &str) -> Noted<String> {
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

    pub fn format_and_print(
        &mut self,
        mode: &SessionMode,
        path: impl Into<PathBuf>,
        source: String,
    ) -> Result<(), KireiError> {
        let path = path.into();
        let noted = {
            let result = self.format(&source, &path.to_string_lossy());
            NotedFile {
                path,
                source,
                result,
            }
        };

        if let Some(formatted) = noted.result.value.as_ref()
            && noted.requires_formatting()
        {
            match mode {
                SessionMode::Check => {
                    eprintln!("{}: file would be formatted", noted.path.display());
                }
                SessionMode::ListDifferent => {
                    eprintln!("{}", noted.path.display());
                }
                SessionMode::Write => {
                    fs::write(&noted.path, formatted).map_err(|source| {
                        KireiError::WriteFailed {
                            path: noted.path.display().to_string(),
                            source,
                        }
                    })?;
                }
                SessionMode::Stdout => {
                    print!("{}", formatted);
                }
            }
        }

        self.results.push(noted);
        Ok(())
    }

    pub fn add_error(&mut self, path: PathBuf, error: KireiError) {
        self.results.push(NotedFile {
            path,
            source: String::new(),
            result: Noted::err(vec![error], vec![]),
        });
    }

    pub fn print_diagnostics(&self, use_color: bool) {
        for noted in &self.results {
            eprint!("{}", Self::diagnostics_from_noted(&noted.result, use_color));
        }
    }

    pub fn has_errors(&self) -> bool {
        self.results.iter().any(|noted| noted.result.has_errors())
    }

    pub fn requires_formatting(&self) -> bool {
        self.results.iter().any(NotedFile::requires_formatting)
    }

    pub fn diagnostics_from_noted<T>(result: &Noted<T>, use_color: bool) -> String {
        let mut output = String::new();
        for error in &result.errors {
            output.push_str(&Self::render_diagnostic(error, use_color));
        }
        for warning in &result.warnings {
            output.push_str(&Self::render_diagnostic(warning, use_color));
        }
        output
    }

    pub fn render_diagnostic(diagnostic: &dyn Diagnostic, use_color: bool) -> String {
        let handler = GraphicalReportHandler::new_themed(if use_color {
            miette::GraphicalTheme::unicode()
        } else {
            miette::GraphicalTheme::none()
        });
        let mut output = String::new();
        handler
            .render_report(&mut output, diagnostic)
            .expect("failed to render diagnostic");
        output
    }
}
