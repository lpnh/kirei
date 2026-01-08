use miette::{Diagnostic, GraphicalReportHandler};
use std::{fs, path::PathBuf};

use crate::{
    ErrorKind, KireiWarning, config::Config, parse::SakuraParser, sakura_tree::SakuraTree,
};

#[derive(Default)]
pub struct Session {
    config: Config,
    pub notes: Notes,
    pub requires_formatting: bool,
}

#[derive(Default)]
pub struct Notes {
    pub errors: Vec<ErrorKind>,
    pub warnings: Vec<KireiWarning>,
}

pub enum SessionMode {
    Write,
    Check,
    ListDifferent,
    Stdout,
}

impl Session {
    pub fn format(&mut self, source: &str, filepath: &str) -> Option<String> {
        SakuraParser::default()
            .parse(&mut self.notes, source, filepath)
            .map(|seed| SakuraTree::grow(&seed, &self.config))
    }

    pub fn format_and_print(&mut self, mode: &SessionMode, path: impl Into<PathBuf>, source: &str) {
        let path = path.into();

        if let Some(result) = self.format(source, &path.to_string_lossy())
            && source != result
        {
            self.requires_formatting = true;
            match mode {
                SessionMode::Check => {
                    eprintln!("{}: file would be formatted", path.display());
                }
                SessionMode::ListDifferent => {
                    eprintln!("{}", path.display());
                }
                SessionMode::Write => {
                    if let Err(source) = fs::write(&path, result) {
                        self.notes.errors.push(ErrorKind::WriteFailed {
                            path: path.display().to_string(),
                            source,
                        });
                    }
                }
                SessionMode::Stdout => {
                    print!("{}", result);
                }
            }
        }
    }

    pub fn print_diagnostics(&self, use_color: bool) {
        for error in &self.notes.errors {
            eprint!("{}", Self::render_diagnostic(error, use_color));
        }
        for warning in &self.notes.warnings {
            eprint!("{}", Self::render_diagnostic(warning, use_color));
        }
    }

    pub fn render_diagnostic(diagnostic: &dyn Diagnostic, use_color: bool) -> String {
        let handler = GraphicalReportHandler::new_themed(if use_color {
            miette::GraphicalTheme::unicode()
        } else {
            miette::GraphicalTheme::none()
        });
        let mut output = String::new();
        handler.render_report(&mut output, diagnostic).unwrap();
        output
    }
}
