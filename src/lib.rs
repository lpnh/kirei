#![allow(clippy::uninlined_format_args)]
#![allow(clippy::must_use_candidate)]
#![allow(unused)]

mod askama;
pub mod cli;
mod config;
mod html;
mod parse;
mod sakura_tree;
pub mod session;

use miette::{Diagnostic, NamedSource, SourceSpan};
use std::io;
use thiserror::Error;
use tree_sitter::Range;

#[derive(Error, Diagnostic, Debug)]
pub enum ErrorKind {
    #[error(transparent)]
    Io(#[from] io::Error),

    #[error("failed to read `{path}`")]
    ReadFailed {
        path: String,
        #[source]
        source: io::Error,
    },

    #[error("failed to write `{path}`")]
    WriteFailed {
        path: String,
        #[source]
        source: io::Error,
    },

    #[error("path `{path}` does not exist")]
    FileNotFound { path: String },

    #[error("`{path}` is not a regular file")]
    NotAFile { path: String },

    #[error("no `.html` files in directory `{path}`")]
    NoHtmlFiles { path: String },

    #[error(transparent)]
    Glob(#[from] globset::Error),

    #[error("no matches for pattern `{pattern}`")]
    NoMatches { pattern: String },

    #[error("--stdin-filepath requires stdin input (use `-` as the path)")]
    StdinFilepathWithoutStdin,

    #[error("parser initialization failed")]
    ParserFailed { lang: String },

    #[error("syntax error in {lang} code")]
    SyntaxError {
        lang: String,
        #[source_code]
        src: NamedSource<String>,
        #[label("{message}")]
        span: SourceSpan,
        message: String,
    },

    #[error("unexpected closing tag")]
    UnexpectedClosingTag {
        expected: String,
        found: String,
        #[source_code]
        src: NamedSource<String>,
        #[label("expected `{expected}`, found `{found}`")]
        close_span: SourceSpan,
        #[label("expected due to this open tag name")]
        open_span: SourceSpan,
        #[help]
        suggestion: Option<String>,
    },

    #[error("nesting too deep")]
    NestingTooDeep,
}

#[derive(Error, Diagnostic, Debug, Clone)]
pub enum KireiWarning {
    #[error("unbalanced HTML across control blocks")]
    #[diagnostic(severity(Warning))]
    UnbalancedHtml {
        #[source_code]
        src: NamedSource<String>,
        #[label]
        span: SourceSpan,
    },
}

pub fn normalize_ws(text: &str) -> String {
    text.split_whitespace().collect::<Vec<_>>().join(" ")
}

pub fn range_to_span(range: &Range) -> SourceSpan {
    SourceSpan::new(range.start_byte.into(), range.end_byte - range.start_byte)
}
