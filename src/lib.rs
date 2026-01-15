#![allow(clippy::cast_possible_truncation)]
#![allow(clippy::uninlined_format_args)]
#![allow(clippy::must_use_candidate)]
#![allow(unused_assignments)]

mod askama;
mod check;
pub mod cli;
mod config;
mod css;
mod html;
mod parse;
mod sakura_tree;
pub mod session;

use miette::{Diagnostic, NamedSource, SourceSpan};
use std::io;
use thiserror::Error;
use tree_sitter::{Node, Range};

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
    Ignore(#[from] ignore::Error),

    #[error("no matches for pattern `{pattern}`")]
    NoMatches { pattern: String },

    #[error("--stdin-filepath requires stdin input (use `-` as the path)")]
    StdinFilepathWithoutStdin,

    #[error("parser initialization failed")]
    ParserFailed { lang: String },

    #[error(transparent)]
    #[diagnostic(transparent)]
    SyntaxError(#[from] Box<BoxedSyntaxError>),

    #[error("unbalanced HTML across control blocks")]
    #[diagnostic(severity(Warning))]
    UnbalancedHtml {
        #[source_code]
        src: NamedSource<String>,
        #[label]
        span: SourceSpan,
    },

    #[error(transparent)]
    #[diagnostic(transparent)]
    UnexpectedClosingTag(#[from] Box<BoxedUnexpectedClosingTag>),

    #[error("nesting too deep")]
    NestingTooDeep,
}

#[derive(Error, Diagnostic, Debug)]
#[error("syntax error in {lang} code")]
pub struct BoxedSyntaxError {
    lang: String,
    #[source_code]
    src: NamedSource<String>,
    #[label("{message}")]
    span: SourceSpan,
    message: String,
}

#[derive(Error, Diagnostic, Debug)]
#[error("unexpected closing tag")]
pub struct BoxedUnexpectedClosingTag {
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
}

pub fn normalize_ws(text: &str) -> String {
    text.split_whitespace().collect::<Vec<_>>().join(" ")
}

pub fn range_to_span(range: &Range) -> SourceSpan {
    SourceSpan::new(range.start_byte.into(), range.end_byte - range.start_byte)
}

pub fn extract_from_ranges(node: &Node, source: &[u8], ranges: &[Range]) -> String {
    let node_start = node.start_byte();
    let node_end = node.end_byte();

    let mut result = Vec::new();
    for range in ranges {
        let range_start = range.start_byte;
        let range_end = range.end_byte;

        if range_start < node_end && range_end > node_start {
            let start = range_start.max(node_start);
            let end = range_end.min(node_end);
            let slice = std::str::from_utf8(&source[start..end]).expect("valid UTF-8");
            result.push(slice);
        }
    }

    result.join("")
}

pub fn format_with_embedded(
    range: &std::ops::Range<usize>,
    source: &str,
    askama_nodes: &[askama::AskamaNode],
    embed: &[usize],
    transform: fn(&str) -> String,
) -> String {
    let mut result = String::new();
    let mut pos = range.start;

    for &idx in embed {
        let node = &askama_nodes[idx];
        if node.start() > pos {
            result.push_str(&transform(&source[pos..node.start()]));
        }
        result.push_str(&askama::format_askama_node(node));
        pos = node.end();
    }

    if pos < range.end {
        result.push_str(&transform(&source[pos..range.end]));
    }

    result
}
