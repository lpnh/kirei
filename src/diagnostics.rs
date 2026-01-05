#![allow(unused)]

use miette::{Diagnostic, GraphicalReportHandler, GraphicalTheme, NamedSource, SourceSpan};
use std::{io, ops};
use thiserror::Error;
use tree_sitter::{Node, Point, Range};

use crate::{
    askama::{AskamaNode, ControlTag},
    html::HtmlNode,
};

#[derive(Error, Diagnostic, Debug)]
pub enum KireiError {
    #[error("failed to read `{path}`")]
    #[diagnostic(code(kirei::io::read_failed))]
    ReadFailed {
        path: String,
        #[source]
        source: io::Error,
    },

    #[error("failed to write `{path}`")]
    #[diagnostic(code(kirei::io::write_failed))]
    WriteFailed {
        path: String,
        #[source]
        source: io::Error,
    },

    #[error("failed to read from stdin")]
    #[diagnostic(code(kirei::io::stdin_failed))]
    StdinFailed {
        #[source]
        source: io::Error,
    },

    #[error("path `{path}` does not exist")]
    #[diagnostic(code(kirei::path::not_found))]
    PathNotFound { path: String },

    #[error("`{path}` is not a regular file")]
    #[diagnostic(code(kirei::path::not_a_file))]
    NotAFile { path: String },

    #[error("no `.html` files in directory `{path}`")]
    #[diagnostic(code(kirei::path::no_html_files))]
    NoHtmlFiles { path: String },

    #[error("invalid glob pattern `{pattern}`")]
    #[diagnostic(code(kirei::glob::invalid_pattern))]
    InvalidGlobPattern {
        pattern: String,
        #[source]
        source: globset::Error,
    },

    #[error("no matches for pattern `{pattern}`")]
    #[diagnostic(code(kirei::glob::no_matches))]
    NoMatches { pattern: String },

    #[error("--stdin-filepath requires stdin input (use `-` as the path)")]
    #[diagnostic(code(kirei::cli::stdin_filepath_without_stdin))]
    StdinFilepathWithoutStdin,

    #[error("failed to parse {kind}")]
    #[diagnostic(code(kirei::format::syntax_error))]
    SyntaxError {
        kind: String,
        #[source_code]
        src: NamedSource<String>,
        #[label("{message}")]
        span: SourceSpan,
        message: String,
    },

    #[error("unexpected closing tag")]
    #[diagnostic(code(kirei::format::unexpected_closing_tag))]
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
    #[diagnostic(code(kirei::format::nesting_too_deep))]
    NestingTooDeep,
}

#[derive(Error, Diagnostic, Debug, Clone)]
pub enum KireiWarning {
    #[error("unbalanced HTML across control blocks")]
    #[diagnostic(code(kirei::unbalanced_html), severity(Warning))]
    UnbalancedHtml {
        #[source_code]
        src: NamedSource<String>,
        #[label]
        span: SourceSpan,
    },
}

pub struct Noted<T> {
    pub value: Option<T>,
    pub errors: Vec<KireiError>,
    pub warnings: Vec<KireiWarning>,
}

impl<T> Noted<T> {
    pub fn ok(value: T, warnings: Vec<KireiWarning>) -> Self {
        Self {
            value: Some(value),
            errors: Vec::new(),
            warnings,
        }
    }

    pub fn err(errors: Vec<KireiError>, warnings: Vec<KireiWarning>) -> Self {
        Self {
            value: None,
            errors,
            warnings,
        }
    }

    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }
}

fn find_error_node<'a>(node: &Node<'a>) -> Option<Node<'a>> {
    if node.is_error() || node.is_missing() {
        return Some(*node);
    }
    for child in node.children(&mut node.walk()) {
        if let Some(err) = find_error_node(&child) {
            return Some(err);
        }
    }
    None
}

fn refine_error_range(error_node: &Node) -> Range {
    let mut range = error_node.range();
    let children: Vec<_> = error_node.children(&mut error_node.walk()).collect();
    let non_error_children: Vec<_> = children.iter().filter(|c| !c.is_error()).collect();

    if non_error_children.is_empty() {
        if range.start_point.row != range.end_point.row {
            range.end_point = range.start_point;
            range.end_point.column = range.start_point.column + 1;
            range.end_byte = range.start_byte + 1;
        }
        return range;
    }

    let opening_pos = non_error_children
        .iter()
        .position(|c| matches!(c.kind(), "{{" | "{%" | "{#"));

    if let Some(open_idx) = opening_pos {
        let closing_kind = match non_error_children[open_idx].kind() {
            "{{" => "}}",
            "{%" => "%}",
            "{#" => "#}",
            _ => "",
        };

        if let Some(close_idx) = non_error_children[open_idx..]
            .iter()
            .position(|c| c.kind() == closing_kind)
        {
            let first = non_error_children[open_idx];
            let last = non_error_children[open_idx + close_idx];
            return range_between(first, last);
        }
    }

    let first = non_error_children.first().unwrap();
    let last = non_error_children.last().unwrap();
    range_between(first, last)
}

fn range_between(first: &Node, last: &Node) -> Range {
    Range {
        start_byte: first.range().start_byte,
        start_point: first.range().start_point,
        end_byte: last.range().end_byte,
        end_point: last.range().end_point,
    }
}

pub fn element_across_control(
    html_nodes: &[HtmlNode],
    askama_nodes: &[AskamaNode],
    source: &str,
    filepath: &str,
) -> (Vec<(usize, usize)>, Vec<KireiWarning>) {
    let mut warnings = Vec::new();
    let mut crossing_indices = Vec::new();

    for i in 0..html_nodes.len() {
        let Some((start_byte, end_byte, end_idx, name)) = (match &html_nodes[i] {
            HtmlNode::Start {
                range,
                end: Some(end_idx),
                name,
                ..
            } => html_nodes.get(*end_idx).and_then(|n| match n {
                HtmlNode::End { start, .. } => Some((range.start, *start, *end_idx, name.clone())),
                _ => None,
            }),
            _ => None,
        }) else {
            continue;
        };

        if !has_crossing_boundary(start_byte, end_byte, askama_nodes) {
            continue;
        }

        let close_tag_end = end_byte + 2 + name.len() + 1;
        let span_range = range_from_bytes(source, start_byte, close_tag_end);

        warnings.push(KireiWarning::UnbalancedHtml {
            src: NamedSource::new(filepath, source.to_string()),
            span: range_to_span(&span_range),
        });

        crossing_indices.push((i, end_idx));
    }

    (crossing_indices, warnings)
}

fn has_crossing_boundary(start: usize, end: usize, askama_nodes: &[AskamaNode]) -> bool {
    if start == end {
        return false;
    }

    let (min_pos, max_pos) = (start.min(end), start.max(end));

    for node in askama_nodes {
        if let AskamaNode::Control {
            range,
            end: close_pos,
            tag,
            ..
        } = node
        {
            if matches!(
                tag,
                ControlTag::Else | ControlTag::ElseIf | ControlTag::When | ControlTag::MatchElse
            ) && range.start > min_pos
                && range.start < max_pos
                && let Some((parent_start, parent_end)) = find_closest_parent(range, askama_nodes)
            {
                let min_inside = min_pos >= parent_start && min_pos < parent_end;
                let max_inside = max_pos >= parent_start && max_pos < parent_end;

                if min_inside && max_inside {
                    return true;
                }
            }

            if let Some(close) = close_pos {
                let start_inside = min_pos >= range.end && min_pos < *close;
                let end_inside = max_pos >= range.end && max_pos < *close;

                if start_inside != end_inside {
                    return true;
                }
            }
        }
    }

    false
}

fn find_closest_parent(
    range: &ops::Range<usize>,
    askama_nodes: &[AskamaNode],
) -> Option<(usize, usize)> {
    let mut closest_parent = None;
    let mut smallest_size = usize::MAX;

    for parent in askama_nodes {
        if let AskamaNode::Control {
            range: parent_range,
            end: Some(parent_end),
            tag: parent_tag,
            ..
        } = parent
            && parent_tag.is_opening()
            && parent_range.end <= range.start
            && *parent_end >= range.end
        {
            let block_size = *parent_end - parent_range.end;
            if block_size < smallest_size {
                closest_parent = Some((parent_range.end, *parent_end));
                smallest_size = block_size;
            }
        }
    }

    closest_parent
}

fn range_from_bytes(source: &str, start: usize, end: usize) -> Range {
    Range {
        start_byte: start,
        end_byte: end,
        start_point: byte_to_point(source, start),
        end_point: byte_to_point(source, end),
    }
}

fn byte_to_point(source: &str, byte_pos: usize) -> Point {
    let (mut row, mut col) = (0, 0);
    for (i, c) in source.char_indices() {
        if i >= byte_pos {
            break;
        }
        if c == '\n' {
            row += 1;
            col = 0;
        } else {
            col += 1;
        }
    }
    Point { row, column: col }
}

pub fn erroneous_end_tag(
    expected: String,
    found: &str,
    open_range: Range,
    close_range: Range,
    source: &str,
    filepath: &str,
) -> KireiError {
    let suggestion = if !expected.is_empty() {
        Some(format!("consider using `{}`", expected))
    } else {
        None
    };

    KireiError::UnexpectedClosingTag {
        expected,
        found: found.to_string(),
        src: NamedSource::new(filepath, source.to_string()),
        close_span: range_to_span(&close_range),
        open_span: range_to_span(&open_range),
        suggestion,
    }
}

pub fn syntax_error(
    root_node: &Node,
    kind: &str,
    source: &str,
    filepath: &str,
) -> Option<KireiError> {
    find_error_node(root_node).map(|node| {
        if node.is_missing() {
            let node_kind = node.kind();
            let parent = node.parent();

            let (kind, message) = if let ("identifier", Some("block_statement")) =
                (node_kind, parent.map(|p| p.kind()))
            {
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

            return KireiError::SyntaxError {
                kind,
                src: NamedSource::new(filepath, source.to_string()),
                span: range_to_span(&node.range()),
                message,
            };
        }

        let range = refine_error_range(&node);
        let span = range_to_span(&range);

        KireiError::SyntaxError {
            kind: kind.to_string(),
            src: NamedSource::new(filepath, source.to_string()),
            span,
            message: String::new(),
        }
    })
}

fn range_to_span(range: &Range) -> SourceSpan {
    SourceSpan::new(range.start_byte.into(), range.end_byte - range.start_byte)
}
