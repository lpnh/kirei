use tree_sitter::{Node, Range};

use crate::draw::{Annotation, Diagnostic};

pub fn io_error(error: &std::io::Error, filepath: Option<&str>) -> ! {
    use std::io::ErrorKind as IoKind;

    let message = match (error.kind(), filepath) {
        (IoKind::NotFound, Some(path)) => format!("file `{}` does not exist", path),
        (IoKind::NotFound, None) => "file does not exist".to_string(),
        (IoKind::PermissionDenied, Some(path)) => {
            format!("permission denied when accessing `{}`", path)
        }
        (IoKind::PermissionDenied, None) => "permission denied".to_string(),
        (IoKind::AlreadyExists, Some(path)) => format!("file `{}` already exists", path),
        (IoKind::AlreadyExists, None) => "file already exists".to_string(),
        (IoKind::InvalidInput, _) => "invalid input".to_string(),
        (IoKind::InvalidData, _) => "invalid data".to_string(),
        (IoKind::TimedOut, _) => "operation timed out".to_string(),
        (IoKind::Interrupted, _) => "operation interrupted".to_string(),
        (IoKind::UnexpectedEof, Some(path)) => {
            format!("unexpected end of file in `{}`", path)
        }
        (IoKind::UnexpectedEof, None) => "unexpected end of file".to_string(),
        (IoKind::BrokenPipe, _) => "broken pipe".to_string(),
        (kind, Some(path)) => format!("IO error when accessing `{}`: {:?}", path, kind),
        (kind, None) => format!("IO error: {:?}", kind),
    };

    let diagnostic = Diagnostic::error(message);
    eprintln!("{}", diagnostic.draw("", filepath));
    std::process::exit(1);
}

pub fn file_would_be_updated(filepath: Option<&str>) -> ! {
    let diagnostic = Diagnostic::error("file would be formatted");
    eprintln!("{}", diagnostic.draw("", filepath));
    std::process::exit(1);
}

pub fn erroneous_end_tag(
    expected: String,
    found: &str,
    open_range: Range,
    close_range: Range,
    source: &str,
) -> Diagnostic {
    let line_index = close_range.start_point.row;
    let start_col = close_range.start_point.column;
    let end_col = close_range.end_point.column;

    let mut diag = Diagnostic::error("unexpected closing tag")
        .with_label(
            close_range,
            format!("expected `{}`, found `{}`", expected, found),
            Annotation::Primary,
        )
        .with_label(
            open_range,
            "expected due to this open tag name",
            Annotation::Secondary,
        )
        .with_help(format!("consider using `{}`", expected));

    if let Some(line) = source.lines().nth(line_index) {
        diag = diag.with_suggestion(line_index, line, start_col, end_col, expected);
    }

    diag
}

pub fn nesting_too_deep() -> Diagnostic {
    Diagnostic::error("nesting too deep")
}

pub fn missing_syntax(node: &Node, _kind: &str) -> Diagnostic {
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

pub fn syntax_error(root_node: &Node, kind: &str) -> Option<Diagnostic> {
    Diagnostic::find_error_node(root_node).map(|error_node| {
        if error_node.is_missing() {
            return missing_syntax(&error_node, kind);
        }

        let range = Diagnostic::refine_error_range(&error_node);
        Diagnostic::error(format!("failed to parse {}", kind)).with_label(
            range,
            "",
            Annotation::Primary,
        )
    })
}
