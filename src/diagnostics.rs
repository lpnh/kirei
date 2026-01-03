use tree_sitter::{Node, Range};

#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub level: Severity,
    pub message: String,
    pub labels: Vec<Label>,
    pub help: Option<String>,
    pub suggestion: Option<Suggestion>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Severity {
    Error,
    Warning,
}

#[derive(Debug, Clone)]
pub struct Suggestion {
    pub line_idx: usize,
    pub original_line: String,
    pub start_col: usize,
    pub end_col: usize,
    pub replacement: String,
}

#[derive(Debug, Clone)]
pub struct Label {
    pub range: Range,
    pub message: String,
    pub annotation: Annotation,
}

impl Label {
    pub fn line_idx(&self) -> usize {
        self.range.start_point.row
    }

    pub fn end_line_idx(&self) -> usize {
        self.range.end_point.row
    }

    pub fn col_display(&self) -> usize {
        self.range.start_point.column + 1
    }

    pub fn span_len(&self) -> usize {
        self.range
            .end_point
            .column
            .saturating_sub(self.range.start_point.column)
            .max(1)
    }

    pub fn underline_char(&self) -> &str {
        match self.annotation {
            Annotation::Primary => "^",
            Annotation::Secondary => "-",
        }
    }

    pub fn is_multiline(&self) -> bool {
        self.range.start_point.row != self.range.end_point.row
    }
}

#[derive(Debug, Clone)]
pub enum Annotation {
    Primary,
    Secondary,
}

impl Diagnostic {
    pub fn error(message: impl Into<String>) -> Self {
        Self {
            level: Severity::Error,
            message: message.into(),
            labels: Vec::new(),
            help: None,
            suggestion: None,
        }
    }

    pub fn warning(message: impl Into<String>) -> Self {
        Self {
            level: Severity::Warning,
            message: message.into(),
            labels: Vec::new(),
            help: None,
            suggestion: None,
        }
    }

    pub fn from_io(error: &std::io::Error, path: &str) -> Self {
        use std::io::ErrorKind;
        match error.kind() {
            ErrorKind::PermissionDenied => Self::error(format!("permission denied: `{}`", path)),
            ErrorKind::InvalidData => Self::error(format!("invalid UTF-8 in `{}`", path)),
            ErrorKind::NotFound => Self::error(format!("file not found: `{}`", path)),
            _ => Self::error(format!("failed to read `{}`", path)),
        }
    }

    pub fn from_glob(error: globset::Error, pattern: &str) -> Self {
        let err_str = error.to_string();
        let msg = err_str.split(": ").last().unwrap_or(&err_str);
        Self::error(format!("invalid glob pattern `{}`: {}", pattern, msg))
    }

    pub fn file_would_be_formatted() -> Self {
        Self::error("file would be formatted")
    }

    pub fn no_html_files_in_directory(path: &str) -> Self {
        Self::error(format!("no `.html` files in directory `{}`", path))
    }

    pub fn stdin_filepath_requires_stdin() -> Self {
        Self::error("--stdin-filepath requires stdin input (use `-` as the path)")
    }

    pub fn not_a_regular_file(path: &str) -> Self {
        Self::error(format!("`{}` is not a regular file", path))
    }

    pub fn path_does_not_exist(path: &str) -> Self {
        Self::error(format!("path `{}` does not exist", path))
    }

    pub fn with_label(
        mut self,
        range: Range,
        message: impl Into<String>,
        annotation: Annotation,
    ) -> Self {
        self.labels.push(Label {
            range,
            message: message.into(),
            annotation,
        });
        self
    }

    pub fn with_help(mut self, help: impl Into<String>) -> Self {
        self.help = Some(help.into());
        self
    }

    pub fn with_suggestion(
        mut self,
        line_index: usize,
        line: impl Into<String>,
        start_col: usize,
        end_col: usize,
        replacement: impl Into<String>,
    ) -> Self {
        self.suggestion = Some(Suggestion {
            line_idx: line_index,
            original_line: line.into(),
            start_col,
            end_col,
            replacement: replacement.into(),
        });
        self
    }

    pub fn find_error_node<'a>(node: &Node<'a>) -> Option<Node<'a>> {
        if node.is_error() || node.is_missing() {
            return Some(*node);
        }
        for child in node.children(&mut node.walk()) {
            if let Some(err) = Self::find_error_node(&child) {
                return Some(err);
            }
        }
        None
    }

    pub fn refine_error_range(error_node: &Node) -> Range {
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
                return Self::range_between(first, last);
            }
        }

        let first = non_error_children.first().unwrap();
        let last = non_error_children.last().unwrap();
        Self::range_between(first, last)
    }

    fn range_between(first: &Node, last: &Node) -> Range {
        Range {
            start_byte: first.range().start_byte,
            start_point: first.range().start_point,
            end_byte: last.range().end_byte,
            end_point: last.range().end_point,
        }
    }
}
