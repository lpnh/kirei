use colored::Colorize;
use std::collections::BTreeSet;
use tree_sitter::{Node, Point, Range};

use crate::askama::{AskamaNode, ControlTag};
use crate::html::HtmlNode;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Severity {
    Error,
    Warning,
}

#[derive(Debug, Clone)]
struct Label {
    range: Range,
    message: String,
    annotation: Annotation,
}

impl Label {
    fn line_idx(&self) -> usize {
        self.range.start_point.row
    }

    fn col_display(&self) -> usize {
        self.range.start_point.column + 1
    }

    fn span_len(&self) -> usize {
        self.range
            .end_point
            .column
            .saturating_sub(self.range.start_point.column)
            .max(1)
    }

    fn underline_char(&self) -> &str {
        match self.annotation {
            Annotation::Primary => "^",
            Annotation::Secondary => "-",
        }
    }
}

#[derive(Debug, Clone)]
pub enum Annotation {
    Primary,
    Secondary,
}

#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub level: Severity,
    message: String,
    labels: Vec<(Range, String, Annotation)>,
    help: Option<String>,
    suggestion: Option<(usize, String, usize, usize, String)>,
}

impl Diagnostic {
    pub fn draw(&self, source: &str, file_path: Option<&str>) -> String {
        let mut output = String::new();

        let (level_text, level_color): (&str, fn(&str) -> colored::ColoredString) = match self.level
        {
            Severity::Error => ("error", |s| s.bold().red()),
            Severity::Warning => ("warning", |s| s.bold().yellow()),
        };

        if self.labels.is_empty() {
            return format!(
                "{}{} {}",
                level_color(level_text),
                ":".bold(),
                self.message.bold()
            );
        }

        let labels: Vec<_> = self
            .labels
            .iter()
            .map(|(r, m, a)| Label {
                range: *r,
                message: m.clone(),
                annotation: a.clone(),
            })
            .collect();

        let max_line_idx = labels.iter().map(Label::line_idx).max().unwrap_or(0);
        let line_num_width = (max_line_idx + 1).to_string().len();

        // Header
        output += &format!(
            "{}{} {}\n",
            level_color(level_text),
            ":".bold(),
            self.message.bold()
        );

        // Span pointer (file:line:col)
        if let Some(primary) = labels
            .iter()
            .find(|l| matches!(l.annotation, Annotation::Primary))
        {
            output += &format!(
                "{}{} {}:{}:{}\n",
                " ".repeat(line_num_width),
                "-->".bold().bright_blue(),
                file_path.unwrap_or("<stdin>"),
                primary.line_idx() + 1,
                primary.col_display()
            );
        }

        output += &format!(
            "{} {}\n",
            " ".repeat(line_num_width),
            "|".bold().bright_blue()
        );

        let mut diagnostic_lines = BTreeSet::new();
        for label in &labels {
            diagnostic_lines.insert(label.line_idx());
        }

        let source_lines: Vec<&str> = source.lines().collect();

        // Source snippets with labels
        let mut last_line_idx = None;
        for &line_idx in &diagnostic_lines {
            if let Some(prev) = last_line_idx
                && line_idx > prev + 1
            {
                output += &format!("{}\n", "...".bold().bright_blue());
            }

            if let Some(content) = source_lines.get(line_idx) {
                output += &format!(
                    "{} {} {}\n",
                    (line_idx + 1).to_string().bold().bright_blue(),
                    "|".bold().bright_blue(),
                    content
                );

                let line_labels: Vec<_> =
                    labels.iter().filter(|l| l.line_idx() == line_idx).collect();

                if !line_labels.is_empty() {
                    output += &format!(
                        "{} {} ",
                        " ".repeat(line_num_width),
                        "|".bold().bright_blue()
                    );
                    output += &Self::underlines(&line_labels, self.level);
                    output += "\n";
                }
            }

            last_line_idx = Some(line_idx);
        }

        // Help
        if let Some(help_msg) = &self.help {
            output += &format!(
                "{} {}\n",
                " ".repeat(line_num_width),
                "|".bold().bright_blue()
            );
            output += &format!("{}{} {}\n", "help".bold().cyan(), ":".bold(), help_msg);

            // Suggestion
            if let Some((line_idx, original_line, start_col, end_col, replacement_text)) =
                &self.suggestion
            {
                output += &format!(
                    "{} {}\n",
                    " ".repeat(line_num_width),
                    "|".bold().bright_blue()
                );
                output += &format!(
                    "{} {} ",
                    (line_idx + 1).to_string().bold().bright_blue(),
                    "~".green()
                );
                output += &original_line[..*start_col];
                output += &replacement_text.green().to_string();
                output += &format!("{}\n", &original_line[*end_col..]);
            }
        }

        output
    }

    pub fn io_error(error: &std::io::Error, filepath: Option<&str>) -> ! {
        use std::io::ErrorKind as IoKind;

        let message = match (error.kind(), filepath) {
            (IoKind::NotFound, Some(path)) => format!("file `{}` does not exist", path),
            (IoKind::NotFound, None) => "file does not exist".to_string(),
            (IoKind::PermissionDenied, Some(path)) => {
                format!("permission denied when accessing `{}`", path)
            }
            (IoKind::PermissionDenied, None) => "permission denied".to_string(),
            (IoKind::AlreadyExists, Some(path)) => {
                format!("file `{}` already exists", path)
            }
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
            (kind, Some(path)) => {
                format!("IO error when accessing `{}`: {:?}", path, kind)
            }
            (kind, None) => format!("IO error: {:?}", kind),
        };

        let diagnostic = Self::error(message);
        eprintln!("{}", diagnostic.draw("", filepath));
        std::process::exit(1);
    }

    pub fn file_would_be_updated(filepath: Option<&str>) -> ! {
        let diagnostic = Self::error("file would be formatted");
        eprintln!("{}", diagnostic.draw("", filepath));
        std::process::exit(1);
    }

    pub fn erroneous_end_tag(
        expected: String,
        found: &str,
        open_range: Range,
        close_range: Range,
        source: &str,
    ) -> Self {
        let line_index = close_range.start_point.row;
        let start_col = close_range.start_point.column;
        let end_col = close_range.end_point.column;

        let mut diag = Self::error("unexpected closing tag")
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

    pub fn nesting_too_deep() -> Self {
        Self::error("nesting too deep")
    }

    pub fn syntax_error(root_node: &Node, kind: &str) -> Option<Self> {
        Self::find_error_node(root_node).map(|error_node| {
            Self::error(format!("failed to parse {}", kind)).with_label(
                error_node.range(),
                "",
                Annotation::Primary,
            )
        })
    }

    fn underlines(labels: &[&Label], severity: Severity) -> String {
        let mut output = String::new();
        let mut sorted_labels = labels.to_vec();
        sorted_labels.sort_by_key(|l| {
            (
                l.range.start_point.column,
                matches!(l.annotation, Annotation::Secondary),
            )
        });

        let has_multiple = sorted_labels.len() > 1;
        let mut cursor = 0;

        for label in &sorted_labels {
            let spaces = label.range.start_point.column.saturating_sub(cursor);
            output += &" ".repeat(spaces);

            let underline_char = match severity {
                Severity::Warning => "^",
                Severity::Error => label.underline_char(),
            };
            let highlight = underline_char.repeat(label.span_len());

            let colored_hl = match severity {
                Severity::Warning => highlight.bold().yellow(),
                Severity::Error => match label.annotation {
                    Annotation::Primary => highlight.bold().red(),
                    Annotation::Secondary => highlight.bold().bright_blue(),
                },
            };
            output += &colored_hl.to_string();

            cursor = label.range.start_point.column + label.span_len();
        }

        if has_multiple {
            if let Some(primary) = sorted_labels
                .iter()
                .find(|l| matches!(l.annotation, Annotation::Primary))
                && !primary.message.is_empty()
            {
                let colored_msg = match severity {
                    Severity::Warning => primary.message.bold().yellow(),
                    Severity::Error => primary.message.bold().red(),
                };
                output += &format!(" {}", colored_msg);
            }

            if let Some(label) = sorted_labels
                .iter()
                .find(|l| matches!(l.annotation, Annotation::Secondary) && !l.message.is_empty())
            {
                output += &format!(
                    "\n{} {} ",
                    " ".repeat((label.line_idx() + 1).to_string().len()),
                    "|".bold().bright_blue()
                );
                output += &" ".repeat(label.range.start_point.column);
                output += &format!("{}\n", "|".bold().bright_blue());
                output += &format!(
                    "{} {} ",
                    " ".repeat((label.line_idx() + 1).to_string().len()),
                    "|".bold().bright_blue()
                );
                output += &" ".repeat(label.range.start_point.column);
                let colored_msg = match severity {
                    Severity::Warning => label.message.bold().yellow(),
                    Severity::Error => label.message.bold().bright_blue(),
                };
                output += &colored_msg.to_string();
            }
        } else if let Some(label) = sorted_labels.first()
            && !label.message.is_empty()
        {
            let colored_msg = match severity {
                Severity::Warning => label.message.bold().yellow().to_string(),
                Severity::Error => match label.annotation {
                    Annotation::Primary => label.message.bold().red().to_string(),
                    Annotation::Secondary => label.message.bold().bright_blue().to_string(),
                },
            };
            output += &format!(" {}", colored_msg);
        }

        output
    }

    fn find_error_node<'a>(node: &Node<'a>) -> Option<Node<'a>> {
        if node.is_error() {
            return Some(*node);
        }
        for child in node.children(&mut node.walk()) {
            if let Some(err) = Self::find_error_node(&child) {
                return Some(err);
            }
        }
        None
    }

    pub fn has_crossing_boundary(start: usize, end: usize, askama_nodes: &[AskamaNode]) -> bool {
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
                    ControlTag::Else
                        | ControlTag::ElseIf
                        | ControlTag::When
                        | ControlTag::MatchElse
                ) && range.start > min_pos
                    && range.start < max_pos
                {
                    let mut closest_parent: Option<(usize, usize)> = None;
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

                    if let Some((parent_start, parent_end)) = closest_parent {
                        let min_inside = min_pos >= parent_start && min_pos < parent_end;
                        let max_inside = max_pos >= parent_start && max_pos < parent_end;

                        if min_inside && max_inside {
                            return true;
                        }
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

    fn error(message: impl Into<String>) -> Self {
        Self {
            level: Severity::Error,
            message: message.into(),
            labels: Vec::new(),
            help: None,
            suggestion: None,
        }
    }

    fn warning(message: impl Into<String>) -> Self {
        Self {
            level: Severity::Warning,
            message: message.into(),
            labels: Vec::new(),
            help: None,
            suggestion: None,
        }
    }

    fn with_label(
        mut self,
        range: Range,
        message: impl Into<String>,
        annotation: Annotation,
    ) -> Self {
        self.labels.push((range, message.into(), annotation));
        self
    }

    fn with_help(mut self, help: impl Into<String>) -> Self {
        self.help = Some(help.into());
        self
    }

    fn with_suggestion(
        mut self,
        line_index: usize,
        line: impl Into<String>,
        start_col: usize,
        end_col: usize,
        replacement: impl Into<String>,
    ) -> Self {
        self.suggestion = Some((
            line_index,
            line.into(),
            start_col,
            end_col,
            replacement.into(),
        ));
        self
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

fn range_from_bytes(source: &str, start: usize, end: usize) -> Range {
    Range {
        start_byte: start,
        end_byte: end,
        start_point: byte_to_point(source, start),
        end_point: byte_to_point(source, end),
    }
}

pub fn crossing_control_boundary(
    html_nodes: &mut [HtmlNode],
    askama_nodes: &[AskamaNode],
    source: &str,
) -> Vec<Diagnostic> {
    let mut diagnostics = Vec::new();

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

        if !Diagnostic::has_crossing_boundary(start_byte, end_byte, askama_nodes) {
            continue;
        }

        let open_range = range_from_bytes(source, start_byte + 1, start_byte + 1 + name.len());
        let close_range = range_from_bytes(source, end_byte + 2, end_byte + 2 + name.len());

        diagnostics.push(
            Diagnostic::warning("unbalanced HTML across control blocks")
                .with_label(
                    open_range,
                    "this opening tag is in a different scope...",
                    Annotation::Primary,
                )
                .with_label(
                    close_range,
                    "...than its closing tag",
                    Annotation::Secondary,
                ),
        );

        if let HtmlNode::Start { indent, end, .. } = &mut html_nodes[i] {
            *indent = 0;
            *end = None;
        }
        if let HtmlNode::End { indent, .. } = &mut html_nodes[end_idx] {
            *indent = 0;
        }
    }

    diagnostics
}
