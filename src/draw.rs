use colored::Colorize;
use std::collections::BTreeSet;
use tree_sitter::{Node, Range};

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

    fn end_line_idx(&self) -> usize {
        self.range.end_point.row
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

    fn is_multiline(&self) -> bool {
        self.range.start_point.row != self.range.end_point.row
    }
}

#[derive(Debug, Clone)]
pub enum Annotation {
    Primary,
    Secondary,
}

#[derive(Debug, Clone, Copy)]
enum Underline {
    Inline,
    Block,
}

#[derive(Debug, Clone)]
struct Suggestion {
    line_idx: usize,
    original_line: String,
    start_col: usize,
    end_col: usize,
    replacement: String,
}

#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub level: Severity,
    message: String,
    labels: Vec<Label>,
    help: Option<String>,
    suggestion: Option<Suggestion>,
}

impl Diagnostic {
    pub fn draw(&self, source: &str, file_path: Option<&str>) -> String {
        let mut output = String::new();
        let (level_text, level_color) = self.level_style();

        if self.labels.is_empty() {
            return format!(
                "{}{} {}",
                level_color(level_text),
                ":".bold(),
                self.message.bold()
            );
        }

        let max_line_idx = self.labels.iter().map(Label::line_idx).max().unwrap_or(0);
        let line_num_width = (max_line_idx + 1).to_string().len();

        output += &format!(
            "{}{} {}\n",
            level_color(level_text),
            ":".bold(),
            self.message.bold()
        );

        if let Some(primary) = self
            .labels
            .iter()
            .find(|l| matches!(l.annotation, Annotation::Primary))
        {
            output += &format!(
                "{}{} {}:{}:{}\n",
                " ".repeat(line_num_width),
                "-->".bold().bright_blue(),
                file_path.unwrap_or("<stdin>"),
                Self::line_number(primary.line_idx()),
                primary.col_display()
            );
        }

        output += &format!("{} {}\n", " ".repeat(line_num_width), Self::pipe());

        let diagnostic_lines = self.collect_diagnostic_lines(&self.labels);
        let source_lines: Vec<&str> = source.lines().collect();
        let style = if self.labels.iter().any(Label::is_multiline) {
            Underline::Block
        } else {
            Underline::Inline
        };

        output += &self.render_source_snippets(
            &self.labels,
            &diagnostic_lines,
            &source_lines,
            style,
            line_num_width,
        );

        if let Some(help_msg) = &self.help {
            output += &self.render_help(help_msg, line_num_width);
        }

        output
    }

    fn level_style(&self) -> (&str, fn(&str) -> colored::ColoredString) {
        match self.level {
            Severity::Error => ("error", |s| s.bold().red()),
            Severity::Warning => ("warning", |s| s.bold().yellow()),
        }
    }

    fn gutter(line_num_width: usize) -> String {
        format!(
            "{} {} ",
            " ".repeat(line_num_width),
            "|".bold().bright_blue()
        )
    }

    fn pipe() -> colored::ColoredString {
        "|".bold().bright_blue()
    }

    fn line_number(line_idx: usize) -> colored::ColoredString {
        (line_idx + 1).to_string().bold().bright_blue()
    }

    fn collect_diagnostic_lines(&self, labels: &[Label]) -> BTreeSet<usize> {
        let mut diagnostic_lines = BTreeSet::new();
        for label in labels {
            if label.is_multiline() {
                for line_idx in label.line_idx()..=label.end_line_idx() {
                    diagnostic_lines.insert(line_idx);
                }
            } else {
                diagnostic_lines.insert(label.line_idx());
            }
        }
        diagnostic_lines
    }

    fn render_source_snippets(
        &self,
        labels: &[Label],
        diagnostic_lines: &BTreeSet<usize>,
        source_lines: &[&str],
        style: Underline,
        line_num_width: usize,
    ) -> String {
        let mut output = String::new();
        let mut last_line_idx = None;

        for &line_idx in diagnostic_lines {
            if let Some(prev) = last_line_idx
                && line_idx > prev + 1
            {
                output += &format!("{}\n", "...".bold().bright_blue());
            }

            if let Some(content) = source_lines.get(line_idx) {
                let modified_content = self.apply_block_markers(content, labels, line_idx, style);

                output += &format!(
                    "{} {} {}\n",
                    Self::line_number(line_idx),
                    Self::pipe(),
                    modified_content
                );

                output += &self.render_underlines(labels, line_idx, style, line_num_width);
            }

            last_line_idx = Some(line_idx);
        }

        output
    }

    fn apply_block_markers(
        &self,
        content: &str,
        labels: &[Label],
        line_idx: usize,
        style: Underline,
    ) -> String {
        match style {
            Underline::Block => {
                let mut modified = format!("  {}", content);
                for label in labels {
                    if label.is_multiline()
                        && line_idx > label.line_idx()
                        && line_idx <= label.end_line_idx()
                    {
                        let mut chars: Vec<char> = modified.chars().collect();
                        chars[0] = '|';
                        modified = chars.into_iter().collect();
                    }
                }
                modified
            }
            Underline::Inline => content.to_string(),
        }
    }

    fn render_underlines(
        &self,
        labels: &[Label],
        line_idx: usize,
        style: Underline,
        line_num_width: usize,
    ) -> String {
        let single_line_labels: Vec<_> = labels
            .iter()
            .filter(|l| !l.is_multiline() && l.line_idx() == line_idx)
            .collect();

        let multiline_labels: Vec<_> = labels
            .iter()
            .filter(|l| {
                l.is_multiline() && line_idx >= l.line_idx() && line_idx <= l.end_line_idx()
            })
            .collect();

        let needs_underline = match style {
            Underline::Inline => !single_line_labels.is_empty() || !multiline_labels.is_empty(),
            Underline::Block => {
                !single_line_labels.is_empty()
                    || multiline_labels
                        .iter()
                        .any(|l| l.line_idx() == line_idx || l.end_line_idx() == line_idx)
            }
        };

        if !needs_underline {
            return String::new();
        }

        let mut output = Self::gutter(line_num_width);

        match style {
            Underline::Inline => {
                let all_labels: Vec<_> = single_line_labels
                    .iter()
                    .chain(multiline_labels.iter())
                    .copied()
                    .collect();
                output += &Self::underlines(&all_labels, self.level);
            }
            Underline::Block => {
                if !single_line_labels.is_empty() {
                    output += &Self::underlines(&single_line_labels, self.level);
                }
                for label in &multiline_labels {
                    output += &Self::block_underline(label, line_idx, self.level);
                }
            }
        }

        output + "\n"
    }

    fn render_help(&self, help_msg: &str, line_num_width: usize) -> String {
        let mut output = format!("{} {}\n", " ".repeat(line_num_width), Self::pipe());
        output += &format!("{}{} {}\n", "help".bold().cyan(), ":".bold(), help_msg);

        if let Some(suggestion) = &self.suggestion {
            output += &format!("{} {}\n", " ".repeat(line_num_width), Self::pipe());
            output += &format!(
                "{} {} ",
                Self::line_number(suggestion.line_idx),
                "~".green()
            );
            output += &suggestion.original_line[..suggestion.start_col];
            output += &suggestion.replacement.green().to_string();
            output += &format!("{}\n", &suggestion.original_line[suggestion.end_col..]);
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

    pub fn missing_syntax(node: &Node, _kind: &str) -> Self {
        let node_kind = node.kind();
        let parent = node.parent();

        let (message, label) = if let ("identifier", Some("block_statement")) =
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
        Self::error(message).with_label(node.range(), &label, Annotation::Primary)
    }

    pub fn syntax_error(root_node: &Node, kind: &str) -> Option<Self> {
        Self::find_error_node(root_node).map(|error_node| {
            if error_node.is_missing() {
                return Self::missing_syntax(&error_node, kind);
            }

            let range = Self::refine_error_range(&error_node);
            Self::error(format!("failed to parse {}", kind)).with_label(
                range,
                "",
                Annotation::Primary,
            )
        })
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

    fn underlines(labels: &[&Label], severity: Severity) -> String {
        let mut output = String::new();
        let mut sorted_labels = labels.to_vec();
        sorted_labels.sort_by_key(|l| {
            (
                l.range.start_point.column,
                matches!(l.annotation, Annotation::Secondary),
            )
        });

        let mut cursor = 0;
        for label in &sorted_labels {
            let spaces = label.range.start_point.column.saturating_sub(cursor);
            output += &" ".repeat(spaces);

            let underline_char = match severity {
                Severity::Warning => "^",
                Severity::Error => label.underline_char(),
            };
            let highlight = underline_char.repeat(label.span_len());
            let colored_hl = Self::colorize_by_annotation(&highlight, severity, &label.annotation);
            output += &colored_hl.to_string();

            cursor = label.range.start_point.column + label.span_len();
        }

        if sorted_labels.len() > 1 {
            output += &Self::render_multiple_label_messages(&sorted_labels, severity);
        } else if let Some(label) = sorted_labels.first()
            && !label.message.is_empty()
        {
            let colored_msg =
                Self::colorize_by_annotation(&label.message, severity, &label.annotation);
            output += &format!(" {}", colored_msg);
        }

        output
    }

    fn render_multiple_label_messages(labels: &[&Label], severity: Severity) -> String {
        let mut output = String::new();

        if let Some(primary) = labels
            .iter()
            .find(|l| matches!(l.annotation, Annotation::Primary))
            && !primary.message.is_empty()
        {
            let colored_msg =
                Self::colorize_by_annotation(&primary.message, severity, &primary.annotation);
            output += &format!(" {}", colored_msg);
        }

        if let Some(label) = labels
            .iter()
            .find(|l| matches!(l.annotation, Annotation::Secondary) && !l.message.is_empty())
        {
            let line_num_width = (label.line_idx() + 1).to_string().len();
            output += &format!("\n{}", Self::gutter(line_num_width));
            output += &" ".repeat(label.range.start_point.column);
            output += &format!("{}\n", Self::pipe());
            output += &Self::gutter(line_num_width);
            output += &" ".repeat(label.range.start_point.column);
            let colored_msg =
                Self::colorize_by_annotation(&label.message, severity, &label.annotation);
            output += &colored_msg.to_string();
        }

        output
    }

    fn block_underline(label: &Label, current_line: usize, severity: Severity) -> String {
        let start_col = label.range.start_point.column;
        let end_col = label.range.end_point.column;
        let is_first_line = current_line == label.line_idx();
        let is_last_line = current_line == label.end_line_idx();

        let underline_char = match severity {
            Severity::Warning => "^",
            Severity::Error => label.underline_char(),
        };

        let visual_start_col = start_col + 2;
        let visual_end_col = end_col + 2;

        let output = if is_first_line {
            format!(
                " {}{}",
                "_".repeat(visual_start_col.saturating_sub(1)),
                underline_char
            )
        } else if is_last_line {
            format!(
                "|{}{}",
                "_".repeat(visual_end_col.saturating_sub(2)),
                underline_char
            )
        } else {
            "|".to_string()
        };

        Self::colorize_by_annotation(&output, severity, &label.annotation).to_string()
    }

    fn colorize_by_annotation(
        text: &str,
        severity: Severity,
        annotation: &Annotation,
    ) -> colored::ColoredString {
        match severity {
            Severity::Warning => text.bold().yellow(),
            Severity::Error => match annotation {
                Annotation::Primary => text.bold().red(),
                Annotation::Secondary => text.bold().bright_blue(),
            },
        }
    }

    fn find_error_node<'a>(node: &Node<'a>) -> Option<Node<'a>> {
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


    fn error(message: impl Into<String>) -> Self {
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
        self.suggestion = Some(Suggestion {
            line_idx: line_index,
            original_line: line.into(),
            start_col,
            end_col,
            replacement: replacement.into(),
        });
        self
    }
}

