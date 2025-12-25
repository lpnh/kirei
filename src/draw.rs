use colored::Colorize;
use std::collections::BTreeSet;
use std::fmt;
use tree_sitter::Range;

use crate::error::KireiError;

pub struct Draw<'a> {
    error: &'a KireiError,
    source: &'a str,
    file_path: Option<&'a str>,
}

impl<'a> Draw<'a> {
    pub fn new(error: &'a KireiError, source: &'a str, file_path: Option<&'a str>) -> Self {
        Self {
            error,
            source,
            file_path,
        }
    }

    fn build_diagnostic(&self) -> KireiDiagnostic {
        let message = self.error.message();

        let labels = self
            .error
            .labels()
            .into_iter()
            .map(|(range, text, annotation)| match annotation {
                Annotation::Primary => Label::primary(range, text),
                Annotation::Secondary => Label::secondary(range, text),
            })
            .collect();

        let help = self.error.help();
        let suggestion = self.error.suggestion(self.source);

        KireiDiagnostic {
            message,
            labels,
            help,
            suggestion,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Annotation {
    Primary,
    Secondary,
}

#[derive(Debug, Clone)]
struct Label {
    range: Range,
    message: String,
    annotation: Annotation,
}

impl Label {
    fn primary(range: Range, message: impl Into<String>) -> Self {
        Self {
            range,
            message: message.into(),
            annotation: Annotation::Primary,
        }
    }

    fn secondary(range: Range, message: impl Into<String>) -> Self {
        Self {
            range,
            message: message.into(),
            annotation: Annotation::Secondary,
        }
    }

    fn line_index(&self) -> usize {
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

struct KireiDiagnostic {
    message: String,
    labels: Vec<Label>,
    help: Option<String>,
    suggestion: Option<(usize, String, usize, usize, String)>,
}

impl fmt::Display for Draw<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let diag = self.build_diagnostic();

        if diag.labels.is_empty() {
            return write!(
                f,
                "{}{} {}",
                "error".bold().red(),
                ":".bold(),
                diag.message.bold()
            );
        }

        let max_line_index = diag.labels.iter().map(Label::line_index).max().unwrap_or(0);
        let line_num_width = (max_line_index + 1).to_string().len();

        // Header
        writeln!(
            f,
            "{}{} {}",
            "error".bold().red(),
            ":".bold(),
            diag.message.bold()
        )?;

        // Span pointer (file:line:col)
        if let Some(primary) = diag
            .labels
            .iter()
            .find(|l| matches!(l.annotation, Annotation::Primary))
        {
            writeln!(
                f,
                "{}{} {}:{}:{}",
                " ".repeat(line_num_width),
                "-->".bold().bright_blue(),
                self.file_path.unwrap_or("<stdin>"),
                primary.line_index() + 1,
                primary.col_display()
            )?;
        }

        writeln!(
            f,
            "{} {}",
            " ".repeat(line_num_width),
            "|".bold().bright_blue()
        )?;

        let mut rendered_lines = BTreeSet::new();
        for label in &diag.labels {
            rendered_lines.insert(label.line_index());
        }

        let source_lines: Vec<&str> = self.source.lines().collect();

        // Source snippets with labels
        let mut last_line_index = None;
        for &line_index in &rendered_lines {
            if let Some(prev) = last_line_index
                && line_index > prev + 1
            {
                writeln!(f, "{}", "...".bold().bright_blue())?;
            }

            if let Some(content) = source_lines.get(line_index) {
                write!(
                    f,
                    "{} {} ",
                    (line_index + 1).to_string().bold().bright_blue(),
                    "|".bold().bright_blue()
                )?;
                writeln!(f, "{}", content)?;

                let line_labels: Vec<_> = diag
                    .labels
                    .iter()
                    .filter(|l| l.line_index() == line_index)
                    .collect();

                if !line_labels.is_empty() {
                    write!(
                        f,
                        "{} {} ",
                        " ".repeat(line_num_width),
                        "|".bold().bright_blue()
                    )?;
                    render_underlines(f, &line_labels)?;
                    writeln!(f)?;
                }
            }

            last_line_index = Some(line_index);
        }

        // Help
        if let Some(help_msg) = &diag.help {
            writeln!(
                f,
                "{} {}",
                " ".repeat(line_num_width),
                "|".bold().bright_blue()
            )?;
            writeln!(f, "{}{} {}", "help".bold().cyan(), ":".bold(), help_msg)?;

            // Suggestion
            if let Some((line_idx, original_line, start_col, end_col, replacement_text)) =
                &diag.suggestion
            {
                writeln!(
                    f,
                    "{} {}",
                    " ".repeat(line_num_width),
                    "|".bold().bright_blue()
                )?;
                write!(
                    f,
                    "{} {} ",
                    (line_idx + 1).to_string().bold().bright_blue(),
                    "~".green()
                )?;
                write!(f, "{}", &original_line[..*start_col])?;
                write!(f, "{}", replacement_text.green())?;
                writeln!(f, "{}", &original_line[*end_col..])?;
            }
        }

        Ok(())
    }
}

fn render_underlines(f: &mut fmt::Formatter, labels: &[&Label]) -> fmt::Result {
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
        write!(f, "{}", " ".repeat(spaces))?;

        let highlight = label.underline_char().repeat(label.span_len());
        let colored_hl = match label.annotation {
            Annotation::Primary => highlight.red(),
            Annotation::Secondary => highlight.bold().bright_blue(),
        };
        write!(f, "{}", colored_hl)?;

        cursor = label.range.start_point.column + label.span_len();
    }

    if has_multiple {
        if let Some(primary) = sorted_labels
            .iter()
            .find(|l| matches!(l.annotation, Annotation::Primary))
            && !primary.message.is_empty()
        {
            write!(f, " {}", primary.message.bold().red())?;
        }

        if let Some(label) = sorted_labels
            .iter()
            .find(|l| matches!(l.annotation, Annotation::Secondary) && !l.message.is_empty())
        {
            writeln!(f)?;
            write!(
                f,
                "{} {} ",
                " ".repeat((label.line_index() + 1).to_string().len()),
                "|".bold().bright_blue()
            )?;
            write!(f, "{}", " ".repeat(label.range.start_point.column))?;
            writeln!(f, "{}", "|".bold().bright_blue())?;
            write!(
                f,
                "{} {} ",
                " ".repeat((label.line_index() + 1).to_string().len()),
                "|".bold().bright_blue()
            )?;
            write!(f, "{}", " ".repeat(label.range.start_point.column))?;
            write!(f, "{}", label.message.bold().bright_blue())?;
        }
    } else if let Some(label) = sorted_labels.first()
        && !label.message.is_empty()
    {
        let colored_msg = match label.annotation {
            Annotation::Primary => label.message.bold().red().to_string(),
            Annotation::Secondary => label.message.bold().bright_blue().to_string(),
        };
        write!(f, " {}", colored_msg)?;
    }

    Ok(())
}
