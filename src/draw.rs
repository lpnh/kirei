use colored::Colorize;
use std::collections::BTreeSet;
use tree_sitter::Range;

use crate::error::KireiError;

struct KireiDiagnostic {
    message: String,
    labels: Vec<Label>,
    help: Option<String>,
    suggestion: Option<(usize, String, usize, usize, String)>,
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

pub fn draw(error: &KireiError, source: &str, file_path: Option<&str>) -> String {
    let diag = diagnostic_from(error, source);
    let mut output = String::new();

    if diag.labels.is_empty() {
        return format!(
            "{}{} {}",
            "error".bold().red(),
            ":".bold(),
            diag.message.bold()
        );
    }

    let max_line_idx = diag.labels.iter().map(Label::line_idx).max().unwrap_or(0);
    let line_num_width = (max_line_idx + 1).to_string().len();

    // Header
    output += &format!(
        "{}{} {}\n",
        "error".bold().red(),
        ":".bold(),
        diag.message.bold()
    );

    // Span pointer (file:line:col)
    if let Some(primary) = diag
        .labels
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

    let mut diagnostic = BTreeSet::new();
    for label in &diag.labels {
        diagnostic.insert(label.line_idx());
    }

    let source_lines: Vec<&str> = source.lines().collect();

    // Source snippets with labels
    let mut last_line_idx = None;
    for &line_idx in &diagnostic {
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

            let line_labels: Vec<_> = diag
                .labels
                .iter()
                .filter(|l| l.line_idx() == line_idx)
                .collect();

            if !line_labels.is_empty() {
                output += &format!(
                    "{} {} ",
                    " ".repeat(line_num_width),
                    "|".bold().bright_blue()
                );
                output += &underlines(&line_labels);
                output += "\n";
            }
        }

        last_line_idx = Some(line_idx);
    }

    // Help
    if let Some(help_msg) = &diag.help {
        output += &format!(
            "{} {}\n",
            " ".repeat(line_num_width),
            "|".bold().bright_blue()
        );
        output += &format!("{}{} {}\n", "help".bold().cyan(), ":".bold(), help_msg);

        // Suggestion
        if let Some((line_idx, original_line, start_col, end_col, replacement_text)) =
            &diag.suggestion
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

fn diagnostic_from(error: &KireiError, source: &str) -> KireiDiagnostic {
    let message = error.message();
    let labels = error
        .labels()
        .into_iter()
        .map(|(range, text, annotation)| match annotation {
            Annotation::Primary => Label::primary(range, text),
            Annotation::Secondary => Label::secondary(range, text),
        })
        .collect();
    let help = error.help();
    let suggestion = error.suggestion(source);

    KireiDiagnostic {
        message,
        labels,
        help,
        suggestion,
    }
}

fn underlines(labels: &[&Label]) -> String {
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

        let highlight = label.underline_char().repeat(label.span_len());
        let colored_hl = match label.annotation {
            Annotation::Primary => highlight.red(),
            Annotation::Secondary => highlight.bold().bright_blue(),
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
            output += &format!(" {}", primary.message.bold().red());
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
            output += &label.message.bold().bright_blue().to_string();
        }
    } else if let Some(label) = sorted_labels.first()
        && !label.message.is_empty()
    {
        let colored_msg = match label.annotation {
            Annotation::Primary => label.message.bold().red().to_string(),
            Annotation::Secondary => label.message.bold().bright_blue().to_string(),
        };
        output += &format!(" {}", colored_msg);
    }

    output
}
