use colored::Colorize;
use std::collections::BTreeSet;

use crate::diagnostics::{Annotation, Diagnostic, Label, Severity};

pub fn eprint_diagnostic(diagnostic: &Diagnostic, source: &str, filepath: Option<&str>) {
    eprint!("{}", draw(diagnostic, source, filepath));
}

#[derive(Debug, Clone, Copy)]
enum Underline {
    Inline,
    Block,
}

pub fn draw(diagnostic: &Diagnostic, source: &str, file_path: Option<&str>) -> String {
    let mut output = String::new();
    let (level_text, level_color) = level_style(diagnostic.level);

    if diagnostic.labels.is_empty() {
        return format!(
            "{}{} {}\n",
            level_color(level_text),
            ":".bold(),
            diagnostic.message.bold()
        );
    } else {
        let max_line_idx = diagnostic
            .labels
            .iter()
            .map(Label::line_idx)
            .max()
            .unwrap_or(0);
        let line_num_width = (max_line_idx + 1).to_string().len();

        output += &format!(
            "{}{} {}\n",
            level_color(level_text),
            ":".bold(),
            diagnostic.message.bold()
        );

        if let Some(primary) = diagnostic
            .labels
            .iter()
            .find(|l| matches!(l.annotation, Annotation::Primary))
        {
            output += &format!(
                "{}{} {}:{}:{}\n",
                " ".repeat(line_num_width),
                "-->".bold().bright_blue(),
                file_path.unwrap_or("<stdin>"),
                line_number(primary.line_idx()),
                primary.col_display()
            );
        }

        output += &format!("{} {}\n", " ".repeat(line_num_width), pipe());

        let diagnostic_lines = collect_diagnostic_lines(&diagnostic.labels);
        let source_lines: Vec<&str> = source.lines().collect();
        let style = if diagnostic.labels.iter().any(Label::is_multiline) {
            Underline::Block
        } else {
            Underline::Inline
        };

        output += &render_source_snippets(
            diagnostic,
            &diagnostic.labels,
            &diagnostic_lines,
            &source_lines,
            style,
            line_num_width,
        );

        if let Some(help_msg) = &diagnostic.help {
            output += &render_help(diagnostic, help_msg, line_num_width);
        }
    }

    output
}

fn level_style(level: Severity) -> (&'static str, fn(&str) -> colored::ColoredString) {
    match level {
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

fn collect_diagnostic_lines(labels: &[Label]) -> BTreeSet<usize> {
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
    diagnostic: &Diagnostic,
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
            let modified_content = apply_block_markers(content, labels, line_idx, style);

            output += &format!(
                "{} {} {}\n",
                line_number(line_idx),
                pipe(),
                modified_content
            );

            output += &render_underlines(diagnostic, labels, line_idx, style, line_num_width);
        }

        last_line_idx = Some(line_idx);
    }

    output
}

fn apply_block_markers(
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
    diagnostic: &Diagnostic,
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
        .filter(|l| l.is_multiline() && line_idx >= l.line_idx() && line_idx <= l.end_line_idx())
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

    let mut output = gutter(line_num_width);

    match style {
        Underline::Inline => {
            let all_labels: Vec<_> = single_line_labels
                .iter()
                .chain(multiline_labels.iter())
                .copied()
                .collect();
            output += &underlines(&all_labels, diagnostic.level);
        }
        Underline::Block => {
            if !single_line_labels.is_empty() {
                output += &underlines(&single_line_labels, diagnostic.level);
            }
            for label in &multiline_labels {
                output += &block_underline(label, line_idx, diagnostic.level);
            }
        }
    }

    output + "\n"
}

fn render_help(diagnostic: &Diagnostic, help_msg: &str, line_num_width: usize) -> String {
    let mut output = format!("{} {}\n", " ".repeat(line_num_width), pipe());
    output += &format!("{}{} {}\n", "help".bold().cyan(), ":".bold(), help_msg);

    if let Some(suggestion) = &diagnostic.suggestion {
        output += &format!("{} {}\n", " ".repeat(line_num_width), pipe());
        output += &format!("{} {} ", line_number(suggestion.line_idx), "~".green());
        output += &suggestion.original_line[..suggestion.start_col];
        output += &suggestion.replacement.green().to_string();
        output += &format!("{}\n", &suggestion.original_line[suggestion.end_col..]);
    }

    output
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
        let colored_hl = colorize_by_annotation(&highlight, severity, &label.annotation);
        output += &colored_hl.to_string();

        cursor = label.range.start_point.column + label.span_len();
    }

    if sorted_labels.len() > 1 {
        output += &render_multiple_label_messages(&sorted_labels, severity);
    } else if let Some(label) = sorted_labels.first()
        && !label.message.is_empty()
    {
        let colored_msg = colorize_by_annotation(&label.message, severity, &label.annotation);
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
        let colored_msg = colorize_by_annotation(&primary.message, severity, &primary.annotation);
        output += &format!(" {}", colored_msg);
    }

    if let Some(label) = labels
        .iter()
        .find(|l| matches!(l.annotation, Annotation::Secondary) && !l.message.is_empty())
    {
        let line_num_width = (label.line_idx() + 1).to_string().len();
        output += &format!("\n{}", gutter(line_num_width));
        output += &" ".repeat(label.range.start_point.column);
        output += &format!("{}\n", pipe());
        output += &gutter(line_num_width);
        output += &" ".repeat(label.range.start_point.column);
        let colored_msg = colorize_by_annotation(&label.message, severity, &label.annotation);
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

    colorize_by_annotation(&output, severity, &label.annotation).to_string()
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
