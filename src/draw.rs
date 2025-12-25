use colored::Colorize;
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

    fn annotations(&self) -> Vec<Annotation> {
        match self.error {
            KireiError::ErroneousEndTag {
                expected,
                open_name_range,
                close_name_range,
                ..
            } => {
                vec![
                    Annotation::secondary(*open_name_range, "expected due to this open tag name"),
                    Annotation::primary(
                        *close_name_range,
                        format!("help: consider using `{}`", expected),
                    ),
                ]
            }
            KireiError::AskamaSyntaxError { error_range }
            | KireiError::HtmlSyntaxError { error_range } => {
                vec![Annotation::primary(*error_range, "")]
            }
            KireiError::IoError { .. } | KireiError::Msg { .. } => vec![],
        }
    }
}

#[derive(Debug, Clone)]
enum AnnotationStyle {
    Primary,
    Secondary,
}

#[derive(Debug, Clone)]
struct Annotation {
    range: Range,
    label: String,
    style: AnnotationStyle,
}

impl Annotation {
    fn primary(range: Range, label: impl Into<String>) -> Self {
        Self {
            range,
            label: label.into(),
            style: AnnotationStyle::Primary,
        }
    }

    fn secondary(range: Range, label: impl Into<String>) -> Self {
        Self {
            range,
            label: label.into(),
            style: AnnotationStyle::Secondary,
        }
    }

    fn line(&self) -> usize {
        self.range.start_point.row + 1
    }

    fn col(&self) -> usize {
        self.range.start_point.column + 1
    }

    fn len(&self) -> usize {
        (self.range.end_point.column - self.range.start_point.column).max(1)
    }

    fn underline_char(&self) -> &str {
        match self.style {
            AnnotationStyle::Primary => "^",
            AnnotationStyle::Secondary => "-",
        }
    }
}

fn render_line_prefix(
    f: &mut fmt::Formatter,
    line: Option<usize>,
    max_line_width: usize,
) -> fmt::Result {
    match line {
        Some(n) => write!(
            f,
            "{} {} ",
            n.to_string().bold().bright_blue(),
            "|".bold().bright_blue()
        ),
        None => write!(
            f,
            "{} {} ",
            " ".repeat(max_line_width),
            "|".bold().bright_blue()
        ),
    }
}

fn render_underlines(
    f: &mut fmt::Formatter,
    anns: &[&Annotation],
    max_line_width: usize,
) -> fmt::Result {
    let mut sorted_anns = anns.to_vec();
    sorted_anns.sort_by_key(|a| (a.col(), matches!(a.style, AnnotationStyle::Secondary)));

    let has_multiple = sorted_anns.len() > 1;
    let mut current_pos = 1;

    for ann in &sorted_anns {
        let spaces = ann.col().saturating_sub(current_pos);
        write!(f, "{}", " ".repeat(spaces))?;

        let highlight = ann.underline_char().repeat(ann.len());
        let colored_hl = match ann.style {
            AnnotationStyle::Primary => highlight.red(),
            AnnotationStyle::Secondary => highlight.bold().bright_blue(),
        };
        write!(f, "{}", colored_hl)?;

        current_pos = ann.col() + ann.len();
    }

    if has_multiple {
        if let Some(primary) = sorted_anns
            .iter()
            .find(|a| matches!(a.style, AnnotationStyle::Primary))
            && !primary.label.is_empty()
        {
            write!(f, " {}", primary.label.bold().red())?;
        }

        for ann in sorted_anns
            .iter()
            .filter(|a| matches!(a.style, AnnotationStyle::Secondary) && !a.label.is_empty())
        {
            writeln!(f)?;
            render_line_prefix(f, None, max_line_width)?;
            write!(f, "{}", " ".repeat(ann.col() - 1))?;
            writeln!(f, "{}", "|".bold().bright_blue())?;
            render_line_prefix(f, None, max_line_width)?;
            write!(f, "{}", " ".repeat(ann.col() - 1))?;
            write!(f, "{}", ann.label.bold().bright_blue())?;
        }
    } else if let Some(ann) = sorted_anns.first()
        && !ann.label.is_empty()
    {
        let label = match ann.style {
            AnnotationStyle::Primary => ann.label.bold().red().to_string(),
            AnnotationStyle::Secondary => ann.label.bold().bright_blue().to_string(),
        };
        write!(f, " {}", label)?;
    }

    Ok(())
}

impl fmt::Display for Draw<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let annotations = self.annotations();

        if annotations.is_empty() {
            write!(
                f,
                "{}{} {}",
                "error".bold().red(),
                ":".bold(),
                self.error.to_string().bold()
            )?;
            return Ok(());
        }

        let max_line = annotations.iter().map(Annotation::line).max().unwrap_or(1);
        let max_line_width = max_line.to_string().len();

        writeln!(
            f,
            "{}{} {}",
            "error".bold().red(),
            ":".bold(),
            self.error.to_string().bold()
        )?;

        let location_ann = annotations
            .iter()
            .find(|a| matches!(a.style, AnnotationStyle::Primary))
            .or_else(|| annotations.first());

        if let Some(ann) = location_ann {
            let path = self.file_path.unwrap_or("<stdin>");
            writeln!(
                f,
                "{}{} {}:{}:{}",
                " ".repeat(max_line_width),
                "-->".bold().bright_blue(),
                path,
                ann.line(),
                ann.col()
            )?;
        }
        render_line_prefix(f, None, max_line_width)?;
        writeln!(f)?;

        let mut lines: Vec<_> = annotations.iter().map(Annotation::line).collect();
        lines.sort_unstable();
        lines.dedup();

        for (i, &line_num) in lines.iter().enumerate() {
            if i > 0 && line_num > lines[i - 1] + 1 {
                writeln!(f, "{}", "...".bold().bright_blue())?;
            }

            if let Some(content) = self.source.lines().collect::<Vec<_>>().get(line_num - 1) {
                render_line_prefix(f, Some(line_num), max_line_width)?;
                writeln!(f, "{}", content)?;

                render_line_prefix(f, None, max_line_width)?;
                let line_anns: Vec<_> = annotations
                    .iter()
                    .filter(|a| a.line() == line_num)
                    .collect();
                render_underlines(f, &line_anns, max_line_width)?;

                if i < lines.len() - 1 {
                    writeln!(f)?;
                }
            }
        }

        Ok(())
    }
}
