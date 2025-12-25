use std::fmt;
use tree_sitter::Range;

use crate::draw::{Annotation, Draw};

#[derive(Debug)]
pub enum KireiError {
    ErroneousEndTag {
        expected: String,
        found: String,
        open_name_range: Range,
        close_name_range: Range,
    },
    AskamaSyntaxError {
        error_range: Range,
    },
    HtmlSyntaxError {
        error_range: Range,
    },
    IoError {
        kind: std::io::ErrorKind,
        path: Option<String>,
    },
    Msg {
        message: String,
    },
}

impl KireiError {
    pub fn draw_msg(message: impl Into<String>) {
        Self::msg(message).draw(None, None);
    }

    pub fn msg(message: impl Into<String>) -> Self {
        Self::Msg {
            message: message.into(),
        }
    }

    fn draw(self, source: Option<&str>, file_path: Option<&str>) {
        eprint!("{}", Draw::new(&self, source.unwrap_or(""), file_path));
        std::process::exit(1);
    }

    pub fn message(&self) -> String {
        match self {
            Self::ErroneousEndTag { .. } => "unexpected closing tag".to_string(),
            Self::AskamaSyntaxError { .. } => "failed to parse Askama".to_string(),
            Self::HtmlSyntaxError { .. } => "failed to parse HTML".to_string(),
            Self::IoError { kind, path } => {
                use std::io::ErrorKind as IoKind;
                match (kind, path) {
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
                }
            }
            Self::Msg { message } => message.clone(),
        }
    }

    pub fn labels(&self) -> Vec<(Range, String, Annotation)> {
        match self {
            Self::ErroneousEndTag {
                expected,
                found,
                open_name_range,
                close_name_range,
                ..
            } => {
                vec![
                    (
                        *close_name_range,
                        format!("expected `{}`, found `{}`", expected, found),
                        Annotation::Primary,
                    ),
                    (
                        *open_name_range,
                        "expected due to this open tag name".to_string(),
                        Annotation::Secondary,
                    ),
                ]
            }
            Self::AskamaSyntaxError { error_range } => {
                vec![(*error_range, String::new(), Annotation::Primary)]
            }
            Self::HtmlSyntaxError { error_range } => {
                vec![(*error_range, String::new(), Annotation::Primary)]
            }
            Self::IoError { .. } | Self::Msg { .. } => vec![],
        }
    }

    pub fn help(&self) -> Option<String> {
        match self {
            Self::ErroneousEndTag { expected, .. } => {
                Some(format!("consider using `{}`", expected))
            }
            _ => None,
        }
    }

    pub fn suggestion(&self, source: &str) -> Option<(usize, String, usize, usize, String)> {
        match self {
            Self::ErroneousEndTag {
                expected,
                close_name_range,
                ..
            } => {
                let line_index = close_name_range.start_point.row;
                let start_col = close_name_range.start_point.column;
                let end_col = close_name_range.end_point.column;

                source.lines().nth(line_index).map(|line| {
                    (
                        line_index,
                        line.to_string(),
                        start_col,
                        end_col,
                        expected.to_string(),
                    )
                })
            }
            _ => None,
        }
    }
}

impl fmt::Display for KireiError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.message())
    }
}

impl std::error::Error for KireiError {}

impl From<std::io::Error> for KireiError {
    fn from(e: std::io::Error) -> Self {
        Self::IoError {
            kind: e.kind(),
            path: None,
        }
    }
}

impl From<std::str::Utf8Error> for KireiError {
    fn from(e: std::str::Utf8Error) -> Self {
        KireiError::msg(format!("UTF8 error: {}", e))
    }
}

pub trait OrMsg<T> {
    fn or_msg(self, msg: impl Into<String>) -> Result<T, KireiError>;
}

impl<T, E: std::fmt::Display> OrMsg<T> for Result<T, E> {
    fn or_msg(self, msg: impl Into<String>) -> Result<T, KireiError> {
        self.map_err(|e| KireiError::msg(format!("{}: {}", msg.into(), e)))
    }
}

impl<T> OrMsg<T> for Option<T> {
    fn or_msg(self, msg: impl Into<String>) -> Result<T, KireiError> {
        self.ok_or_else(|| KireiError::msg(msg))
    }
}

pub trait OrDraw<T> {
    fn or_draw(self, source: Option<&str>, file_path: Option<&str>) -> T;
}

impl<T> OrDraw<T> for Result<T, KireiError> {
    fn or_draw(self, source: Option<&str>, file_path: Option<&str>) -> T {
        match self {
            Ok(value) => value,
            Err(error) => {
                eprint!("{}", Draw::new(&error, source.unwrap_or(""), file_path));
                std::process::exit(1);
            }
        }
    }
}

impl<T> OrDraw<T> for Result<T, std::io::Error> {
    fn or_draw(self, source: Option<&str>, file_path: Option<&str>) -> T {
        match self {
            Ok(value) => value,
            Err(e) => {
                let error = KireiError::IoError {
                    kind: e.kind(),
                    path: file_path.map(|s| s.to_string()),
                };
                eprint!("{}", Draw::new(&error, source.unwrap_or(""), file_path));
                std::process::exit(1);
            }
        }
    }
}
