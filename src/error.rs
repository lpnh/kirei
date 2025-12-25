use std::fmt;
use tree_sitter::Range;

use crate::draw::Draw;

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
        eprintln!("{}", Draw::new(&self, source.unwrap_or(""), file_path));
        std::process::exit(1);
    }
}

impl fmt::Display for KireiError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::ErroneousEndTag {
                expected, found, ..
            } => {
                write!(f, "expected `{}`, found `{}`", expected, found)
            }
            Self::AskamaSyntaxError { .. } => write!(f, "failed to parse Askama"),
            Self::HtmlSyntaxError { .. } => write!(f, "failed to parse HTML"),
            Self::IoError { kind, path } => {
                use std::io::ErrorKind as IoKind;
                match (kind, path) {
                    (IoKind::NotFound, Some(path)) => write!(f, "file `{}` does not exist", path),
                    (IoKind::NotFound, None) => write!(f, "file does not exist"),
                    (IoKind::PermissionDenied, Some(path)) => {
                        write!(f, "permission denied when accessing `{}`", path)
                    }
                    (IoKind::PermissionDenied, None) => write!(f, "permission denied"),
                    (IoKind::AlreadyExists, Some(path)) => {
                        write!(f, "file `{}` already exists", path)
                    }
                    (IoKind::AlreadyExists, None) => write!(f, "file already exists"),
                    (IoKind::InvalidInput, _) => write!(f, "invalid input"),
                    (IoKind::InvalidData, _) => write!(f, "invalid data"),
                    (IoKind::TimedOut, _) => write!(f, "operation timed out"),
                    (IoKind::Interrupted, _) => write!(f, "operation interrupted"),
                    (IoKind::UnexpectedEof, Some(path)) => {
                        write!(f, "unexpected end of file in `{}`", path)
                    }
                    (IoKind::UnexpectedEof, None) => write!(f, "unexpected end of file"),
                    (IoKind::BrokenPipe, _) => write!(f, "broken pipe"),
                    (kind, Some(path)) => {
                        write!(f, "IO error when accessing `{}`: {:?}", path, kind)
                    }
                    (kind, None) => write!(f, "IO error: {:?}", kind),
                }
            }
            Self::Msg { message } => write!(f, "{}", message),
        }
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
                eprintln!("{}", Draw::new(&error, source.unwrap_or(""), file_path));
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
                eprintln!("{}", Draw::new(&error, source.unwrap_or(""), file_path));
                std::process::exit(1);
            }
        }
    }
}
