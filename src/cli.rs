use clap::Parser;
use std::io::{self, Read};

use crate::{
    diagnostics::{Diagnostic, Severity},
    draw::draw,
    write::Kirei,
};

#[derive(Parser)]
#[command(name = "kirei", about, version)]
#[command(arg_required_else_help = true)]
struct Args {
    /// Path of file to format
    #[arg(value_name = "PATH", help_heading = "Input")]
    input: String,

    /// Check if files are formatted
    #[arg(short = 'c', long = "check")]
    check: bool,

    /// Edit files in-place (overwrite with formatted output)
    #[arg(short = 'w', long = "write")]
    write: bool,

    /// Print the names of files that would be changed
    #[arg(short = 'l', long = "list-different")]
    list_different: bool,

    /// When reading from stdin, use this as the filepath
    #[arg(
        long = "stdin-filepath",
        value_name = "PATH",
        help_heading = "Other options"
    )]
    stdin_filepath: Option<String>,
}

pub fn run() {
    let args = Args::parse();

    let (input, filepath) = if args.input == "-" {
        let mut buffer = String::new();
        io::stdin()
            .read_to_string(&mut buffer)
            .unwrap_or_else(|e| io_error(&e, None));
        (buffer, args.stdin_filepath.as_deref())
    } else {
        let content = std::fs::read_to_string(&args.input)
            .unwrap_or_else(|e| io_error(&e, Some(&args.input)));
        (content, Some(&*args.input))
    };

    let noted = Kirei::default().write(&input);

    let has_errors = noted.diagnostics.iter().any(|d| d.level == Severity::Error);
    for diagnostic in &noted.diagnostics {
        eprint!("{}", draw(diagnostic, &input, filepath));
    }

    let mut formatted = noted.value;

    if has_errors {
        std::process::exit(1);
    }

    if !formatted.ends_with('\n') {
        formatted.push('\n');
    }

    if args.check {
        if input != formatted {
            file_would_be_updated(filepath)
        }
    } else if args.list_different {
        if input != formatted {
            eprintln!("{}", filepath.unwrap_or("missing file path"));
            std::process::exit(1);
        }
    } else if args.write
        && let Some(path) = filepath
    {
        std::fs::write(path, &formatted).unwrap_or_else(|e| io_error(&e, Some(path)));
    } else {
        print!("{formatted}");
    }
}

// Diagnostics
fn io_error(error: &std::io::Error, filepath: Option<&str>) -> ! {
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
    eprintln!("{}", draw(&diagnostic, "", filepath));
    std::process::exit(1);
}

fn file_would_be_updated(filepath: Option<&str>) -> ! {
    let diagnostic = Diagnostic::error("file would be formatted");
    eprintln!("{}", draw(&diagnostic, "", filepath));
    std::process::exit(1);
}
