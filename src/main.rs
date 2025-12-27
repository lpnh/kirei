use clap::Parser;
use std::io::{self, Read};

mod cli;
use cli::Args;
use kirei::{Kirei, Severity, draw::Diagnostic};

fn main() {
    let args = Args::parse();

    let (input, filepath) = if args.input == "-" {
        let mut buffer = String::new();
        io::stdin()
            .read_to_string(&mut buffer)
            .unwrap_or_else(|e| Diagnostic::io_error(&e, None));
        (buffer, args.stdin_filepath.as_deref())
    } else {
        let content = std::fs::read_to_string(&args.input)
            .unwrap_or_else(|e| Diagnostic::io_error(&e, Some(&args.input)));
        (content, Some(&*args.input))
    };

    let (mut formatted, diagnostics) = Kirei::default().write(&input);

    let has_errors = diagnostics.iter().any(|d| d.level == Severity::Error);
    for diagnostic in &diagnostics {
        eprint!("{}", diagnostic.draw(&input, filepath));
    }

    if has_errors {
        std::process::exit(1);
    }

    if !formatted.ends_with('\n') {
        formatted.push('\n');
    }

    if args.check {
        if input != formatted {
            Diagnostic::file_would_be_updated(filepath)
        }
    } else if args.list_different {
        if input != formatted {
            eprintln!("{}", filepath.unwrap_or("missing file path"));
            std::process::exit(1);
        }
    } else if args.write
        && let Some(path) = filepath
    {
        std::fs::write(path, &formatted).unwrap_or_else(|e| Diagnostic::io_error(&e, Some(path)));
    } else {
        print!("{formatted}");
    }
}
