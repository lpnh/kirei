use clap::Parser;
use ignore::{DirEntry, WalkBuilder};
use std::{io::Read, path::PathBuf, process::ExitCode};

use kirei::{
    ErrorKind,
    cli::Args,
    session::{Session, SessionMode},
};

fn main() -> ExitCode {
    let args = Args::parse();

    match run(&args) {
        Ok(code) => code,
        Err(e) => {
            eprint!("{}", Session::render_diagnostic(&e, args.color.use_color()));
            ExitCode::FAILURE
        }
    }
}

fn run(args: &Args) -> Result<ExitCode, ErrorKind> {
    let is_stdin_mode = args.input.len() == 1 && args.input[0] == "-";
    if !is_stdin_mode && args.stdin_filepath.is_some() {
        Err(ErrorKind::StdinFilepathWithoutStdin)?;
    }

    let mut session = Session::default();
    session.with_colors(args.color.use_color());

    let mut mode = SessionMode::from(args);

    if is_stdin_mode {
        let filepath = args.stdin_filepath.as_deref().unwrap_or("<stdin>");
        if matches!(mode, SessionMode::Write) {
            mode = SessionMode::Stdout;
        }

        let mut source = String::new();
        std::io::stdin().read_to_string(&mut source)?;

        session.format_and_print(&mode, PathBuf::from(filepath), &source);
    } else {
        for pattern in &args.input {
            let p = PathBuf::from(pattern);
            if !p.exists() {
                return Err(ErrorKind::FileNotFound {
                    path: pattern.clone(),
                });
            }
            if !p.is_file() && !p.is_dir() {
                return Err(ErrorKind::NotAFile {
                    path: pattern.clone(),
                });
            }
        }

        let mut builder = WalkBuilder::new(&args.input[0]);
        for pattern in &args.input[1..] {
            builder.add(pattern);
        }

        builder.standard_filters(!args.no_ignore);

        let paths: Vec<PathBuf> = builder
            .build()
            .filter_map(Result::ok)
            .filter(|e| e.file_type().is_some_and(|ft| ft.is_file()))
            .filter(|e| {
                e.path()
                    .extension()
                    .and_then(|ext| ext.to_str())
                    .is_some_and(|ext| ext == "html")
            })
            .map(DirEntry::into_path)
            .collect();

        for path in &paths {
            let source = std::fs::read_to_string(path)?;
            session.format_and_print(&mode, path.clone(), &source);
        }
    }

    if ((args.check || args.list_different) && session.requires_formatting) || session.had_errors {
        return Ok(ExitCode::FAILURE);
    }

    Ok(ExitCode::SUCCESS)
}
