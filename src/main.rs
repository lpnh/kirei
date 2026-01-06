use clap::Parser;
use globset::Glob;
use ignore::{DirEntry, WalkBuilder};
use std::{
    io::Read,
    path::{Path, PathBuf},
    process::ExitCode,
};

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
    let mut mode = SessionMode::from(args);

    let use_color = args.color.use_color();

    if is_stdin_mode {
        let filepath = args.stdin_filepath.as_deref().unwrap_or("<stdin>");
        if matches!(mode, SessionMode::Write) {
            mode = SessionMode::Stdout;
        }

        let mut source = String::new();
        std::io::stdin().read_to_string(&mut source)?;

        session.format_and_print(&mode, PathBuf::from(filepath), &source);
    } else {
        let mut paths = Vec::new();
        for pattern in &args.input {
            paths.extend(resolve_pattern(pattern, args.no_ignore)?);
        }
        for path in &paths {
            let source = std::fs::read_to_string(path)?;
            session.format_and_print(&mode, path.clone(), &source);
        }
    }

    session.print_diagnostics(use_color);

    if ((args.check || args.list_different) && session.requires_formatting)
        || !session.notes.errors.is_empty()
    {
        return Ok(ExitCode::FAILURE);
    }

    Ok(ExitCode::SUCCESS)
}

fn resolve_pattern(pattern: &str, no_ignore: bool) -> Result<Vec<PathBuf>, ErrorKind> {
    let path = PathBuf::from(pattern);

    if pattern.contains(['*', '?', '[']) {
        return try_glob(pattern, no_ignore);
    }

    if path.exists() {
        if path.is_file() {
            if no_ignore || !is_ignored(&path) {
                return Ok(vec![path]);
            }
            return Ok(vec![]);
        } else if path.is_dir() {
            let paths = walk_files(&path, no_ignore, Some("html"));
            if paths.is_empty() {
                Err(ErrorKind::NoHtmlFiles {
                    path: pattern.to_string(),
                })?;
            }
            return Ok(paths);
        }

        Err(ErrorKind::NotAFile {
            path: pattern.to_string(),
        })?;
    }

    Err(ErrorKind::FileNotFound {
        path: pattern.to_string(),
    })
}

fn try_glob(pattern: &str, no_ignore: bool) -> Result<Vec<PathBuf>, ErrorKind> {
    let glob = Glob::new(pattern)?;

    let matcher = glob.compile_matcher();
    let max_depth = if pattern.contains("**") {
        None
    } else {
        Some(1)
    };

    let paths: Vec<PathBuf> = WalkBuilder::new(".")
        .standard_filters(!no_ignore)
        .max_depth(max_depth)
        .build()
        .filter_map(Result::ok)
        .filter(|e| e.file_type().is_some_and(|ft| ft.is_file()))
        .filter(|e| {
            matcher.is_match(e.path())
                || e.file_name().to_str().is_some_and(|n| matcher.is_match(n))
        })
        .map(DirEntry::into_path)
        .collect();

    if paths.is_empty() {
        Err(ErrorKind::NoMatches {
            pattern: pattern.to_string(),
        })?;
    }
    Ok(paths)
}

fn is_ignored(path: &Path) -> bool {
    let parent = path.parent().unwrap_or_else(|| Path::new("."));
    let Ok(canonical) = path.canonicalize() else {
        return false;
    };

    !WalkBuilder::new(parent)
        .standard_filters(true)
        .max_depth(Some(1))
        .build()
        .filter_map(Result::ok)
        .any(|e| e.path().canonicalize().ok() == Some(canonical.clone()))
}

fn walk_files(root: &Path, no_ignore: bool, extension: Option<&str>) -> Vec<PathBuf> {
    WalkBuilder::new(root)
        .standard_filters(!no_ignore)
        .build()
        .filter_map(Result::ok)
        .filter(|e| e.file_type().is_some_and(|ft| ft.is_file()))
        .filter(|e| {
            extension.is_none_or(|ext| {
                e.path()
                    .extension()
                    .and_then(|e| e.to_str())
                    .is_some_and(|e| e == ext)
            })
        })
        .map(DirEntry::into_path)
        .collect()
}
