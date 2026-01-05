use clap::Parser;
use globset::Glob;
use ignore::{DirEntry, WalkBuilder};
use std::{
    fs,
    io::{self, Read},
    path::{Path, PathBuf},
    process::ExitCode,
};

use kirei::{KireiError, Session, SessionMode, cli::Args};

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

fn run(args: &Args) -> Result<ExitCode, KireiError> {
    let is_stdin_mode = args.input.len() == 1 && args.input[0] == "-";
    if !is_stdin_mode && args.stdin_filepath.is_some() {
        return Err(KireiError::StdinFilepathWithoutStdin);
    }

    let mut session = Session::default();
    let mut mode = SessionMode::from(args);

    let use_color = args.color.use_color();

    if is_stdin_mode {
        let filepath = args.stdin_filepath.as_deref().unwrap_or("<stdin>");
        if matches!(mode, SessionMode::Write) {
            mode = SessionMode::Stdout;
        }
        read_stdin(&mut session, &mode, filepath)?;
    } else {
        let paths = resolve_paths(&args.input, args.no_ignore)?;

        for path in &paths {
            if let Err(e) = read_file(&mut session, &mode, path) {
                session.add_error(path.clone(), e);
            }
        }
    }

    session.print_diagnostics(use_color);

    let code = if ((args.check || args.list_different) && session.requires_formatting())
        || session.has_errors()
    {
        ExitCode::FAILURE
    } else {
        ExitCode::SUCCESS
    };

    Ok(code)
}

fn read_stdin(session: &mut Session, mode: &SessionMode, filepath: &str) -> Result<(), KireiError> {
    let mut source = String::new();
    io::stdin()
        .read_to_string(&mut source)
        .map_err(|source| KireiError::StdinFailed { source })?;

    session.format_and_print(mode, PathBuf::from(filepath), source)
}

fn read_file(session: &mut Session, mode: &SessionMode, path: &Path) -> Result<(), KireiError> {
    let source = fs::read_to_string(path).map_err(|source| KireiError::ReadFailed {
        path: path.display().to_string(),
        source,
    })?;

    session.format_and_print(mode, path.to_path_buf(), source)
}

fn resolve_paths(patterns: &[String], no_ignore: bool) -> Result<Vec<PathBuf>, KireiError> {
    let mut all_paths = Vec::new();
    for pattern in patterns {
        all_paths.extend(resolve_pattern(pattern, no_ignore)?);
    }
    Ok(all_paths)
}

fn resolve_pattern(pattern: &str, no_ignore: bool) -> Result<Vec<PathBuf>, KireiError> {
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
                return Err(KireiError::NoHtmlFiles {
                    path: pattern.to_string(),
                });
            }
            return Ok(paths);
        }

        return Err(KireiError::NotAFile {
            path: pattern.to_string(),
        });
    }

    Err(KireiError::PathNotFound {
        path: pattern.to_string(),
    })
}

fn try_glob(pattern: &str, no_ignore: bool) -> Result<Vec<PathBuf>, KireiError> {
    let glob = Glob::new(pattern).map_err(|source| KireiError::InvalidGlobPattern {
        pattern: pattern.to_string(),
        source,
    })?;

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
        return Err(KireiError::NoMatches {
            pattern: pattern.to_string(),
        });
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
