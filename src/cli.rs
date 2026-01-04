use clap::{Parser, ValueEnum};
use globset::Glob;
use ignore::{DirEntry, WalkBuilder};
use miette::{IntoDiagnostic, Result};
use std::{
    fs,
    io::{self, IsTerminal, Read},
    path::{Path, PathBuf},
    process::ExitCode,
};

use crate::{
    diagnostics::{KireiError, Noted, diagnostics_from_noted},
    write::Kirei,
};

#[derive(Parser)]
#[command(name = "kirei", about, version)]
#[command(arg_required_else_help = true)]
struct Args {
    /// File or directory to format. Use `-` to read from standard input
    #[arg(value_name = "PATH")]
    input: Vec<String>,

    /// Check if files are formatted
    #[arg(short = 'c', long = "check")]
    check: bool,

    /// Edit files in-place (overwrite with formatted output)
    #[arg(short = 'w', long = "write")]
    write: bool,

    /// Print the names of files that would be changed
    #[arg(short = 'l', long = "list-different")]
    list_different: bool,

    /// When set, ignore files such as .gitignore and .ignore will not be respected
    #[arg(long = "no-ignore")]
    no_ignore: bool,

    /// Specify when to use colored output
    #[arg(long = "color", value_name = "WHEN", default_value = "auto")]
    color: ColorWhen,

    /// When reading from stdin, use this as the filepath
    #[arg(
        long = "stdin-filepath",
        value_name = "PATH",
        help_heading = "Other options"
    )]
    stdin_filepath: Option<String>,
}

#[derive(Default, Copy, Clone, Debug, PartialEq, Eq, ValueEnum)]
enum ColorWhen {
    #[default]
    Auto,
    Never,
    Always,
}

impl ColorWhen {
    fn should_colorize(self) -> bool {
        match self {
            Self::Auto => io::stderr().is_terminal(),
            Self::Never => false,
            Self::Always => true,
        }
    }
}

pub fn run() -> Result<ExitCode> {
    let args = Args::parse();

    let use_color = args.color.should_colorize();
    miette::set_hook(Box::new(move |_| {
        Box::new(
            miette::MietteHandlerOpts::new()
                .color(use_color)
                .unicode(use_color)
                .build(),
        )
    }))
    .ok();

    let is_stdin = args.input.len() == 1 && args.input[0] == "-";

    if !is_stdin && args.stdin_filepath.is_some() {
        return Err(KireiError::StdinFilepathWithoutStdin).into_diagnostic();
    }

    let mut kirei = Kirei::default();

    if is_stdin {
        let mut buffer = String::new();
        io::stdin()
            .read_to_string(&mut buffer)
            .map_err(|e| KireiError::StdinFailed { source: e })
            .into_diagnostic()?;

        let filepath = args.stdin_filepath.as_deref().unwrap_or("<stdin>");
        let result = kirei.write(&buffer, filepath);

        draw_diagnostics(&result, args.color.should_colorize());

        if !result.has_errors()
            && let Some(formatted) = result.value
        {
            if buffer == formatted {
                return Ok(ExitCode::SUCCESS);
            }

            if args.check {
                eprintln!("file would be formatted");
                return Ok(ExitCode::FAILURE);
            } else if args.list_different {
                eprintln!("{}", filepath);
                return Ok(ExitCode::FAILURE);
            }

            print!("{}", formatted);
            return Ok(ExitCode::SUCCESS);
        }
        return Ok(ExitCode::FAILURE);
    }

    let paths = resolve_paths(&args.input, args.no_ignore)?;

    for path in paths {
        let filepath = path.to_str().expect("valid filepath");
        let source = fs::read_to_string(&path)
            .map_err(|source| KireiError::ReadFailed {
                path: filepath.to_string(),
                source,
            })
            .into_diagnostic()?;

        let noted_file = kirei.annotate(source, filepath);

        draw_diagnostics(&noted_file.result, args.color.should_colorize());

        if !noted_file.result.has_errors()
            && let Some(formatted) = noted_file.formatted_output()
        {
            if args.check && noted_file.needs_formatting() {
                eprintln!("{}: file would be formatted", filepath);
                return Ok(ExitCode::FAILURE);
            } else if args.list_different && noted_file.needs_formatting() {
                eprintln!("{}", filepath);
                return Ok(ExitCode::FAILURE);
            } else if args.write && noted_file.needs_formatting() {
                fs::write(&path, formatted).map_err(|source| KireiError::WriteFailed {
                    path: filepath.to_string(),
                    source,
                })?;
            } else if !args.check && !args.list_different && !args.write {
                print!("{}", formatted);
            }
        } else {
            return Ok(ExitCode::FAILURE);
        }
    }

    Ok(ExitCode::SUCCESS)
}

fn draw_diagnostics<T>(result: &Noted<T>, use_color: bool) {
    eprint!("{}", diagnostics_from_noted(result, use_color));
}

fn walk_files(root: &PathBuf, no_ignore: bool, extension: Option<&str>) -> Vec<PathBuf> {
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

fn try_glob(pattern: &str, no_ignore: bool) -> Result<Vec<PathBuf>> {
    let glob = Glob::new(pattern)
        .map_err(|e| KireiError::InvalidGlobPattern {
            pattern: pattern.to_string(),
            source: e,
        })
        .into_diagnostic()?;
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
                || e.path()
                    .file_name()
                    .and_then(|n| n.to_str())
                    .is_some_and(|n| matcher.is_match(n))
        })
        .map(DirEntry::into_path)
        .collect();

    if paths.is_empty() {
        return Err(KireiError::NoMatches {
            pattern: pattern.to_string(),
        })
        .into_diagnostic();
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

fn resolve_single_pattern(pattern: &str, no_ignore: bool) -> Result<Vec<PathBuf>> {
    let path = PathBuf::from(pattern);

    if path.exists() {
        return if path.is_file() {
            Ok(if no_ignore || !is_ignored(&path) {
                vec![path]
            } else {
                vec![]
            })
        } else if path.is_dir() {
            let paths = walk_files(&path, no_ignore, Some("html"));
            if paths.is_empty() {
                Err(KireiError::NoHtmlFiles {
                    path: pattern.to_string(),
                })
                .into_diagnostic()
            } else {
                Ok(paths)
            }
        } else {
            Err(KireiError::NotAFile {
                path: pattern.to_string(),
            })
            .into_diagnostic()
        };
    }

    if pattern.contains('*') || pattern.contains('?') || pattern.contains('[') {
        try_glob(pattern, no_ignore)
    } else {
        Err(KireiError::PathNotFound {
            path: pattern.to_string(),
        })
        .into_diagnostic()
    }
}

fn resolve_paths(patterns: &[String], no_ignore: bool) -> Result<Vec<PathBuf>> {
    let mut all_paths = Vec::new();

    for pattern in patterns {
        all_paths.extend(resolve_single_pattern(pattern, no_ignore)?);
    }

    Ok(all_paths)
}
