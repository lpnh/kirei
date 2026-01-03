use clap::Parser;
use globset::Glob;
use ignore::{DirEntry, WalkBuilder};
use std::{
    io::{self, Read},
    path::PathBuf,
};

use crate::{diagnostics::Diagnostic, draw::eprint_diagnostic, noted::Noted, write::Kirei};

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

    /// When reading from stdin, use this as the filepath
    #[arg(
        long = "stdin-filepath",
        value_name = "PATH",
        help_heading = "Other options"
    )]
    stdin_filepath: Option<String>,
}

struct ProcessedFile {
    path: PathBuf,
    source: String,
    formatted: String,
}

impl ProcessedFile {
    fn needs_formatting(&self) -> bool {
        self.source != self.formatted
    }
}

fn walk_files(root: &PathBuf, no_ignore: bool, extension: Option<&str>) -> Vec<PathBuf> {
    WalkBuilder::new(root)
        .standard_filters(!no_ignore)
        .build()
        .filter_map(Result::ok)
        .filter(|e| e.file_type().is_some_and(|ft| ft.is_file()))
        .filter(|e| {
            extension.map_or(true, |ext| {
                e.path()
                    .extension()
                    .and_then(|e| e.to_str())
                    .is_some_and(|e| e == ext)
            })
        })
        .map(DirEntry::into_path)
        .collect()
}

fn try_glob(pattern: &str, no_ignore: bool) -> Result<Vec<PathBuf>, String> {
    let glob = Glob::new(pattern).map_err(|e| e.to_string())?;
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
        Err(format!("no matches for pattern `{}`", pattern))
    } else {
        Ok(paths)
    }
}

fn is_ignored(path: &PathBuf) -> bool {
    let parent = path.parent().unwrap_or_else(|| std::path::Path::new("."));
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

fn resolve_single_pattern(pattern: &str, no_ignore: bool) -> Result<Vec<PathBuf>, Diagnostic> {
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
                Err(Diagnostic::no_html_files_in_directory(pattern))
            } else {
                Ok(paths)
            }
        } else {
            Err(Diagnostic::not_a_regular_file(pattern))
        };
    }

    if pattern.contains('*') || pattern.contains('?') || pattern.contains('[') {
        try_glob(pattern, no_ignore).map_err(|msg| Diagnostic::error(msg))
    } else {
        Err(Diagnostic::path_does_not_exist(pattern))
    }
}

fn resolve_paths(patterns: &[String], no_ignore: bool) -> Result<Vec<PathBuf>, Diagnostic> {
    let mut all_paths = Vec::new();

    for pattern in patterns {
        all_paths.extend(resolve_single_pattern(pattern, no_ignore)?);
    }

    Ok(all_paths)
}

fn process_file(path: PathBuf, kirei: &mut Kirei) -> Noted<ProcessedFile> {
    let source = match std::fs::read_to_string(&path) {
        Ok(content) => content,
        Err(e) => {
            let diagnostic = Diagnostic::from_io(&e, &path.display().to_string());
            return Noted::with_diagnostics(
                ProcessedFile {
                    path,
                    source: String::new(),
                    formatted: String::new(),
                },
                vec![diagnostic],
            );
        }
    };

    let noted = kirei.write(&source);
    let mut formatted = noted.value;

    if !formatted.is_empty() && !formatted.ends_with('\n') {
        formatted.push('\n');
    }

    Noted::with_diagnostics(
        ProcessedFile {
            path,
            source,
            formatted,
        },
        noted.diagnostics,
    )
}

fn handle_results<F>(results: &[Noted<ProcessedFile>], mut action: F) -> i32
where
    F: FnMut(&ProcessedFile) -> Option<i32>,
{
    let mut exit_code = 0;

    for noted in results {
        for diagnostic in &noted.diagnostics {
            eprint_diagnostic(diagnostic, &noted.value.source, noted.value.path.to_str());
        }

        if noted.has_errors() {
            exit_code = 1;
        } else if let Some(code) = action(&noted.value) {
            exit_code = code;
        }
    }

    exit_code
}

fn run_stdin(args: &Args) -> i32 {
    let mut buffer = String::new();
    if let Err(e) = io::stdin().read_to_string(&mut buffer) {
        eprint_diagnostic(&Diagnostic::from_io(&e, "stdin"), "", None);
        return 1;
    }

    let filepath = args.stdin_filepath.as_deref();
    let noted = Kirei::default().write(&buffer);

    for diagnostic in &noted.diagnostics {
        eprint_diagnostic(diagnostic, &buffer, filepath);
    }

    if noted.has_errors() {
        return 1;
    }

    let mut formatted = noted.value;
    if !formatted.ends_with('\n') {
        formatted.push('\n');
    }

    if buffer == formatted {
        return 0;
    }

    if args.check {
        eprint_diagnostic(&Diagnostic::file_would_be_formatted(), "", filepath);
        1
    } else if args.list_different {
        eprintln!("{}", filepath.unwrap_or("missing file path"));
        1
    } else {
        print!("{}", formatted);
        0
    }
}

pub fn run() -> i32 {
    let args = Args::parse();

    if args.input.len() == 1 && args.input[0] == "-" {
        return run_stdin(&args);
    }

    if args.stdin_filepath.is_some() {
        eprint_diagnostic(&Diagnostic::stdin_filepath_requires_stdin(), "", None);
        return 1;
    }

    let paths = match resolve_paths(&args.input, args.no_ignore) {
        Ok(paths) => paths,
        Err(diagnostic) => {
            eprint_diagnostic(&diagnostic, "", None);
            return 1;
        }
    };

    let mut kirei = Kirei::default();
    let results: Vec<_> = paths
        .into_iter()
        .map(|p| process_file(p, &mut kirei))
        .collect();

    if args.check {
        handle_results(&results, |file| {
            file.needs_formatting().then(|| {
                eprint_diagnostic(
                    &Diagnostic::file_would_be_formatted(),
                    "",
                    file.path.to_str(),
                );
                1
            })
        })
    } else if args.list_different {
        handle_results(&results, |file| {
            file.needs_formatting().then(|| {
                eprintln!("{}", file.path.display());
                1
            })
        })
    } else if args.write {
        handle_results(&results, |file| {
            std::fs::write(&file.path, &file.formatted).err().map(|e| {
                eprint_diagnostic(
                    &Diagnostic::from_io(&e, &file.path.display().to_string()),
                    "",
                    file.path.to_str(),
                );
                1
            })
        })
    } else {
        handle_results(&results, |file| {
            print!("{}", file.formatted);
            None
        })
    }
}
