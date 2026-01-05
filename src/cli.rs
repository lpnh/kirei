use clap::{Parser, ValueEnum};
use std::io::{self, IsTerminal};

use crate::session::SessionMode;

#[derive(Parser)]
#[command(name = "kirei", about, version)]
#[command(arg_required_else_help = true)]
pub struct Args {
    /// File or directory to format. Use `-` to read from standard input
    #[arg(value_name = "PATH")]
    pub input: Vec<String>,

    /// Check if files are formatted
    #[arg(short = 'c', long = "check")]
    pub check: bool,

    /// Edit files in-place (overwrite with formatted output)
    #[arg(short = 'w', long = "write")]
    pub write: bool,

    /// Print the names of files that would be changed
    #[arg(short = 'l', long = "list-different")]
    pub list_different: bool,

    /// When set, ignore files such as .gitignore and .ignore will not be respected
    #[arg(long = "no-ignore")]
    pub no_ignore: bool,

    /// Specify when to use colored output
    #[arg(long = "color", value_name = "WHEN", default_value = "auto")]
    pub color: ColorWhen,

    /// When reading from stdin, use this as the filepath
    #[arg(
        long = "stdin-filepath",
        value_name = "PATH",
        help_heading = "Other options"
    )]
    pub stdin_filepath: Option<String>,
}

#[derive(Default, Copy, Clone, Debug, PartialEq, Eq, ValueEnum)]
pub enum ColorWhen {
    #[default]
    Auto,
    Never,
    Always,
}

impl ColorWhen {
    pub fn use_color(self) -> bool {
        match self {
            Self::Auto => io::stderr().is_terminal(),
            Self::Never => false,
            Self::Always => true,
        }
    }
}

impl From<&Args> for SessionMode {
    fn from(args: &Args) -> Self {
        if args.check {
            SessionMode::Check
        } else if args.list_different {
            SessionMode::ListDifferent
        } else if args.write {
            SessionMode::Write
        } else {
            SessionMode::Stdout
        }
    }
}
