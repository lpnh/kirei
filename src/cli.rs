use clap::Parser;

#[derive(Parser)]
#[command(name = "kirei", about, version)]
#[command(arg_required_else_help = true)]
pub struct Args {
    /// Path of file to format
    #[arg(value_name = "PATH", help_heading = "Input")]
    pub input: String,

    /// Check if files are formatted
    #[arg(short = 'c', long = "check")]
    pub check: bool,

    /// Edit files in-place (overwrite with formatted output)
    #[arg(short = 'w', long = "write")]
    pub write: bool,

    /// Print the names of files that would be changed
    #[arg(short = 'l', long = "list-different")]
    pub list_different: bool,

    /// When reading from stdin, use this as the filepath
    #[arg(
        long = "stdin-filepath",
        value_name = "PATH",
        help_heading = "Other options"
    )]
    pub stdin_filepath: Option<String>,
}
