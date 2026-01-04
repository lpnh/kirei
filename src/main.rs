use std::process::ExitCode;

fn main() -> miette::Result<ExitCode> {
    kirei::cli::run()
}
