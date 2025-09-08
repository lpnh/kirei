use clap::Parser;
use kirei::cli::Args;
use kirei::run;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();
    run(args)
}
