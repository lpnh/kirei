use anyhow::{Context, Result};
use clap::Parser;
use std::io::{self, Read};

mod cli;
use cli::Args;

use kirei::AskamaFormatter;

fn main() -> Result<()> {
    let args = Args::parse();
    run(&args)
}

fn run(args: &Args) -> Result<()> {
    let (input, maybe_filepath) = get_input_and_filepath(args)?;
    let mut kirei = AskamaFormatter::default();
    let raw_formatted = kirei.format(&input)?;

    // Provide end of line by default
    let formatted = if raw_formatted.ends_with('\n') {
        raw_formatted
    } else {
        format!("{}\n", raw_formatted)
    };

    // Handle --check and --list-different
    if args.check || args.list_different {
        if input == formatted {
            std::process::exit(0);
        } else {
            if args.list_different {
                eprintln!(
                    "{}",
                    maybe_filepath.unwrap_or_else(|| "<stdin>".to_string())
                );
            } else {
                eprintln!("File needs formatting");
            }
            std::process::exit(1);
        }
    }

    // Handle --write
    if args.write {
        if let Some(path) = maybe_filepath {
            std::fs::write(path, formatted)?;
        } else {
            print!("{}", formatted);
        }
        return Ok(());
    }

    // Default: print to stdout
    print!("{}", formatted);
    Ok(())
}

fn get_input_and_filepath(args: &Args) -> Result<(String, Option<String>)> {
    if args.input != "-" {
        let content = std::fs::read_to_string(&args.input)
            .with_context(|| format!("Failed to read file '{}'", args.input))?;
        return Ok((content, Some(args.input.clone())));
    }

    // Reading from stdin
    let mut buffer = String::new();
    io::stdin()
        .read_to_string(&mut buffer)
        .context("Failed to read from stdin")?;

    let filepath = args.stdin_filepath.clone();
    Ok((buffer, filepath))
}
