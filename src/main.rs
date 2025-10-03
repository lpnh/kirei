use clap::Parser;
use std::io::{self, Read};

mod cli;
use cli::Args;

use kirei::AskamaFormatter;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();
    run(&args)
}

fn run(args: &Args) -> Result<(), Box<dyn std::error::Error>> {
    let (input, maybe_filepath) = get_input_and_filepath(args)?;
    let mut kirei = AskamaFormatter::default();
    let raw_formatted = kirei.format(&input);

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

fn get_input_and_filepath(
    args: &Args,
) -> Result<(String, Option<String>), Box<dyn std::error::Error>> {
    if args.input != "-" {
        let content = std::fs::read_to_string(&args.input)?;
        return Ok((content, Some(args.input.clone())));
    }

    // Reading from stdin
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer)?;

    let filepath = args.stdin_filepath.clone();
    Ok((buffer, filepath))
}
