use clap::Parser;
use std::io::{self, Read};

mod cli;
use cli::Args;
use kirei::{Kirei, OrDraw, error::KireiError};

fn main() {
    let args = Args::parse();

    let (input, filepath) = if args.input == "-" {
        let mut buffer = String::new();
        io::stdin().read_to_string(&mut buffer).or_draw(None, None);
        (buffer, args.stdin_filepath.as_deref())
    } else {
        let content = std::fs::read_to_string(&args.input).or_draw(None, Some(&*args.input));
        (content, Some(&*args.input))
    };

    let mut formatted = Kirei::default()
        .write(&input)
        .or_draw(Some(&input), filepath);

    if !formatted.ends_with('\n') {
        formatted.push('\n');
    }

    if args.check {
        if input != formatted {
            KireiError::draw_msg("file needs formatting");
        }
    } else if args.list_different {
        if input != formatted {
            KireiError::draw_msg(filepath.unwrap_or("missing file path"));
        }
    } else if args.write
        && let Some(path) = filepath
    {
        std::fs::write(path, &formatted).or_draw(None, Some(path));
    } else {
        print!("{formatted}");
    }
}
