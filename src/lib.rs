#![allow(clippy::uninlined_format_args)]
#![allow(clippy::must_use_candidate)]

mod askama;
mod check;
mod cli;
mod config;
mod diagnostics;
mod draw;
mod html;
mod noted;
mod sakura_tree;
mod write;

pub use crate::{cli::run, diagnostics::Severity, draw::draw, write::Kirei};

pub fn normalize_ws(text: &str) -> String {
    text.split_whitespace().collect::<Vec<_>>().join(" ")
}
