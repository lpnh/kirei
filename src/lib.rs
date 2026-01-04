#![allow(clippy::uninlined_format_args)]
#![allow(clippy::must_use_candidate)]

mod askama;
pub mod cli;
mod config;
mod diagnostics;
mod html;
mod sakura_tree;
mod write;

pub use crate::{diagnostics::diagnostics_from_noted, write::Kirei};

pub fn normalize_ws(text: &str) -> String {
    text.split_whitespace().collect::<Vec<_>>().join(" ")
}
