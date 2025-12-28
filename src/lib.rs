#![allow(clippy::uninlined_format_args)]
#![allow(clippy::must_use_candidate)]

pub mod askama;
mod check;
mod config;
pub mod diagnostics;
pub mod draw;
mod html;
mod sakura_tree;
pub mod write;

pub use crate::{draw::Severity, write::Kirei};

#[must_use]
pub fn normalize_ws(text: &str) -> String {
    text.split_whitespace().collect::<Vec<_>>().join(" ")
}
