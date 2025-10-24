#![allow(clippy::uninlined_format_args)]

pub mod askama;
mod config;
pub mod formatter;
mod html;
mod sakura_tree;
mod wire;
mod woodcut;

pub use crate::formatter::AskamaFormatter;

pub fn normalize_whitespace(text: &str) -> String {
    text.split_whitespace().collect::<Vec<_>>().join(" ")
}
