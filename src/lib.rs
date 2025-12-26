#![allow(clippy::uninlined_format_args)]

pub mod askama;
mod config;
pub mod draw;
pub mod error;
mod html;
mod sakura_tree;
pub mod write;

pub use crate::{
    error::{OrDraw, OrMsg},
    write::Kirei,
};

#[must_use]
pub fn normalize_ws(text: &str) -> String {
    text.split_whitespace().collect::<Vec<_>>().join(" ")
}
