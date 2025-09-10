#![allow(clippy::uninlined_format_args)]

mod config;
mod extraction;
pub mod formatter;
mod layout_engine;
pub mod types;

pub use crate::formatter::AskamaFormatter;
