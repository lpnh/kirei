#[derive(Debug, Clone)]
pub(crate) struct Config {
    pub(crate) indent_size: usize,
    pub(crate) max_line_length: usize,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            indent_size: 4,
            max_line_length: 80,
        }
    }
}

// Not much to see right now...
// Configuration will be handled here in the future
