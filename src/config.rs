#[derive(Debug, Clone)]
pub(crate) struct Config {
    pub indent_size: usize,
    pub max_line_length: usize,
    pub comment_threshold: usize,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            indent_size: 4,
            max_line_length: 80,
            comment_threshold: 100,
        }
    }
}

// Not much to see right now...
// Configuration will be handled here in the future
