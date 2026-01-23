#[derive(Debug, Clone)]
pub struct Config {
    pub indent_size: usize,
    pub max_width: usize,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            indent_size: 4,
            max_width: 80,
        }
    }
}

// Not much to see right now...
// Configuration will be handled here in the future
