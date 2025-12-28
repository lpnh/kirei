use crate::diagnostics::Diagnostic;

pub struct Noted<T> {
    pub value: T,
    pub diagnostics: Vec<Diagnostic>,
}

impl<T> Noted<T> {
    pub fn new(value: T) -> Self {
        Self {
            value,
            diagnostics: Vec::new(),
        }
    }

    pub fn with_diagnostics(value: T, diagnostics: Vec<Diagnostic>) -> Self {
        Self { value, diagnostics }
    }
}
