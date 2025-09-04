use std::fmt;

// Our internal placeholder tokens
pub(crate) const ASKAMA_TOKEN: &str = "__ASKAMA_";
pub(crate) const ASKAMA_EXPR_TOKEN: &str = "__ASKAMA_EXPR_";
pub(crate) const ASKAMA_CTRL_TOKEN: &str = "__ASKAMA_CTRL_";
pub(crate) const ASKAMA_COMMENT_TOKEN: &str = "__ASKAMA_COMMENT_";
pub(crate) const ASKAMA_END_TOKEN: &str = "_ASKAMA_END__";

// Askama template syntax delimiters
pub(crate) const EXPR_OPEN: &str = "{{";
pub(crate) const EXPR_CLOSE: &str = "}}";
pub(crate) const CTRL_OPEN: &str = "{%";
pub(crate) const CTRL_CLOSE: &str = "%}";
pub(crate) const COMMENT_OPEN: &str = "{#";
pub(crate) const COMMENT_CLOSE: &str = "#}";

#[derive(Debug, Clone)]
pub(crate) enum ControlTag {
    Open,   // Tags that start blocks: if, for, block, etc.
    Middle, // Tags that continue blocks: else, else if, when
    Close,  // Tags that end blocks: endif, endfor, endblock, etc.
    Other,  // Everything else: set, include, etc.
}

#[derive(Debug, Clone)]
pub(crate) enum AskamaNode {
    Control {
        inner: String,
        open_delimiter: String,
        close_delimiter: String,
        tag_type: ControlTag,
    },
    Expression {
        inner: String,
        open_delimiter: String,
        close_delimiter: String,
    },
    Comment {
        inner: String,
        open_delimiter: String,
        close_delimiter: String,
    },
}

impl fmt::Display for AskamaNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            AskamaNode::Control {
                inner,
                open_delimiter,
                close_delimiter,
                ..
            } => {
                let formatted_inner = inner.split_whitespace().collect::<Vec<_>>().join(" ");
                write!(
                    f,
                    "{} {} {}",
                    open_delimiter, formatted_inner, close_delimiter
                )
            }
            AskamaNode::Expression {
                inner,
                open_delimiter,
                close_delimiter,
            } => {
                // Normalize whitespace inside the expression
                let formatted_inner = inner.split_whitespace().collect::<Vec<_>>().join(" ");
                write!(
                    f,
                    "{} {} {}",
                    open_delimiter, formatted_inner, close_delimiter
                )
            }
            AskamaNode::Comment {
                inner,
                open_delimiter,
                close_delimiter,
            } => {
                let formatted_inner = inner.split_whitespace().collect::<Vec<_>>().join(" ");
                write!(
                    f,
                    "{} {} {}",
                    open_delimiter, formatted_inner, close_delimiter
                )
            }
        }
    }
}
