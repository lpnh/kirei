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
pub(crate) const EXPR_OPEN_TRIM: &str = "{{-";
pub(crate) const EXPR_CLOSE_TRIM: &str = "-}}";
pub(crate) const CTRL_OPEN: &str = "{%";
pub(crate) const CTRL_CLOSE: &str = "%}";
pub(crate) const CTRL_OPEN_TRIM: &str = "{%-";
pub(crate) const CTRL_CLOSE_TRIM: &str = "-%}";
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
        trim_left: bool,
        trim_right: bool,
        tag_type: ControlTag,
    },
    Expression {
        inner: String,
        trim_left: bool,
        trim_right: bool,
    },
    Comment {
        inner: String,
    },
}

impl fmt::Display for AskamaNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            AskamaNode::Control {
                inner,
                trim_left,
                trim_right,
                ..
            } => {
                let open = if *trim_left {
                    CTRL_OPEN_TRIM
                } else {
                    CTRL_OPEN
                };
                let close = if *trim_right {
                    CTRL_CLOSE_TRIM
                } else {
                    CTRL_CLOSE
                };
                let formatted_inner = inner.split_whitespace().collect::<Vec<_>>().join(" ");
                write!(f, "{} {} {}", open, formatted_inner, close)
            }
            AskamaNode::Expression {
                inner,
                trim_left,
                trim_right,
            } => {
                // Choose the right delimiters based on trimming flags
                let open = if *trim_left {
                    EXPR_OPEN_TRIM
                } else {
                    EXPR_OPEN
                };
                let close = if *trim_right {
                    EXPR_CLOSE_TRIM
                } else {
                    EXPR_CLOSE
                };
                // Normalize whitespace inside the expression
                let formatted_inner = inner.split_whitespace().collect::<Vec<_>>().join(" ");
                write!(f, "{} {} {}", open, formatted_inner, close)
            }
            AskamaNode::Comment { inner } => {
                let formatted_inner = inner.split_whitespace().collect::<Vec<_>>().join(" ");
                write!(f, "{} {} {}", COMMENT_OPEN, formatted_inner, COMMENT_CLOSE)
            }
        }
    }
}
