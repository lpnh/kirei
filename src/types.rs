use std::fmt;

// Someone said this helps with the performance
pub(crate) const TOKEN_PREFIXES: &[&str] =
    &[ASKAMA_CTRL_TOKEN, ASKAMA_EXPR_TOKEN, ASKAMA_COMMENT_TOKEN];

// Our internal placeholder tokens
pub(crate) const ASKAMA_TOKEN: &str = "__ASKAMA_";
pub(crate) const ASKAMA_CTRL_TOKEN: &str = "__ASKAMA_CTRL_";
pub(crate) const ASKAMA_EXPR_TOKEN: &str = "__ASKAMA_EXPR_";
pub(crate) const ASKAMA_COMMENT_TOKEN: &str = "__ASKAMA_COMMENT_";
pub(crate) const ASKAMA_END_TOKEN: &str = "_ASKAMA_END__";

// Askama template syntax delimiters
pub(crate) const CTRL_OPEN: &str = "{%";
pub(crate) const CTRL_CLOSE: &str = "%}";
pub(crate) const EXPR_OPEN: &str = "{{";
pub(crate) const EXPR_CLOSE: &str = "}}";
pub(crate) const COMMENT_OPEN: &str = "{#";
pub(crate) const COMMENT_CLOSE: &str = "#}";

#[derive(Debug, Clone, Copy)]
pub(crate) struct Indentation(pub i8, pub i8); // (before, after)

#[derive(Debug, Clone, Copy)]
pub(crate) enum BlockKind {
    Open,   // Tags that start blocks: if, for, block, etc.
    Clause, // Tags that continue blocks at same indent: else, else if
    Inner,  // Tags that continue blocks with new indent: when
    Close,  // Tags that end blocks: endif, endfor, endblock, etc.
}

impl BlockKind {
    pub fn indentation(&self) -> Indentation {
        match self {
            BlockKind::Open => Indentation(0, 1),
            BlockKind::Inner => Indentation(0, 1),
            BlockKind::Clause | BlockKind::Close => Indentation(-1, 0),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum Style {
    Inline,
    Block(BlockKind),
}

impl Style {
    pub fn indentation(&self) -> Indentation {
        match self {
            Style::Inline => Indentation(0, 0),
            Style::Block(kind) => kind.indentation(),
        }
    }
}

impl Style {
    // Get get Style from Node kind
    pub fn try_from_node(child: tree_sitter::Node<'_>) -> Option<Self> {
        let grand_child = child.child(1)?;

        match grand_child.kind() {
            "if_statement" | "for_statement" | "block_statement" | "filter_statement"
            | "match_statement" | "macro_statement" | "call_statement" => {
                Some(Style::Block(BlockKind::Open))
            }

            "else_statement" | "else_if_statement" => Some(Style::Block(BlockKind::Clause)),

            "when_statement" => Some(Style::Block(BlockKind::Inner)),

            "endif_statement"
            | "endfor_statement"
            | "endblock_statement"
            | "endfilter_statement"
            | "endmatch_statement"
            | "endmacro_statement"
            | "endcall_statement" => Some(Style::Block(BlockKind::Close)),

            "macro_call_statement"
            | "let_statement"
            | "extends_statement"
            | "include_statement"
            | "import_statement" => Some(Style::Inline),

            unknown => {
                eprintln!(
                    "Unknown statement type '{}' in node '{}'",
                    unknown,
                    child.kind()
                );
                // return Some(Style::Inline);
                None
            }
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) enum AskamaNode {
    Control {
        inner: String,
        open_delimiter: String,
        close_delimiter: String,
        style: Style,
    },
    Expression {
        inner: String,
        open_delimiter: String,
        close_delimiter: String,
        style: Style,
    },
    Comment {
        inner: String,
        open_delimiter: String,
        close_delimiter: String,
        style: Style,
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
            }
            | AskamaNode::Expression {
                inner,
                open_delimiter,
                close_delimiter,
                ..
            }
            | AskamaNode::Comment {
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
        }
    }
}
