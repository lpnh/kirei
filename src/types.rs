use std::fmt;
use tree_sitter::Node;

// Someone said this helps with the performance
pub(crate) const TOKEN_PREFIXES: &[&str] =
    &[ASKAMA_CTRL_TOKEN, ASKAMA_EXPR_TOKEN, ASKAMA_COMMENT_TOKEN];

// Our internal placeholder tokens
pub(crate) const ASKAMA_TOKEN: &str = "__ASKAMA_";
pub(crate) const ASKAMA_CTRL_TOKEN: &str = "__ASKAMA_CTRL_";
pub(crate) const ASKAMA_EXPR_TOKEN: &str = "__ASKAMA_EXPR_";
pub(crate) const ASKAMA_COMMENT_TOKEN: &str = "__ASKAMA_COMMENT_";
pub(crate) const ASKAMA_END_TOKEN: &str = "_ASKAMA_END__";

#[derive(Debug, Clone, Copy)]
pub(crate) struct Indentation(pub i8, pub i8); // (before, after)

#[derive(Debug, Clone, Copy)]
pub(crate) enum BlockKind {
    Open,   // Tags that start blocks: if, for, block, etc.
    Inner,  // Tags that continue blocks with new indent: when
    Clause, // Tags that continue blocks at same indent: else, else if
    Close,  // Tags that end blocks: endif, endfor, endblock, etc.
}

impl BlockKind {
    pub fn indentation(self) -> Indentation {
        match self {
            BlockKind::Open => Indentation(0, 1),
            BlockKind::Inner => Indentation(0, 0),
            BlockKind::Clause => Indentation(-1, 1),
            BlockKind::Close => Indentation(-1, 0),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum Style {
    Inline,
    Block(BlockKind),
}

impl Style {
    pub fn indentation(self) -> Indentation {
        match self {
            Style::Inline => Indentation(0, 0),
            Style::Block(kind) => kind.indentation(),
        }
    }
}

impl Style {
    // Get get Style from Node kind
    pub fn try_from_node(child: Node<'_>) -> Option<Self> {
        let grand_child = child.child(1)?;

        match grand_child.kind() {
            "if_statement"
            | "for_statement"
            | "block_statement"
            | "filter_statement"
            | "match_statement"
            | "macro_statement"
            | "call_statement"
            | "macro_call_statement" => Some(Style::Block(BlockKind::Open)),

            "else_statement" | "else_if_statement" => Some(Style::Block(BlockKind::Clause)),

            "when_statement" => Some(Style::Block(BlockKind::Inner)),

            "endif_statement"
            | "endfor_statement"
            | "endblock_statement"
            | "endfilter_statement"
            | "endmatch_statement"
            | "endmacro_statement"
            | "endcall_statement" => Some(Style::Block(BlockKind::Close)),

            "let_statement" | "extends_statement" | "include_statement" | "import_statement" => {
                Some(Style::Inline)
            }

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

impl AskamaNode {
    // Create an AskamaNode from a tree-sitter node and source text
    pub fn from_node(child: Node<'_>, source: &str) -> Option<Self> {
        let (open_delimiter, close_delimiter, inner) = Self::split_node(child, source)?;

        match child.kind() {
            "control_tag" => {
                let style = Style::try_from_node(child)?;
                Some(AskamaNode::Control {
                    inner,
                    open_delimiter,
                    close_delimiter,
                    style,
                })
            }
            "render_expression" => Some(AskamaNode::Expression {
                inner,
                open_delimiter,
                close_delimiter,
                style: Style::Inline,
            }),
            "comment" => Some(AskamaNode::Comment {
                inner,
                open_delimiter,
                close_delimiter,
                style: Style::Inline,
            }),
            _ => None,
        }
    }

    // Extract the open and close delimiters, and the inner content from a node
    fn split_node<'a>(node: Node<'a>, source: &'a str) -> Option<(String, String, String)> {
        if node.child_count() < 2 {
            return None;
        }

        let first_child = node.child(0)?;
        let last_child = node.child(node.child_count() - 1)?;

        // Extract delimiter text and trim any surrounding whitespace
        let open = first_child
            .utf8_text(source.as_bytes())
            .ok()?
            .trim()
            .to_string();
        let close = last_child
            .utf8_text(source.as_bytes())
            .ok()?
            .trim()
            .to_string();

        // Use byte positions to extract the content between delimiters
        let start = first_child.end_byte();
        let end = last_child.start_byte();

        let inner = if start < end {
            source[start..end].trim().to_string()
        } else {
            String::new()
        };

        // This can be a good place to handle the whitespace controls in the future
        let trimmed_inner = inner.split_whitespace().collect::<Vec<_>>().join(" ");

        Some((open, close, trimmed_inner))
    }
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
                if inner.is_empty() {
                    // No content, no whitespace
                    write!(f, "{}{}{}", open_delimiter, inner, close_delimiter)
                } else {
                    write!(f, "{} {} {}", open_delimiter, inner, close_delimiter)
                }
            }
        }
    }
}
