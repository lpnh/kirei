use anyhow::Result;
use std::ops;
use tree_sitter::{Node, Range};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Boundary {
    Open,       // if, for, block, etc.
    Inner,      // when, else, else if, etc.
    Close,      // endif, endfor, endblock, etc.
    Standalone, // extends, import, etc.
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ControlTag {
    Block,
    Endblock,
    Filter,
    Endfilter,
    Extends,
    Include,
    Import,
    Let,
    For,
    Break,
    Continue,
    Endfor,
    If,
    ElseIf,
    Else,
    MatchElse,
    Endif,
    Match,
    Endmatch,
    When,
    Endwhen,
    Macro,
    Endmacro,
    MacroCall,
    Endcall,
    Raw,
    Endraw,
}

impl ControlTag {
    fn from_node(node: Node) -> Self {
        let child = node.child(1).unwrap();
        match child.kind() {
            "if_statement" => Self::If,
            "for_statement" => Self::For,
            "block_statement" => Self::Block,
            "filter_statement" => Self::Filter,
            "match_statement" => Self::Match,
            "macro_statement" => Self::Macro,
            "macro_call_statement" => Self::MacroCall,

            "else_statement" => {
                if is_match_arm(node) {
                    Self::MatchElse
                } else {
                    Self::Else
                }
            }
            "else_if_statement" => Self::ElseIf,
            "when_statement" => Self::When,

            "endif_statement" => Self::Endif,
            "endfor_statement" => Self::Endfor,
            "endblock_statement" => Self::Endblock,
            "endfilter_statement" => Self::Endfilter,
            "endmatch_statement" => Self::Endmatch,
            "endmacro_statement" => Self::Endmacro,
            "endcall_statement" => Self::Endcall,
            "endwhen_statement" => Self::Endwhen,

            "extends_statement" => Self::Extends,
            "include_statement" => Self::Include,
            "import_statement" => Self::Import,
            "let_statement" => Self::Let,
            "break_statement" => Self::Break,
            "continue_statement" => Self::Continue,
            _ => unreachable!(),
        }
    }

    fn boundary(self) -> Boundary {
        match self {
            Self::If
            | Self::For
            | Self::Block
            | Self::Filter
            | Self::Match
            | Self::Macro
            | Self::MacroCall
            | Self::Raw => Boundary::Open,
            Self::ElseIf | Self::Else | Self::When | Self::MatchElse => Boundary::Inner,
            Self::Endif
            | Self::Endfor
            | Self::Endblock
            | Self::Endfilter
            | Self::Endmatch
            | Self::Endwhen
            | Self::Endmacro
            | Self::Endcall
            | Self::Endraw => Boundary::Close,
            Self::Extends
            | Self::Include
            | Self::Import
            | Self::Let
            | Self::Break
            | Self::Continue => Boundary::Standalone,
        }
    }

    #[must_use]
    pub fn indent(self) -> (isize, isize) {
        match self {
            Self::Match => (0, 2),
            Self::Endmatch => (-2, 0),
            _ => match self.boundary() {
                Boundary::Open => (0, 1),
                Boundary::Inner => (-1, 1),
                Boundary::Close => (-1, 0),
                Boundary::Standalone => (0, 0),
            },
        }
    }

    fn is_opening(self) -> bool {
        matches!(self.boundary(), Boundary::Open)
    }

    #[must_use]
    pub fn is_match_arm(self) -> bool {
        matches!(self, Self::When | Self::MatchElse)
    }

    #[must_use]
    pub fn matches_close(self, close: Self) -> bool {
        matches!(
            (self, close),
            (Self::Block, Self::Endblock)
                | (Self::Filter, Self::Endfilter)
                | (Self::For, Self::Endfor)
                | (Self::If, Self::Endif)
                | (Self::Match, Self::Endmatch)
                | (Self::When, Self::Endwhen)
                | (Self::Macro, Self::Endmacro)
                | (Self::MacroCall, Self::Endcall)
                | (Self::Raw, Self::Endraw)
        )
    }
}

#[derive(Debug, Clone)]
pub struct Delimiters {
    open: String,
    close: String,
}

#[derive(Debug, Clone)]
pub enum AskamaNode {
    Control {
        dlmts: Delimiters,
        inner: String,
        tag: ControlTag,
        range: ops::Range<usize>,
        end: Option<usize>,
    },
    Expression {
        dlmts: Delimiters,
        inner: String,
        range: ops::Range<usize>,
    },
    Comment {
        dlmts: Delimiters,
        inner: String,
        range: ops::Range<usize>,
    },
}

impl AskamaNode {
    #[must_use]
    pub fn start(&self) -> usize {
        match self {
            Self::Control { range, .. }
            | Self::Expression { range, .. }
            | Self::Comment { range, .. } => range.start,
        }
    }

    #[must_use]
    pub fn end(&self) -> usize {
        match self {
            Self::Control { range, .. }
            | Self::Expression { range, .. }
            | Self::Comment { range, .. } => range.end,
        }
    }

    fn delimiters(&self) -> (&str, &str) {
        match self {
            Self::Control { dlmts, .. }
            | Self::Expression { dlmts, .. }
            | Self::Comment { dlmts, .. } => (&dlmts.open, &dlmts.close),
        }
    }

    fn inner(&self) -> &str {
        match self {
            Self::Control { inner, .. }
            | Self::Expression { inner, .. }
            | Self::Comment { inner, .. } => inner,
        }
    }

    fn is_expr(&self) -> bool {
        matches!(self, Self::Expression { .. })
    }
}

pub fn extract_askama_nodes(root: &Node, source: &str) -> Result<(Vec<AskamaNode>, Vec<Range>)> {
    let mut nodes = Vec::new();
    let mut content_node_ranges = Vec::new();
    let mut stack: Vec<(usize, ControlTag)> = Vec::new();

    let mut cursor = root.walk();
    for child in root.children(&mut cursor) {
        let start_byte = child.start_byte();
        let end_byte = child.end_byte();

        match child.kind() {
            "control_tag" | "render_expression" | "comment" | "raw_statement"
            | "endraw_statement" => {
                let mut node = parse_askama_node(child, source)?;

                if let AskamaNode::Control { tag, range, .. } = &mut node {
                    if tag.is_opening() {
                        stack.push((nodes.len(), *tag));
                    } else if let Some(pos) =
                        stack.iter().rposition(|(_, open)| open.matches_close(*tag))
                    {
                        let (open_idx, _) = stack.remove(pos);
                        if let AskamaNode::Control { end, .. } = &mut nodes[open_idx] {
                            *end = Some(range.start);
                        }
                    }
                }

                nodes.push(node);
            }
            "content" | "raw_content" => {
                content_node_ranges.push(Range {
                    start_byte,
                    end_byte,
                    start_point: child.start_position(),
                    end_point: child.end_position(),
                });
            }
            _ => unreachable!(),
        }
    }

    Ok((nodes, content_node_ranges))
}

fn parse_askama_node(node: Node, source: &str) -> Result<AskamaNode> {
    let (dlmts, inner) = extract_delimiters(node, source)?;
    let range = node.start_byte()..node.end_byte();

    let askama_node = match node.kind() {
        "control_tag" => AskamaNode::Control {
            dlmts,
            inner,
            tag: ControlTag::from_node(node),
            range,
            end: None,
        },
        "render_expression" => AskamaNode::Expression {
            dlmts,
            inner,
            range,
        },
        "raw_statement" => AskamaNode::Control {
            dlmts,
            inner,
            tag: ControlTag::Raw,
            range,
            end: None,
        },
        "endraw_statement" => AskamaNode::Control {
            dlmts,
            inner,
            tag: ControlTag::Endraw,
            range,
            end: None,
        },
        "comment" => AskamaNode::Comment {
            dlmts,
            inner,
            range,
        },
        _ => unreachable!(),
    };

    Ok(askama_node)
}

fn is_match_arm(node: Node) -> bool {
    let Some(parent) = node.parent() else {
        return false;
    };

    let mut cursor = parent.walk();
    let mut prev_node = None;

    for sibling in parent.children(&mut cursor) {
        if sibling.id() == node.id() {
            break;
        }
        if sibling.kind() == "control_tag" {
            prev_node = Some(sibling);
        }
    }

    prev_node
        .and_then(|prev| prev.child(1))
        .is_some_and(|child| child.kind() == "when_statement")
}

fn extract_delimiters(node: Node, source: &str) -> Result<(Delimiters, String)> {
    let first = node
        .child(0)
        .ok_or_else(|| anyhow::anyhow!("Node has no first child"))?;
    let last = node
        .child(node.child_count() - 1)
        .ok_or_else(|| anyhow::anyhow!("Node has no last child"))?;

    let open = first.utf8_text(source.as_bytes())?.trim().to_string();
    let close = last.utf8_text(source.as_bytes())?.trim().to_string();

    let start = first.end_byte();
    let end = last.start_byte();

    let inner = if start < end {
        source[start..end].to_string()
    } else {
        String::new()
    };

    Ok((Delimiters { open, close }, inner))
}

// TODO: find better way to solve this
pub fn is_inside_same_ctrl(start: usize, end: usize, nodes: &[AskamaNode]) -> bool {
    innermost_ctrl_idx(start, nodes) == innermost_ctrl_idx(end, nodes)
}

fn innermost_ctrl_idx(pos: usize, nodes: &[AskamaNode]) -> Option<usize> {
    nodes
        .iter()
        .enumerate()
        .filter_map(|(idx, node)| {
            if let AskamaNode::Control {
                range,
                end: Some(close_idx),
                ..
            } = node
                && pos >= range.end
                && pos < *close_idx
            {
                Some((idx, close_idx - range.end))
            } else {
                None
            }
        })
        .min_by_key(|(_, size)| *size)
        .map(|(idx, _)| idx)
}

#[must_use]
pub fn format_askama_node(node: &AskamaNode) -> String {
    if let AskamaNode::Comment { dlmts, inner, .. } = node {
        return format!("{}{}{}", dlmts.open, inner, dlmts.close);
    }

    let (raw_open, raw_close) = node.delimiters();
    let raw_inner = node.inner();

    let open = raw_open.to_string();
    let close = raw_close.to_string();

    let inner = if node.is_expr() {
        raw_inner.trim().to_string()
    } else {
        crate::normalize_ws(raw_inner)
    };

    if inner.is_empty() {
        return format!("{}{}", open, close);
    }

    format!("{} {} {}", open, inner, close)
}
