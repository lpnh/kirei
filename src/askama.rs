use std::borrow::Cow;
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

    pub fn is_opening(self) -> bool {
        matches!(self.boundary(), Boundary::Open)
    }

    fn matches_close(self, close: Self) -> bool {
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
pub struct Delimiters<'a> {
    open: &'a str,
    close: &'a str,
}

#[derive(Debug, Clone)]
pub enum AskamaNode<'a> {
    Control {
        dlmts: Delimiters<'a>,
        inner: Cow<'a, str>,
        tag: ControlTag,
        range: std::ops::Range<usize>,
        end: Option<usize>,
    },
    Expression {
        dlmts: Delimiters<'a>,
        inner: Cow<'a, str>,
        range: std::ops::Range<usize>,
    },
    Comment {
        dlmts: Delimiters<'a>,
        inner: Cow<'a, str>,
        range: std::ops::Range<usize>,
    },
}

impl AskamaNode<'_> {
    pub fn start(&self) -> usize {
        match self {
            Self::Control { range, .. }
            | Self::Expression { range, .. }
            | Self::Comment { range, .. } => range.start,
        }
    }

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
            | Self::Comment { dlmts, .. } => (dlmts.open, dlmts.close),
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

pub fn extract_askama<'a>(root: &Node, source: &'a str) -> (Vec<AskamaNode<'a>>, Vec<Range>) {
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
                let mut node = parse_askama_node(child, source);

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

    (nodes, content_node_ranges)
}

fn parse_askama_node<'a>(node: Node, source: &'a str) -> AskamaNode<'a> {
    let (dlmts, inner) = extract_delimiters(node, source);
    let range = node.start_byte()..node.end_byte();

    match node.kind() {
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
    }
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

fn extract_delimiters<'a>(node: Node, source: &'a str) -> (Delimiters<'a>, Cow<'a, str>) {
    let first = node.child(0).expect("node should have first child");
    let last = node
        .child(node.child_count().saturating_sub(1) as u32)
        .expect("node should have last child");

    let open = first
        .utf8_text(source.as_bytes())
        .expect("valid UTF-8")
        .trim();
    let close = last
        .utf8_text(source.as_bytes())
        .expect("valid UTF-8")
        .trim();

    let start = first.end_byte();
    let end = last.start_byte();

    let inner = if start < end {
        Cow::Borrowed(&source[start..end])
    } else {
        Cow::Borrowed("")
    };

    (Delimiters { open, close }, inner)
}

pub fn format_askama_node(node: &AskamaNode) -> String {
    if let AskamaNode::Comment { dlmts, inner, .. } = node {
        return format!("{}{}{}", dlmts.open, inner, dlmts.close);
    }

    let (open, close) = node.delimiters();
    let raw_inner = node.inner();

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
