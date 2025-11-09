use anyhow::Result;
use std::ops;
use tree_sitter::{Node, Range};

use crate::config::Config;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Boundary {
    Open,       // Tags that start blocks: if, for, block, etc.
    Inner,      // Tags that continue blocks with new indent: when
    Clause,     // Tags that continue blocks at same indent: else, else if
    Close,      // Tags that end blocks: endif, endfor, endblock, etc.
    Standalone, // Tags that are "standalone": extends, import, etc.
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ControlTag {
    Block(Boundary),
    Endblock(Boundary),
    Filter(Boundary),
    Endfilter(Boundary),
    Extends(Boundary),
    Include(Boundary),
    Import(Boundary),
    Let(Boundary),
    For(Boundary),
    Break(Boundary),
    Continue(Boundary),
    Endfor(Boundary),
    If(Boundary),
    ElseIf(Boundary),
    Else(Boundary),
    Endif(Boundary),
    Match(Boundary),
    Endmatch(Boundary),
    When(Boundary),
    Endwhen(Boundary),
    Macro(Boundary),
    Endmacro(Boundary),
    MacroCall(Boundary),
    Endcall(Boundary),
}

impl ControlTag {
    pub fn boundary(self) -> Boundary {
        match self {
            Self::Block(b)
            | Self::Endblock(b)
            | Self::Filter(b)
            | Self::Endfilter(b)
            | Self::Extends(b)
            | Self::Include(b)
            | Self::Import(b)
            | Self::Let(b)
            | Self::For(b)
            | Self::Break(b)
            | Self::Continue(b)
            | Self::Endfor(b)
            | Self::If(b)
            | Self::ElseIf(b)
            | Self::Else(b)
            | Self::Endif(b)
            | Self::Match(b)
            | Self::Endmatch(b)
            | Self::When(b)
            | Self::Endwhen(b)
            | Self::Macro(b)
            | Self::Endmacro(b)
            | Self::MacroCall(b)
            | Self::Endcall(b) => b,
        }
    }

    pub fn indent_delta(self) -> (i32, i32) {
        match self {
            Self::Match(_) => (0, 2),
            Self::Endmatch(_) => (-2, 0),
            _ => match self.boundary() {
                Boundary::Open => (0, 1),
                Boundary::Clause | Boundary::Inner => (-1, 1),
                Boundary::Close => (-1, 0),
                Boundary::Standalone => (0, 0),
            },
        }
    }

    pub fn is_opening(self) -> bool {
        matches!(self.boundary(), Boundary::Open)
    }

    pub fn is_match_arm(self) -> bool {
        matches!(self, Self::When(_) | Self::Else(Boundary::Inner))
    }

    pub fn matching_close(self) -> Option<Self> {
        match self {
            Self::If(_) => Some(Self::Endif(Boundary::Close)),
            Self::For(_) => Some(Self::Endfor(Boundary::Close)),
            Self::Match(_) => Some(Self::Endmatch(Boundary::Close)),
            Self::Block(_) => Some(Self::Endblock(Boundary::Close)),
            Self::Filter(_) => Some(Self::Endfilter(Boundary::Close)),
            Self::Macro(_) => Some(Self::Endmacro(Boundary::Close)),
            Self::MacroCall(_) => Some(Self::Endcall(Boundary::Close)),
            _ => None,
        }
    }

    // Check if this opening tag matches with a closing tag
    pub fn matches_close(self, close: Self) -> bool {
        matches!(
            (self, close),
            (Self::Block(_), Self::Endblock(_))
                | (Self::Filter(_), Self::Endfilter(_))
                | (Self::For(_), Self::Endfor(_))
                | (Self::If(_), Self::Endif(_))
                | (Self::Match(_), Self::Endmatch(_))
                | (Self::When(_), Self::Endwhen(_))
                | (Self::Macro(_), Self::Endmacro(_))
                | (Self::MacroCall(_), Self::Endcall(_))
        )
    }

    // Check if two tags are the same kind (both If, both For, etc.)
    pub fn same_kind(self, other: Self) -> bool {
        std::mem::discriminant(&self) == std::mem::discriminant(&other)
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
        ctrl_tag: ControlTag,
        range: ops::Range<usize>,
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

    pub fn indent_delta(&self) -> (i32, i32) {
        match self {
            Self::Control { ctrl_tag, .. } => ctrl_tag.indent_delta(),
            _ => (0, 0),
        }
    }

    pub fn is_ctrl(&self) -> bool {
        matches!(self, Self::Control { .. })
    }
    pub fn is_expr(&self) -> bool {
        matches!(self, Self::Expression { .. })
    }
    pub fn is_comment(&self) -> bool {
        matches!(self, Self::Comment { .. })
    }
    pub fn get_ctrl_tag(&self) -> ControlTag {
        match self {
            Self::Control { ctrl_tag, .. } => *ctrl_tag,
            _ => unreachable!(),
        }
    }
    pub fn is_match_arm(&self) -> bool {
        matches!(
            self.get_ctrl_tag(),
            ControlTag::When(_) | ControlTag::Else(Boundary::Inner)
        )
    }
}

pub fn extract_askama_nodes(root: &Node, source: &str) -> Result<(Vec<AskamaNode>, Vec<Range>)> {
    let mut nodes = Vec::new();
    let mut content_node_ranges = Vec::new();

    let mut cursor = root.walk();
    for child in root.children(&mut cursor) {
        let start_byte = child.start_byte();
        let end_byte = child.end_byte();

        match child.kind() {
            "control_tag" | "render_expression" | "comment" => {
                let node = parse_askama_node(child, source)?;
                nodes.push(node);
            }
            "content" => {
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
        "control_tag" => {
            let ctrl_tag = detect_block_type(node);
            AskamaNode::Control {
                dlmts,
                inner,
                ctrl_tag,
                range,
            }
        }
        "render_expression" => AskamaNode::Expression {
            dlmts,
            inner,
            range,
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

fn belongs_to_match_statement(node: Node) -> bool {
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

fn detect_block_type(node: Node) -> ControlTag {
    let child = node.child(1).unwrap();
    match child.kind() {
        "if_statement" => ControlTag::If(Boundary::Open),
        "for_statement" => ControlTag::For(Boundary::Open),
        "block_statement" => ControlTag::Block(Boundary::Open),
        "filter_statement" => ControlTag::Filter(Boundary::Open),
        "match_statement" => ControlTag::Match(Boundary::Open),
        "macro_statement" => ControlTag::Macro(Boundary::Open),
        "macro_call_statement" => ControlTag::MacroCall(Boundary::Open),

        "else_statement" => {
            let boundary = if belongs_to_match_statement(node) {
                Boundary::Inner
            } else {
                Boundary::Clause
            };
            ControlTag::Else(boundary)
        }
        "else_if_statement" => ControlTag::ElseIf(Boundary::Clause),
        "when_statement" => ControlTag::When(Boundary::Inner),

        "endif_statement" => ControlTag::Endif(Boundary::Close),
        "endfor_statement" => ControlTag::Endfor(Boundary::Close),
        "endblock_statement" => ControlTag::Endblock(Boundary::Close),
        "endfilter_statement" => ControlTag::Endfilter(Boundary::Close),
        "endmatch_statement" => ControlTag::Endmatch(Boundary::Close),
        "endmacro_statement" => ControlTag::Endmacro(Boundary::Close),
        "endcall_statement" => ControlTag::Endcall(Boundary::Close),
        "endwhen_statement" => ControlTag::Endwhen(Boundary::Close),

        "extends_statement" => ControlTag::Extends(Boundary::Standalone),
        "include_statement" => ControlTag::Include(Boundary::Standalone),
        "import_statement" => ControlTag::Import(Boundary::Standalone),
        "let_statement" => ControlTag::Let(Boundary::Standalone),
        "break_statement" => ControlTag::Break(Boundary::Standalone),
        "continue_statement" => ControlTag::Continue(Boundary::Standalone),
        _ => unreachable!(),
    }
}

fn extract_delimiters(node: Node, source: &str) -> Result<(Delimiters, String)> {
    let first = node
        .child(0)
        .ok_or_else(|| anyhow::anyhow!("Node has no first child"))?;
    let last = node
        .child(node.child_count() - 1)
        .ok_or_else(|| anyhow::anyhow!("Node has no last child"))?;

    let open = first.utf8_text(source.as_bytes())?.to_string();
    let close = last.utf8_text(source.as_bytes())?.to_string();

    let start = first.end_byte();
    let end = last.start_byte();

    let inner = if start < end {
        source[start..end].to_string()
    } else {
        String::new()
    };

    Ok((Delimiters { open, close }, inner))
}

// Format an Askama node with proper normalization
pub fn format_askama_node(config: &Config, node: &AskamaNode) -> String {
    // Normalize whitespace inside delimiters
    let (open, close, inner) = normalize_askama_node(node, config);

    // If no inner content, return delimiters
    if inner.is_empty() {
        return format!("{}{}", open, close);
    }

    let total_inline_len = open.len() + 1 + inner.len() + 1 + close.len();

    if total_inline_len > config.max_width {
        // Multiline format
        let content_indent = " ".repeat(config.indent_size);

        // Respect user's line breaks by processing each line separately
        let mut formatted_lines = Vec::new();

        // Remove whitespace to avoid extra empty lines
        let trimmed_inner = inner.trim();

        for line in trimmed_inner.lines() {
            let trimmed_line = line.trim();
            if trimmed_line.is_empty() {
                // Preserve empty lines
                formatted_lines.push(String::new());
            } else {
                formatted_lines.push(format!("{}{}", content_indent, trimmed_line));
            }
        }

        // The closing delimiter should be aligned with the opening delimiter
        format!("{}\n{}\n{}", open, formatted_lines.join("\n"), close)
    } else {
        // Inline format with enforced single space padding
        format!("{} {} {}", open, inner, close)
    }
}

fn normalize_askama_node(node: &AskamaNode, config: &Config) -> (String, String, String) {
    let (raw_open, raw_close) = node.delimiters();
    let raw_inner = node.inner();

    // Normalize delimiters just in case
    let open = raw_open.trim().to_string();
    let close = raw_close.trim().to_string();

    // Normalize inner content
    let inner = if node.is_expr() {
        // Expressions just need simple trimming
        raw_inner.trim().to_string()
    } else if node.is_comment() {
        // Special treatment for comments
        normalize_askama_comment(raw_inner, config.max_width)
    } else {
        crate::normalize_whitespace(raw_inner)
    };

    (open, close, inner)
}

fn normalize_askama_comment(text: &str, max_length: usize) -> String {
    let normalized = crate::normalize_whitespace(text);
    if normalized.len() <= max_length {
        normalized
    } else {
        text.to_string()
    }
}
