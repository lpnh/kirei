use anyhow::Result;
use tree_sitter::Node;

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
        ctrl_tag: Option<ControlTag>,
    },
    Expression {
        dlmts: Delimiters,
        inner: String,
    },
    Comment {
        dlmts: Delimiters,
        inner: String,
    },
}

impl AskamaNode {
    pub fn placeholder(&self, idx: usize) -> String {
        match self {
            Self::Control { .. } => format!("__ASKAMA_CTRL_{}_ASKAMA_END__", idx),
            Self::Expression { .. } => format!("__ASKAMA_EXPR_{}_ASKAMA_END__", idx),
            Self::Comment { .. } => format!("__ASKAMA_COMMENT_{}_ASKAMA_END__", idx),
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
            Self::Control {
                ctrl_tag: Some(tag),
                ..
            } => match tag {
                ControlTag::Match(_) => (0, 2),
                ControlTag::Endmatch(_) => (-2, 0),
                _ => match tag.boundary() {
                    Boundary::Open => (0, 1),
                    Boundary::Clause | Boundary::Inner => (-1, 1),
                    Boundary::Close => (-1, 0),
                    Boundary::Standalone => (0, 0),
                },
            },
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
    pub fn get_ctrl_tag(&self) -> Option<ControlTag> {
        match self {
            Self::Control { ctrl_tag, .. } => *ctrl_tag,
            _ => None,
        }
    }
    pub fn is_match_arm(&self) -> bool {
        matches!(
            self.get_ctrl_tag(),
            Some(ControlTag::When(_) | ControlTag::Else(Boundary::Inner))
        )
    }
}

pub fn extract_nodes(source: &str, root: &Node) -> Result<(String, Vec<AskamaNode>)> {
    let mut html = String::new();
    let mut nodes = Vec::new();
    let mut pos = 0;

    let mut cursor = root.walk();
    for child in root.children(&mut cursor) {
        let start = child.start_byte();
        let end = child.end_byte();

        if start > pos {
            let text_between = &source[pos..start];
            html.push_str(text_between);
        }

        if let Some(node) = parse_askama_node(child, source) {
            let placeholder = node.placeholder(nodes.len());
            html.push_str(&placeholder);
            nodes.push(node);
        } else {
            let text = child.utf8_text(source.as_bytes())?;
            html.push_str(text);
        }

        pos = end;
    }

    if pos < source.len() {
        let remaining = &source[pos..];
        html.push_str(remaining);
    }

    Ok((html, nodes))
}

fn parse_askama_node(node: Node, source: &str) -> Option<AskamaNode> {
    let (dlmts, inner) = extract_delimiters(node, source)?;

    match node.kind() {
        "control_tag" => {
            let ctrl_tag = detect_block_type(node);
            Some(AskamaNode::Control {
                dlmts,
                inner,
                ctrl_tag,
            })
        }
        "render_expression" => Some(AskamaNode::Expression { dlmts, inner }),
        "comment" => Some(AskamaNode::Comment { dlmts, inner }),
        _ => None,
    }
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

fn detect_block_type(node: Node) -> Option<ControlTag> {
    let child = node.child(1)?;
    match child.kind() {
        "if_statement" => Some(ControlTag::If(Boundary::Open)),
        "for_statement" => Some(ControlTag::For(Boundary::Open)),
        "block_statement" => Some(ControlTag::Block(Boundary::Open)),
        "filter_statement" => Some(ControlTag::Filter(Boundary::Open)),
        "match_statement" => Some(ControlTag::Match(Boundary::Open)),
        "macro_statement" => Some(ControlTag::Macro(Boundary::Open)),
        "macro_call_statement" => Some(ControlTag::MacroCall(Boundary::Open)),

        "else_statement" => {
            let boundary = if belongs_to_match_statement(node) {
                Boundary::Inner
            } else {
                Boundary::Clause
            };
            Some(ControlTag::Else(boundary))
        }
        "else_if_statement" => Some(ControlTag::ElseIf(Boundary::Clause)),
        "when_statement" => Some(ControlTag::When(Boundary::Inner)),

        "endif_statement" => Some(ControlTag::Endif(Boundary::Close)),
        "endfor_statement" => Some(ControlTag::Endfor(Boundary::Close)),
        "endblock_statement" => Some(ControlTag::Endblock(Boundary::Close)),
        "endfilter_statement" => Some(ControlTag::Endfilter(Boundary::Close)),
        "endmatch_statement" => Some(ControlTag::Endmatch(Boundary::Close)),
        "endmacro_statement" => Some(ControlTag::Endmacro(Boundary::Close)),
        "endcall_statement" => Some(ControlTag::Endcall(Boundary::Close)),
        "endwhen_statement" => Some(ControlTag::Endwhen(Boundary::Close)),

        "extends_statement" => Some(ControlTag::Extends(Boundary::Standalone)),
        "include_statement" => Some(ControlTag::Include(Boundary::Standalone)),
        "import_statement" => Some(ControlTag::Import(Boundary::Standalone)),
        "let_statement" => Some(ControlTag::Let(Boundary::Standalone)),
        "break_statement" => Some(ControlTag::Break(Boundary::Standalone)),
        "continue_statement" => Some(ControlTag::Continue(Boundary::Standalone)),

        _ => None,
    }
}

fn extract_delimiters(node: Node, source: &str) -> Option<(Delimiters, String)> {
    let first = node.child(0)?;
    let last = node.child(node.child_count() - 1)?;

    let open = first.utf8_text(source.as_bytes()).ok()?.to_string();
    let close = last.utf8_text(source.as_bytes()).ok()?.to_string();

    let start = first.end_byte();
    let end = last.start_byte();

    let inner = if start < end {
        source[start..end].to_string()
    } else {
        String::new()
    };

    Some((Delimiters { open, close }, inner))
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

        // Remove leading whitespace to avoid extra empty lines
        let trimmed_inner = inner.trim_start();

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
        let trimmed_inner = inner.trim();
        format!("{} {} {}", open, trimmed_inner, close)
    }
}

pub fn replace_placeholder_in_raw_text(text: &str, nodes: &[AskamaNode]) -> String {
    let mut result = text.to_string();
    for (idx, node) in nodes.iter().enumerate() {
        let placeholder = node.placeholder(idx);
        if result.contains(&placeholder) {
            let formatted = fmt_node_for_attr_or_raw_text(node);
            result = result.replace(&placeholder, &formatted);
        }
    }
    result
}

pub fn fmt_node_for_attr_or_raw_text(node: &AskamaNode) -> String {
    let (open, close) = node.delimiters();
    let inner = normalize_whitespace(node.inner());
    format!("{} {} {}", open, inner.trim(), close)
}

fn normalize_askama_node(node: &AskamaNode, config: &Config) -> (String, String, String) {
    let (raw_open, raw_close) = node.delimiters();
    let raw_inner = node.inner();

    // Normalize delimiters just in case
    let open = raw_open.trim().to_string();
    let close = raw_close.trim().to_string();

    // Normalize inner content
    let inner = if node.is_comment() {
        // Special treatment for comments
        normalize_askama_comment(raw_inner, config.max_width)
    } else {
        normalize_whitespace(raw_inner)
    };

    (open, close, inner)
}

fn normalize_askama_comment(text: &str, max_length: usize) -> String {
    let normalized = normalize_whitespace(text);
    if normalized.len() <= max_length {
        normalized
    } else {
        text.to_string()
    }
}

fn normalize_whitespace(text: &str) -> String {
    text.split_whitespace().collect::<Vec<_>>().join(" ")
}
