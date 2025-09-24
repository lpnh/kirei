pub(crate) const ASKAMA_TOKEN: &str = "__ASKAMA_";
pub(crate) const ASKAMA_CTRL_TOKEN: &str = "__ASKAMA_CTRL_";
pub(crate) const ASKAMA_EXPR_TOKEN: &str = "__ASKAMA_EXPR_";
pub(crate) const ASKAMA_COMMENT_TOKEN: &str = "__ASKAMA_COMMENT_";
pub(crate) const ASKAMA_END_TOKEN: &str = "_ASKAMA_END__";

#[derive(Debug, Clone, Copy)]
pub(crate) enum Block {
    Open,   // Tags that start blocks: if, for, block, etc.
    Inner,  // Tags that continue blocks with new indent: when
    Clause, // Tags that continue blocks at same indent: else, else if
    Close,  // Tags that end blocks: endif, endfor, endblock, etc.
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum BlockType {
    If,
    For,
    Block,
    Filter,
    Match,
    Macro,
    MacroCall,
}

#[derive(Debug, Clone)]
pub(crate) struct Delimiters {
    pub(crate) open: String,
    pub(crate) close: String,
}

#[derive(Debug, Clone)]
pub(crate) enum AskamaNode {
    Control {
        dlmts: Delimiters,
        inner: String,
        block_info: Option<(Block, BlockType)>,
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
    pub(crate) fn placeholder(&self, idx: usize) -> String {
        let token = match self {
            Self::Control { .. } => ASKAMA_CTRL_TOKEN,
            Self::Expression { .. } => ASKAMA_EXPR_TOKEN,
            Self::Comment { .. } => ASKAMA_COMMENT_TOKEN,
        };
        format!("{}{}{}", token, idx, ASKAMA_END_TOKEN)
    }

    pub(crate) fn delimiters(&self) -> (&str, &str) {
        match self {
            Self::Control { dlmts, .. }
            | Self::Expression { dlmts, .. }
            | Self::Comment { dlmts, .. } => (&dlmts.open, &dlmts.close),
        }
    }

    pub(crate) fn inner(&self) -> &str {
        match self {
            Self::Control { inner, .. }
            | Self::Expression { inner, .. }
            | Self::Comment { inner, .. } => inner,
        }
    }

    pub(crate) fn indent_delta(&self) -> (i32, i32) {
        match self {
            Self::Control {
                block_info: Some((Block::Open, _)),
                ..
            } => (0, 1),
            Self::Control {
                block_info: Some((Block::Inner, _)),
                ..
            } => (0, 0),
            Self::Control {
                block_info: Some((Block::Clause, _)),
                ..
            } => (-1, 1),
            Self::Control {
                block_info: Some((Block::Close, _)),
                ..
            } => (-1, 0),
            _ => (0, 0), // Should we handle Expressions and Comments here too?
        }
    }

    pub(crate) fn is_expr(&self) -> bool {
        matches!(self, Self::Expression { .. })
    }

    pub(crate) fn is_comment(&self) -> bool {
        matches!(self, Self::Comment { .. })
    }

    pub(crate) fn get_block_info(&self) -> Option<(Block, BlockType)> {
        match self {
            Self::Control { block_info, .. } => *block_info,
            _ => None,
        }
    }

    pub(crate) fn prefers_inline(&self) -> bool {
        matches!(
            self.get_block_info(),
            Some(
                (Block::Open | Block::Close, BlockType::MacroCall)
                    | (Block::Inner, BlockType::Match)
            )
        ) || matches!(self, AskamaNode::Expression { .. })
    }
}
