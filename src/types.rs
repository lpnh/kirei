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

#[derive(Debug, Clone)]
pub(crate) enum AskamaNode {
    Control {
        inner: String,
        dlmts: (String, String),
        maybe_block: Option<Block>,
    },
    Expression {
        inner: String,
        dlmts: (String, String),
    },
    Comment {
        inner: String,
        dlmts: (String, String),
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

    pub(crate) fn indent_delta(&self) -> (i32, i32) {
        match self {
            Self::Control {
                maybe_block: Some(Block::Open),
                ..
            } => (0, 1),
            Self::Control {
                maybe_block: Some(Block::Inner),
                ..
            } => (0, 0),
            Self::Control {
                maybe_block: Some(Block::Clause),
                ..
            } => (-1, 1),
            Self::Control {
                maybe_block: Some(Block::Close),
                ..
            } => (-1, 0),
            _ => (0, 0), // Inline
        }
    }

    pub(crate) fn is_opening_block(&self) -> bool {
        matches!(
            self,
            Self::Control {
                maybe_block: Some(Block::Open),
                ..
            }
        )
    }

    pub(crate) fn is_closing_block(&self) -> bool {
        matches!(
            self,
            Self::Control {
                maybe_block: Some(Block::Close),
                ..
            }
        )
    }

    pub(crate) fn get_block_type(&self) -> Option<&str> {
        match self {
            Self::Control {
                inner,
                maybe_block: Some(_),
                ..
            } => inner.split_whitespace().next(),
            _ => None,
        }
    }
}
