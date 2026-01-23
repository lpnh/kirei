use super::{
    leaf::{self, Leaf},
    ring::Ring,
};

use crate::config::Config;

#[derive(Debug, Clone)]
pub struct Branch {
    pub start: usize,
    pub end: usize,
    pub style: Style,
}

#[derive(Debug, Clone)]
pub enum Style {
    Inline,
    Wrapped,
    Comment,
    Opaque,
}

pub fn grow_branches_recursive(
    ring: &Ring,
    leaves: &[Leaf],
    indent_map: &[usize],
    cfg: &Config,
    branches: &mut Vec<Branch>,
) {
    let grow = |branches: &mut Vec<Branch>, start, end, style| {
        branches.push(Branch { start, end, style });
    };

    match ring {
        Ring::Block {
            start,
            end,
            inner,
            trailing,
        } => {
            let fits_inline = leaf::fits(*start, *trailing, leaves, indent_map, cfg)
                && inner.iter().all(|r| matches!(r, Ring::Phrasing { .. }));
            if fits_inline {
                grow(branches, *start, *end, Style::Inline);
            } else {
                grow(branches, *start, *start, Style::Inline);
                for child in inner {
                    grow_branches_recursive(child, leaves, indent_map, cfg, branches);
                }
                if leaves[*end].ws_after && trailing > end {
                    grow(branches, *end, *end, Style::Inline);
                    grow(branches, *end + 1, *trailing, Style::Wrapped);
                } else {
                    grow(branches, *end, *trailing, Style::Inline);
                }
            }
        }
        Ring::Phrasing { start, end } => grow(branches, *start, *end, Style::Wrapped),
        Ring::MatchArm { start, end } => grow(branches, *start, *end, Style::Inline),
        Ring::Opaque(idx) => grow(branches, *idx, *idx, Style::Opaque),
        Ring::Comment(idx) => grow(branches, *idx, *idx, Style::Comment),
        Ring::Single(idx) => grow(branches, *idx, *idx, Style::Inline),
    }
}
