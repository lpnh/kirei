use super::leaf::{self, Leaf, Root};

use crate::{config::Config, parse::askama::ControlTag};

#[derive(Debug, Clone)]
pub enum Ring {
    Block {
        start: usize,
        end: usize,
        inner: Vec<Ring>,
        trailing: usize,
    },
    MatchArm {
        start: usize,
        end: usize,
    },
    Phrasing {
        start: usize,
        end: usize,
    },
    Comment(usize),
    Single(usize),

    Opaque(usize),
}

pub fn generate_indent_map(leaves: &[Leaf]) -> Vec<usize> {
    let mut indent_map = Vec::new();
    let mut curr_indent: usize = 0;

    for leaf in leaves {
        let (pre_delta, post_delta) = match &leaf.root {
            Root::Control { tag, .. } => tag.indent(),
            Root::Tag { indent, .. } | Root::CssBlock { indent } => {
                if *indent < 0 {
                    (*indent, 0)
                } else {
                    (0, *indent)
                }
            }
            _ => (0, 0),
        };

        curr_indent = curr_indent.saturating_add_signed(pre_delta);
        indent_map.push(curr_indent);
        curr_indent = curr_indent.saturating_add_signed(post_delta);
    }

    indent_map
}

pub fn grow_rings_recursive(
    leaves: &[Leaf],
    indent_map: &[usize],
    cfg: &Config,
    mut start: usize,
    end: usize,
    phrasing_ctx: bool,
) -> Vec<Ring> {
    let mut rings = Vec::new();
    while start < end {
        let leaf = &leaves[start];

        let (ring, last) = match leaf.root {
            Root::Control {
                tag: ControlTag::When | ControlTag::MatchElse,
                ..
            } => match_arm(start, end, leaves, indent_map, cfg),
            Root::Script => (Ring::Opaque(start), start),
            Root::Comment => (Ring::Comment(start), start),
            Root::CssText => (Ring::Single(start), start),
            _ if !leaf.is_block() && (leaf.pair.is_none() || !leaf.ws_after) => {
                phrasing(start, end, leaves, indent_map, cfg)
            }
            _ => match leaf.pair {
                Some(pair) => block(start, pair, end, leaves, indent_map, cfg, phrasing_ctx),
                None => (Ring::Single(start), start),
            },
        };

        rings.push(ring);
        start = last + 1;
    }

    rings
}

fn match_arm(
    start: usize,
    end_idx: usize,
    leaves: &[Leaf],
    indent_map: &[usize],
    cfg: &Config,
) -> (Ring, usize) {
    let mut curr = start + 1;
    while curr < end_idx && !leaves.get(curr).is_none_or(Leaf::is_ctrl) {
        curr = leaves[curr].pair.unwrap_or(curr) + 1;
    }

    let end = if leaf::fits(start, curr.saturating_sub(1), leaves, indent_map, cfg) {
        curr.saturating_sub(1)
    } else {
        start
    };

    (Ring::MatchArm { start, end }, end)
}

fn phrasing(
    start: usize,
    end_idx: usize,
    leaves: &[Leaf],
    indent_map: &[usize],
    cfg: &Config,
) -> (Ring, usize) {
    let (mut curr, mut end) = (start, start);

    while curr < end_idx {
        let curr_leaf = &leaves[curr];
        if !curr_leaf.can_be_inline()
            || curr > start
                && curr_leaf.pair.is_some_and(|idx| {
                    !leaf::fits(curr, idx, leaves, indent_map, cfg) && curr_leaf.ws_after
                })
            || curr_leaf.is_ctrl() && curr_leaf.pair.is_none()
        {
            break;
        }
        end = curr_leaf.pair.unwrap_or(curr);
        curr = end + 1;
    }

    (Ring::Phrasing { start, end }, end)
}

fn block(
    start: usize,
    pair: usize,
    end: usize,
    leaves: &[Leaf],
    indent_map: &[usize],
    cfg: &Config,
    phrasing_ctx: bool,
) -> (Ring, usize) {
    let leaf = &leaves[start];
    let curr = start + 1;
    let all_inline = leaves[curr..pair].iter().all(Leaf::can_be_inline);

    let trailing = if leaf.can_be_inline() {
        let mut trailing = pair;
        while trailing + 1 < end {
            if leaves[trailing].ws_after {
                break;
            }
            let next = &leaves[trailing + 1];
            if next.ws_before {
                break;
            }
            trailing = next.pair.unwrap_or(trailing + 1);
        }
        trailing
    } else {
        pair
    };

    let ring = if phrasing_ctx && all_inline && leaf.can_be_inline() && !leaf.ws_after {
        Ring::Phrasing {
            start,
            end: trailing,
        }
    } else {
        Ring::Block {
            start,
            end: pair,
            inner: grow_rings_recursive(
                leaves,
                indent_map,
                cfg,
                curr,
                pair,
                phrasing_ctx || all_inline || !leaf.ws_before,
            ),
            trailing,
        }
    };

    (ring, trailing)
}
