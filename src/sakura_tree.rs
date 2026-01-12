use crate::{
    askama::ControlTag,
    config::Config,
    parse::{Leaf, Root, SakuraSeed},
};

#[derive(Debug, Clone)]
pub struct SakuraTree;

#[derive(Debug, Clone)]
struct Leaflet<'a> {
    content: &'a str,
    ws_before: bool,
    pair: Option<usize>,
}

#[derive(Debug, Clone)]
struct Branch {
    start: usize,
    end: usize,
    style: Style,
}

#[derive(Debug, Clone)]
enum Style {
    Inline,
    Wrapped,
    Comment,
    Raw,
}

#[derive(Debug, Clone)]
enum Ring {
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
    Script(usize),
    Comment(usize),
    Single(usize),
}

impl SakuraTree {
    pub fn grow(seed: &SakuraSeed<'_>, cfg: &Config) -> String {
        let leaves = seed.grow_leaves();
        let indent_map = Self::generate_indent_map(&leaves);
        let rings = Self::grow_rings_recursive(&leaves, &indent_map, cfg, 0, leaves.len(), false);

        let mut branches = Vec::new();
        for ring in rings {
            Self::grow_branches_recursive(&ring, &leaves, &indent_map, cfg, &mut branches);
        }

        let mut output = String::new();

        for branch in branches {
            let indent = indent_map[branch.start];
            let base_indent = " ".repeat(indent * cfg.indent_size);

            let lines = match branch.style {
                Style::Inline => vec![Self::branch_content(branch.start, branch.end, &leaves)],
                Style::Wrapped => {
                    Self::render_wrapped(branch.start, branch.end, &leaves, &indent_map, cfg)
                }
                Style::Comment => Self::render_comment(branch.start, branch.end, &leaves, cfg),
                Style::Raw => Self::render_raw(branch.start, branch.end, &leaves),
            };

            for line in lines {
                if line.is_empty() {
                    output.push('\n');
                } else {
                    output.push_str(&base_indent);
                    output.push_str(&line);
                    output.push('\n');
                }
            }
        }

        output
    }

    fn grow_branches_recursive(
        ring: &Ring,
        leaves: &[Leaf<'_>],
        indent_map: &[usize],
        cfg: &Config,
        branches: &mut Vec<Branch>,
    ) {
        let add = |branches: &mut Vec<Branch>, start, end, style| {
            branches.push(Branch { start, end, style });
        };

        match ring {
            Ring::Block {
                start,
                end,
                inner,
                trailing,
            } => {
                let fits_inline = Self::fits(*start, *trailing, leaves, indent_map, cfg)
                    && inner.iter().all(|r| matches!(r, Ring::Phrasing { .. }));

                if fits_inline {
                    add(branches, *start, *end, Style::Inline);
                } else {
                    add(branches, *start, *start, Style::Inline); // Open

                    for child in inner {
                        Self::grow_branches_recursive(child, leaves, indent_map, cfg, branches);
                    }

                    if leaves[*end].ws_after && trailing > end {
                        add(branches, *end, *end, Style::Inline);
                        add(branches, *end + 1, *trailing, Style::Wrapped);
                    } else {
                        add(branches, *end, *trailing, Style::Inline);
                    }
                }
            }
            Ring::Phrasing { start, end } => add(branches, *start, *end, Style::Wrapped),
            Ring::MatchArm { start, end } => add(branches, *start, *end, Style::Inline),
            Ring::Script(idx) => add(branches, *idx, *idx, Style::Raw),
            Ring::Comment(idx) => add(branches, *idx, *idx, Style::Comment),
            Ring::Single(idx) => add(branches, *idx, *idx, Style::Inline),
        }
    }

    fn generate_indent_map(leaves: &[Leaf<'_>]) -> Vec<usize> {
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

    fn grow_rings_recursive(
        leaves: &[Leaf<'_>],
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
                } => Self::match_arm(start, end, leaves, indent_map, cfg),

                Root::Script | Root::Todo => (Ring::Script(start), start),
                Root::Comment => (Ring::Comment(start), start),
                Root::CssText => (Ring::Single(start), start),

                _ if !leaf.is_block() && (leaf.pair.is_none() || !leaf.ws_after) => {
                    Self::phrasing(start, end, leaves, indent_map, cfg)
                }

                _ => match leaf.pair {
                    Some(pair) => {
                        Self::block(start, pair, end, leaves, indent_map, cfg, phrasing_ctx)
                    }
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
        leaves: &[Leaf<'_>],
        indent_map: &[usize],
        cfg: &Config,
    ) -> (Ring, usize) {
        let mut curr = start + 1;
        while curr < end_idx && !leaves.get(curr).is_none_or(Leaf::is_ctrl) {
            curr = leaves[curr].pair.unwrap_or(curr) + 1;
        }

        let end = if Self::fits(start, curr.saturating_sub(1), leaves, indent_map, cfg) {
            curr.saturating_sub(1)
        } else {
            start
        };

        (Ring::MatchArm { start, end }, end)
    }

    fn phrasing(
        start: usize,
        end_idx: usize,
        leaves: &[Leaf<'_>],
        indent_map: &[usize],
        cfg: &Config,
    ) -> (Ring, usize) {
        let (mut curr, mut end) = (start, start);

        while curr < end_idx {
            let curr_leaf = &leaves[curr];
            if !curr_leaf.can_be_inline()
                || curr > start
                    && curr_leaf.pair.is_some_and(|idx| {
                        !Self::fits(curr, idx, leaves, indent_map, cfg) && curr_leaf.ws_after
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
        leaves: &[Leaf<'_>],
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
                inner: Self::grow_rings_recursive(
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

    fn fits(
        start: usize,
        end: usize,
        leaves: &[Leaf<'_>],
        indent_map: &[usize],
        cfg: &Config,
    ) -> bool {
        indent_map[start] * cfg.indent_size + Self::width(start, end, leaves) <= cfg.max_width
    }

    fn fits_2(start: usize, line: &str, extra: usize, indent_map: &[usize], cfg: &Config) -> bool {
        indent_map[start] * cfg.indent_size + line.chars().count() + extra < cfg.max_width
    }

    fn width(start: usize, end: usize, leaves: &[Leaf<'_>]) -> usize {
        (start..=end)
            .filter_map(|i| leaves.get(i))
            .map(|l| l.content.chars().count())
            .sum()
    }

    fn branch_content(start: usize, end: usize, leaves: &[Leaf<'_>]) -> String {
        let mut content = String::new();
        let mut prev_idx = None;

        for leaf_idx in start..=end {
            if let Some(leaf) = leaves.get(leaf_idx) {
                if let Some(prev) = prev_idx {
                    let has_ws = leaves.get(prev).is_some_and(|l: &Leaf| {
                        l.preserves_ws() && leaf.preserves_ws() && (l.ws_after || leaf.ws_before)
                    });
                    if has_ws {
                        content.push(' ');
                    }
                }
                content.push_str(&leaf.content);
                prev_idx = Some(leaf_idx);
            }
        }

        content
    }

    fn grow_leaflets<'a>(
        branch_start: usize,
        branch_end: usize,
        leaves: &'a [Leaf<'a>],
    ) -> Vec<Leaflet<'a>> {
        let leaf_slice = &leaves[branch_start..=branch_end];
        let mut leaflets = Vec::new();
        let mut pairs = Vec::new();

        for leaf in leaf_slice {
            pairs.push(leaflets.len());

            if leaf.root == Root::Text {
                for (i, content) in leaf.content.split_whitespace().enumerate() {
                    leaflets.push(Leaflet {
                        content,
                        ws_before: i > 0 || leaf.ws_before,
                        pair: None,
                    });
                }
            } else {
                leaflets.push(Leaflet {
                    content: &leaf.content,
                    ws_before: leaf.ws_before,
                    pair: None,
                });
            }
        }

        for (i, leaf) in leaf_slice.iter().enumerate() {
            if let Some(leaf_pair) = leaf.pair.and_then(|p| p.checked_sub(branch_start))
                && let (Some(start), Some(end)) = (pairs.get(i), pairs.get(leaf_pair))
                && let Some(leaflet) = leaflets.get_mut(*start)
            {
                leaflet.pair = Some(*end);
            }

            if leaf.ws_after
                && let Some(next) = pairs.get(i + 1)
                && let Some(leaflet) = leaflets.get_mut(*next)
            {
                leaflet.ws_before = true;
            }
        }

        leaflets
    }

    fn render_wrapped(
        start: usize,
        end: usize,
        leaves: &[Leaf<'_>],
        indent_map: &[usize],
        cfg: &Config,
    ) -> Vec<String> {
        let leaflets = Self::grow_leaflets(start, end, leaves);
        let mut lines = Vec::new();
        let mut curr_line = String::new();
        let mut pair = None;

        for (i, leaflet) in leaflets.iter().enumerate() {
            let mut end = leaflets[i].pair.unwrap_or(i);

            while let Some(next) = leaflets.get(end + 1).filter(|n| !n.ws_before) {
                end = next.pair.unwrap_or(end + 1);
            }

            let curr_width = leaflets[i..=end]
                .iter()
                .enumerate()
                .map(|(i, ll)| ll.content.chars().count() + usize::from(i > 0 && ll.ws_before))
                .sum();

            if leaflet.ws_before
                && pair.is_none_or(|p| i > p)
                && !curr_line.is_empty()
                && !Self::fits_2(start, &curr_line, curr_width, indent_map, cfg)
            {
                lines.push(std::mem::take(&mut curr_line));
            }

            if leaflet.ws_before && !curr_line.is_empty() {
                curr_line.push(' ');
            }

            curr_line.push_str(leaflet.content);
            pair = pair.max(leaflet.pair);
        }

        lines.push(curr_line);
        lines
    }

    fn render_comment(start: usize, end: usize, leaves: &[Leaf<'_>], cfg: &Config) -> Vec<String> {
        let content = Self::branch_content(start, end, leaves);
        let lines: Vec<&str> = content.lines().collect();
        let indent = " ".repeat(cfg.indent_size);

        lines
            .iter()
            .enumerate()
            .map(|(i, line)| {
                let line = line.trim();
                if line.is_empty() || i == 0 || i == lines.len() - 1 {
                    line.to_string()
                } else {
                    format!("{}{}", indent, line)
                }
            })
            .collect()
    }

    fn render_raw(start: usize, end: usize, leaves: &[Leaf<'_>]) -> Vec<String> {
        let content = Self::branch_content(start, end, leaves);
        let lines: Vec<&str> = content.lines().collect();

        let indent = lines
            .iter()
            .filter_map(|l| l.chars().position(|c| !c.is_whitespace()))
            .min()
            .unwrap_or(0);

        lines
            .iter()
            .enumerate()
            .filter_map(|(i, line)| {
                if !line.trim().is_empty() {
                    Some(line.get(indent..).unwrap_or(line).to_string())
                } else if i != 0 && i != lines.len() - 1 {
                    Some(String::new())
                } else {
                    None
                }
            })
            .collect()
    }
}
