use std::cmp::Ordering;
use std::collections::{BTreeSet, HashSet};
use std::ops::Range;

use crate::{
    askama::{self, AskamaNode, ControlTag},
    config::Config,
    html::{self, HtmlNode},
};

#[derive(Debug, Clone)]
pub struct SakuraTree {
    cfg: Config,
    leaves: Vec<Leaf>,
    branches: Vec<Branch>,
    indent_map: Vec<usize>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
struct Leaf {
    root: Root,
    content: String,
    ws_before: bool,
    ws_after: bool,
    start: usize,
    end: usize,
}

impl Ord for Leaf {
    fn cmp(&self, other: &Self) -> Ordering {
        self.start.cmp(&other.start)
    }
}

impl PartialOrd for Leaf {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
enum Root {
    Control {
        tag: ControlTag,
        end: Option<usize>,
    },
    Expr,
    Comment,

    Tag {
        indent: isize,
        is_phrasing: bool,
        is_ws_sensitive: bool,
        end: Option<usize>,
    },

    Text,
    Entity,
    Raw,
}

#[derive(Debug, Clone)]
struct Leaflet<'a> {
    content: &'a str,
    ws_before: bool,
    pair_end: Option<usize>,
}

#[derive(Debug, Clone)]
struct Branch {
    indent: usize,
    lines: Vec<String>,
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
    Raw(usize),
    Comment(usize),
    Single(usize),
}

impl Leaf {
    fn grow(root: Root, content: String, start: usize, end: usize) -> Self {
        Self {
            root,
            content,
            ws_before: false,
            ws_after: false,
            start,
            end,
        }
    }

    fn pair(&self) -> Option<usize> {
        match &self.root {
            Root::Control { end: Some(end), .. } | Root::Tag { end: Some(end), .. } => Some(*end),
            _ => None,
        }
    }

    fn from_askama(askama_node: &AskamaNode) -> Self {
        let content = askama::format_askama_node(askama_node);

        let root = match askama_node {
            AskamaNode::Control { tag, .. } => Root::Control {
                tag: *tag,
                end: None,
            },
            AskamaNode::Expression { .. } => Root::Expr,
            AskamaNode::Comment { .. } => Root::Comment,
        };

        Self::grow(root, content, askama_node.start(), askama_node.end())
    }

    fn from_html(html_node: &HtmlNode) -> Self {
        let content = html_node.format();
        let start = html_node.start();
        let end = html_node.range().map_or(start, |r| r.end);
        let root = match html_node {
            HtmlNode::Start { .. } => Root::Tag {
                indent: 0,
                is_phrasing: html_node.is_phrasing(),
                is_ws_sensitive: html_node.is_ws_sensitive(),
                end: None,
            },
            HtmlNode::Void { .. } | HtmlNode::SelfClosing { .. } | HtmlNode::Doctype { .. } => {
                Root::Tag {
                    indent: 0,
                    is_phrasing: html_node.is_phrasing(),
                    is_ws_sensitive: false,
                    end: None,
                }
            }
            HtmlNode::End { .. } | HtmlNode::ErroneousEnd { .. } => Root::Tag {
                indent: -1,
                is_phrasing: html_node.is_phrasing(),
                is_ws_sensitive: html_node.is_ws_sensitive(),
                end: None,
            },
            HtmlNode::Text { .. } => Root::Text,
            HtmlNode::Entity { .. } => Root::Entity,
            HtmlNode::Raw { .. } => Root::Raw,
            HtmlNode::Comment { .. } => Root::Comment,
        };

        Self::grow(root, content, start, end)
    }

    fn is_ctrl(&self) -> bool {
        matches!(self.root, Root::Control { .. })
    }

    fn is_phrasing(&self) -> bool {
        matches!(
            &self.root,
            Root::Tag {
                is_phrasing: true,
                ..
            } | Root::Text
                | Root::Entity
                | Root::Expr
        )
    }

    fn can_be_inline(&self) -> bool {
        self.is_phrasing() || (self.is_ctrl() && !self.ws_before && !self.ws_after)
    }

    fn from_text(text: &str, start: usize, end: usize) -> Self {
        Self::grow(Root::Text, crate::normalize_ws(text), start, end)
    }

    fn from_mixed_text(
        range: &Range<usize>,
        askama_nodes: &[AskamaNode],
        embed_askama: &[usize],
        source: &str,
    ) -> Vec<Self> {
        let mut segments = Vec::new();
        let mut curr_pos = range.start;

        for &idx in embed_askama {
            let node = &askama_nodes[idx];
            if node.start() > curr_pos {
                segments.push((curr_pos, node.start()));
            }
            curr_pos = node.end();
        }

        if curr_pos < range.end {
            segments.push((curr_pos, range.end));
        }

        segments
            .into_iter()
            .map(|(start, end)| Self::from_text(&source[start..end], start, end))
            .collect()
    }
}

impl SakuraTree {
    pub fn grow(
        askama_nodes: &[AskamaNode],
        html_nodes: &[HtmlNode],
        source: &str,
        cfg: &Config,
    ) -> Self {
        let leaves = Self::grow_leaves(askama_nodes, html_nodes, source);

        let mut tree = Self {
            cfg: cfg.clone(),
            leaves,
            branches: Vec::new(),
            indent_map: Vec::new(),
        };

        tree.generate_indent_map();

        let rings = tree.grow_rings(0, tree.leaves.len());

        for ring in rings {
            tree.grow_branch_recursive(&ring);
        }

        tree
    }

    fn grow_leaves(askama_nodes: &[AskamaNode], html_nodes: &[HtmlNode], src: &str) -> Vec<Leaf> {
        let (mut leaves, pruned) = Self::leaves_from_html(html_nodes, askama_nodes, src);
        leaves.extend(Self::leaves_from_askama(askama_nodes, &pruned));

        let mut leaves: Vec<Leaf> = leaves.into_iter().collect();

        let preserves: Vec<bool> = leaves
            .iter()
            .map(|l| {
                l.is_phrasing()
                    || l.is_ctrl()
                    || matches!(
                        l.root,
                        Root::Tag {
                            is_ws_sensitive: true,
                            ..
                        }
                    )
            })
            .collect();

        for (i, leaf) in leaves.iter_mut().enumerate().filter(|(i, _)| preserves[*i]) {
            leaf.ws_before = preserves.get(i.wrapping_sub(1)).copied().unwrap_or(false)
                && Self::source_has_ws(src, leaf.start.wrapping_sub(1));

            leaf.ws_after = preserves.get(i + 1).copied().unwrap_or(false)
                && Self::source_has_ws(src, leaf.end);
        }

        for node in askama_nodes {
            if let AskamaNode::Control { end, range, .. } = node
                && let Some(start) = leaves.iter().position(|l| l.start == range.start)
                && let end_idx = leaves.iter().position(|l| Some(l.start) == *end)
                && let Root::Control { end, .. } = &mut leaves[start].root
            {
                *end = end_idx;
            }
        }

        for html_node in html_nodes {
            if let (Some(range), Some(end)) = (html_node.range(), html_node.end())
                && let Some(start) = leaves.iter().position(|l| l.start == range.start)
                && let Some(end_node) = html_nodes.get(end)
                && askama::is_inside_same_ctrl(range.start, end_node.start(), askama_nodes) // TODO: remove this
                && let end_idx = leaves.iter().position(|l| l.start == end_node.start())
                && let Root::Tag { end, indent, .. } = &mut leaves[start].root
            {
                *end = end_idx;
                *indent = 1;
            }
        }

        leaves
    }

    fn leaves_from_askama(askama_nodes: &[AskamaNode], pruned: &HashSet<usize>) -> BTreeSet<Leaf> {
        askama_nodes
            .iter()
            .enumerate()
            .filter(|(idx, _)| !pruned.contains(idx))
            .map(|(_, node)| Leaf::from_askama(node))
            .collect()
    }

    fn leaves_from_html(
        html_nodes: &[HtmlNode],
        askama_nodes: &[AskamaNode],
        source: &str,
    ) -> (BTreeSet<Leaf>, HashSet<usize>) {
        let mut leaves = BTreeSet::new();
        let mut pruned = HashSet::new();

        for node in html_nodes {
            match node {
                HtmlNode::Start { .. } | HtmlNode::Void { .. } | HtmlNode::SelfClosing { .. } => {
                    let Some(range) = node.range() else { continue };

                    if let Some(embed) = node.embed_askm() {
                        pruned.extend(embed.iter().copied());
                        let root = Root::Tag {
                            indent: 0,
                            is_phrasing: node.is_phrasing(),
                            is_ws_sensitive: node.is_ws_sensitive(),
                            end: None,
                        };
                        let content = html::format_tag(range, source, askama_nodes, embed);
                        leaves.insert(Leaf::grow(root, content, range.start, range.end));
                    } else {
                        leaves.insert(Leaf::from_html(node));
                    }
                }
                HtmlNode::Text { text, .. } => {
                    let Some(range) = node.range() else { continue };

                    if let Some(embed) = node.embed_askm() {
                        leaves.extend(Leaf::from_mixed_text(range, askama_nodes, embed, source));
                    } else {
                        leaves.insert(Leaf::from_text(text, range.start, range.end));
                    }
                }
                HtmlNode::Raw { .. } | HtmlNode::Comment { .. } => {
                    let Some(range) = node.range() else { continue };

                    if let Some(embed) = node.embed_askm() {
                        pruned.extend(embed.iter().copied());
                        let root = match node {
                            HtmlNode::Raw { .. } => Root::Raw,
                            HtmlNode::Comment { .. } => Root::Comment,
                            _ => unreachable!(),
                        };
                        let content = html::format_opaque(range, source, askama_nodes, embed);
                        leaves.insert(Leaf::grow(root, content, range.start, range.end));
                    } else {
                        leaves.insert(Leaf::from_html(node));
                    }
                }
                _ => {
                    leaves.insert(Leaf::from_html(node));
                }
            }
        }

        (leaves, pruned)
    }

    fn source_has_ws(src: &str, pos: usize) -> bool {
        src.as_bytes().get(pos).is_some_and(u8::is_ascii_whitespace)
    }

    fn generate_indent_map(&mut self) {
        let mut curr_indent: usize = 0;

        for leaf in &self.leaves {
            let (pre_delta, post_delta) = match &leaf.root {
                Root::Control { tag, .. } => tag.indent(),
                Root::Tag { indent, .. } => {
                    if *indent < 0 {
                        (*indent, 0)
                    } else {
                        (0, *indent)
                    }
                }
                _ => (0, 0),
            };

            curr_indent = curr_indent.saturating_add_signed(pre_delta);
            self.indent_map.push(curr_indent);
            curr_indent = curr_indent.saturating_add_signed(post_delta);
        }
    }

    fn grow_branch(&mut self, start: usize, end: usize, style: &Style) {
        let indent = self.indent_map[start];

        let lines = match style {
            Style::Inline => vec![self.branch_content(start, end)],
            Style::Wrapped => self.render_wrapped(start, end),
            Style::Comment => self.render_comment(start, end),
            Style::Raw => self.render_raw(start, end),
        };

        self.branches.push(Branch { indent, lines });
    }

    fn grow_rings(&self, start_idx: usize, end_idx: usize) -> Vec<Ring> {
        let mut rings = Vec::new();
        let mut start = start_idx;

        while start < end_idx {
            let leaf = &self.leaves[start];

            let (ring, last) = match leaf.root {
                Root::Control {
                    tag: ControlTag::When | ControlTag::MatchElse,
                    ..
                } => {
                    let mut curr = start + 1;
                    while curr < end_idx && !self.leaves.get(curr).is_none_or(Leaf::is_ctrl) {
                        curr = self.leaves[curr].pair().map_or(curr + 1, |end| end + 1);
                    }
                    let end = if self.fits(start, curr - 1) {
                        curr - 1
                    } else {
                        start
                    };
                    (Ring::MatchArm { start, end }, end)
                }
                Root::Raw => (Ring::Raw(start), start),
                Root::Comment => (Ring::Comment(start), start),
                _ if leaf.can_be_inline()
                    && (leaf.pair().is_none() || !leaf.ws_after)
                    && leaf.pair().is_none_or(|end| {
                        self.leaves[start + 1..end].iter().all(Leaf::can_be_inline)
                    }) =>
                {
                    let (mut curr, mut end) = (start, start);
                    while curr < end_idx {
                        let curr_leaf = &self.leaves[curr];
                        if !curr_leaf.can_be_inline()
                            || (curr > start
                                && curr_leaf
                                    .pair()
                                    .is_some_and(|idx| !self.fits(curr, idx) && curr_leaf.ws_after))
                        {
                            break;
                        }
                        end = curr_leaf.pair().unwrap_or(curr);
                        if curr_leaf.is_ctrl() && self.leaves[end].ws_after {
                            break;
                        }
                        curr = end + 1;
                    }
                    (Ring::Phrasing { start, end }, end)
                }
                _ => match leaf.pair() {
                    Some(end) => {
                        let inner = self.grow_rings(start + 1, end);
                        let mut trailing = end;
                        if leaf.is_phrasing() || leaf.is_ctrl() {
                            while let Some(next) = self.leaves.get(trailing + 1)
                                && next.is_phrasing()
                                && !next.ws_before
                            {
                                trailing += 1;
                            }
                        }
                        (
                            Ring::Block {
                                start,
                                end,
                                inner,
                                trailing,
                            },
                            trailing,
                        )
                    }
                    None => (Ring::Single(start), start),
                },
            };

            rings.push(ring);
            start = last + 1;
        }

        rings
    }

    fn grow_branch_recursive(&mut self, ring: &Ring) {
        match ring {
            Ring::Block {
                start,
                end,
                inner,
                trailing,
            } => {
                let is_ctrl = self.leaves[*start].is_ctrl();
                let start_has_ws = self.leaves[*start].ws_after;
                let end_has_ws = self.leaves[*end].ws_after;

                if inner.iter().all(|r| matches!(r, Ring::Phrasing { .. }))
                    && (is_ctrl && !start_has_ws || !is_ctrl && self.fits(*start, *trailing))
                {
                    self.grow_branch(*start, *end, &Style::Inline);
                } else {
                    self.grow_branch(*start, *start, &Style::Inline);
                    for ring in inner {
                        self.grow_branch_recursive(ring);
                    }
                    if end_has_ws && trailing > end {
                        self.grow_branch(*end, *end, &Style::Inline);
                        self.grow_branch(*end + 1, *trailing, &Style::Wrapped);
                    } else {
                        self.grow_branch(*end, *trailing, &Style::Inline);
                    }
                }
            }
            Ring::Phrasing { start, end } => self.grow_branch(*start, *end, &Style::Wrapped),
            Ring::MatchArm { start, end } => self.grow_branch(*start, *end, &Style::Inline),
            Ring::Raw(idx) => self.grow_branch(*idx, *idx, &Style::Raw),
            Ring::Comment(idx) => self.grow_branch(*idx, *idx, &Style::Comment),
            Ring::Single(idx) => self.grow_branch(*idx, *idx, &Style::Inline),
        }
    }

    fn fits(&self, start: usize, end: usize) -> bool {
        self.indent_map[start] * self.cfg.indent_size + self.width(start, end) <= self.cfg.max_width
    }

    fn fits_2(&self, start: usize, line: &str, extra: usize) -> bool {
        self.indent_map[start] * self.cfg.indent_size + line.chars().count() + extra
            < self.cfg.max_width
    }

    fn width(&self, start: usize, end: usize) -> usize {
        (start..=end)
            .filter_map(|i| self.leaves.get(i))
            .map(|l| l.content.chars().count())
            .sum()
    }

    pub fn print(&self) -> String {
        let estimated_size = self
            .branches
            .iter()
            .map(|b| b.lines.iter().map(|l| l.len() + 1).sum::<usize>())
            .sum::<usize>();

        let mut output = String::with_capacity(estimated_size);

        for branch in &self.branches {
            let base_indent = self.indent_as_string(branch.indent);
            for line in &branch.lines {
                if line.is_empty() {
                    output.push('\n');
                } else {
                    output.push_str(&base_indent);
                    output.push_str(line);
                    output.push('\n');
                }
            }
        }

        output
    }

    fn branch_content(&self, start: usize, end: usize) -> String {
        let mut content = String::new();
        let mut prev_idx = None;

        for leaf_idx in start..=end {
            if let Some(leaf) = self.leaves.get(leaf_idx) {
                if let Some(prev) = prev_idx {
                    let has_ws =
                        self.leaves.get(prev).is_some_and(|l: &Leaf| l.ws_after) || leaf.ws_before;
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

    fn grow_leaflets(&self, branch_start: usize, branch_end: usize) -> Vec<Leaflet<'_>> {
        let leaves = &self.leaves[branch_start..=branch_end];
        let mut leaflets = Vec::new();
        let mut pairs = Vec::new();

        for leaf in leaves {
            pairs.push(leaflets.len());

            if leaf.root == Root::Text {
                for (i, content) in leaf.content.split_whitespace().enumerate() {
                    leaflets.push(Leaflet {
                        content,
                        ws_before: i > 0 || leaf.ws_before,
                        pair_end: None,
                    });
                }
            } else {
                leaflets.push(Leaflet {
                    content: leaf.content.as_str(),
                    ws_before: leaf.ws_before,
                    pair_end: None,
                });
            }
        }

        for (i, leaf) in leaves.iter().enumerate() {
            if let Some(leaf_pair) = leaf.pair().and_then(|p| p.checked_sub(branch_start))
                && let (Some(start), Some(end)) = (pairs.get(i), pairs.get(leaf_pair))
                && let Some(leaflet) = leaflets.get_mut(*start)
            {
                leaflet.pair_end = Some(*end);
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

    fn render_wrapped(&self, start: usize, end: usize) -> Vec<String> {
        let leaflets = self.grow_leaflets(start, end);
        let mut lines = Vec::new();
        let mut curr_line = String::new();
        let mut pair = None;

        for (i, leaflet) in leaflets.iter().enumerate() {
            let mut end = leaflets[i].pair_end.unwrap_or(i);

            while let Some(next) = leaflets.get(end + 1).filter(|n| !n.ws_before) {
                end = next.pair_end.unwrap_or(end + 1);
            }

            let curr_width = leaflets[i..=end]
                .iter()
                .enumerate()
                .map(|(i, ll)| ll.content.chars().count() + usize::from(i > 0 && ll.ws_before))
                .sum();

            if leaflet.ws_before
                && pair.is_none_or(|p| i > p)
                && !curr_line.is_empty()
                && !self.fits_2(start, &curr_line, curr_width)
            {
                lines.push(std::mem::take(&mut curr_line));
            }

            if leaflet.ws_before && !curr_line.is_empty() {
                curr_line.push(' ');
            }

            curr_line.push_str(leaflet.content);
            pair = pair.max(leaflet.pair_end);
        }

        lines.push(curr_line);
        lines
    }

    fn render_comment(&self, start: usize, end: usize) -> Vec<String> {
        let content = self.branch_content(start, end);
        let lines: Vec<&str> = content.lines().collect();
        let indent = self.indent_as_string(1);

        lines
            .iter()
            .enumerate()
            .map(|(i, line)| {
                let line = line.trim_start();
                if line.is_empty() || i == 0 || i == lines.len() - 1 {
                    line.to_string()
                } else {
                    format!("{}{}", indent, line)
                }
            })
            .collect()
    }

    fn render_raw(&self, start: usize, end: usize) -> Vec<String> {
        let content = self.branch_content(start, end);
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

    fn indent_as_string(&self, indent: usize) -> String {
        " ".repeat(indent * self.cfg.indent_size)
    }
}
