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
    RawText,
    Doctype,
}

#[derive(Debug, Clone)]
struct Branch {
    indent: usize,
    lines: Vec<String>,
}

#[derive(Debug, Clone)]
enum Style {
    Inline,
    WrappedText,
    WrappedSequence,
    Comment,
    Raw,
}

#[derive(Debug, Clone)]
enum Ring {
    Paired {
        start: usize,
        end: usize,
        inner: Vec<Ring>,
    },
    MatchArm {
        start: usize,
        end: usize,
    },
    TextSequence {
        start: usize,
        end: usize,
        inner: Vec<Ring>,
    },
    RawText {
        start: usize,
        end: usize,
    },
    Single(usize),
}

impl Ring {
    fn range(&self) -> (usize, usize) {
        match self {
            Ring::Paired { start, end, .. }
            | Ring::MatchArm { start, end }
            | Ring::TextSequence { start, end, .. }
            | Ring::RawText { start, end } => (*start, *end),
            Ring::Single(idx) => (*idx, *idx),
        }
    }
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

    fn from_askama(cfg: &Config, askama_node: &AskamaNode) -> Self {
        let content = askama::format_askama_node(cfg, askama_node);

        let root = match askama_node {
            AskamaNode::Control { ctrl_tag, .. } => Root::Control {
                tag: *ctrl_tag,
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
            HtmlNode::StartTag { .. } => Root::Tag {
                indent: 0,
                is_phrasing: html_node.is_phrasing(),
                is_ws_sensitive: html_node.is_ws_sensitive(),
                end: None,
            },
            HtmlNode::Void { .. } | HtmlNode::SelfClosingTag { .. } => Root::Tag {
                indent: 0,
                is_phrasing: html_node.is_phrasing(),
                is_ws_sensitive: false,
                end: None,
            },
            HtmlNode::EndTag { .. } | HtmlNode::ErroneousEndTag { .. } => Root::Tag {
                indent: -1,
                is_phrasing: html_node.is_phrasing(),
                is_ws_sensitive: html_node.is_ws_sensitive(),
                end: None,
            },
            HtmlNode::Text { .. } => Root::Text,
            HtmlNode::Entity { .. } => Root::Entity,
            HtmlNode::RawText { .. } => Root::RawText,
            HtmlNode::Comment { .. } => Root::Comment,
            HtmlNode::Doctype { .. } => Root::Doctype,
        };

        Self::grow(root, content, start, end)
    }

    fn chars_count(&self) -> usize {
        self.content.chars().count()
    }

    fn is_ctrl(&self) -> bool {
        matches!(self.root, Root::Control { .. })
    }

    fn preserves_ws(&self) -> bool {
        self.is_phrasing()
            || self.is_ctrl()
            || matches!(
                &self.root,
                Root::RawText
                    | Root::Tag {
                        is_ws_sensitive: true,
                        ..
                    }
            )
    }

    fn is_block_level(&self) -> bool {
        match &self.root {
            Root::Control { tag, .. } => tag.is_opening(),
            Root::Tag { is_phrasing, .. } => !is_phrasing,
            Root::Comment => self.content.contains('\n'),
            _ => false,
        }
    }

    fn is_phrasing(&self) -> bool {
        matches!(
            &self.root,
                | Root::Tag {
                    is_phrasing: true,
                    ..
                }
        ) | self.is_text_sequence()
    }

    fn is_text_sequence(&self) -> bool {
        matches!(&self.root, Root::Text | Root::Entity | Root::Expr)
    }

    fn from_text_or_raw(text: &str, is_raw: bool, start: usize, end: usize) -> Self {
        let (content, root) = if is_raw {
            (
                text.trim_matches('\n')
                    .lines()
                    .map(str::trim)
                    .collect::<Vec<_>>()
                    .join("\n"),
                Root::RawText,
            )
        } else {
            (crate::normalize_ws(text), Root::Text)
        };

        Self::grow(root, content, start, end)
    }

    fn split_text(
        range: &Range<usize>,
        askama_nodes: &[AskamaNode],
        embed_askm: &[usize],
        source: &str,
        is_raw: bool,
    ) -> Vec<Self> {
        let mut segments = Vec::new();
        let mut current_pos = range.start;

        for &idx in embed_askm {
            let askama = &askama_nodes[idx];
            if askama.start() > current_pos {
                segments.push((current_pos, askama.start()));
            }
            current_pos = askama.end();
        }

        if current_pos < range.end {
            segments.push((current_pos, range.end));
        }

        segments
            .into_iter()
            .map(|(start, end)| {
                let text = &source[start..end];
                Self::from_text_or_raw(text, is_raw, start, end)
            })
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
        let leaves = Self::grow_leaves(askama_nodes, html_nodes, source, cfg);

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

    fn grow_leaves(
        askama_nodes: &[AskamaNode],
        html_nodes: &[HtmlNode],
        source: &str,
        cfg: &Config,
    ) -> Vec<Leaf> {
        let (mut leaves, pruned) = Self::leaves_from_html(html_nodes, askama_nodes, source, cfg);
        leaves.extend(Self::leaves_from_askama(askama_nodes, &pruned, cfg));

        let mut leaves: Vec<Leaf> = leaves.into_iter().collect();

        Self::preserve_ws(&mut leaves, source);

        for node in askama_nodes {
            if let AskamaNode::Control {
                ctrl_tag,
                close_tag: Some(end),
                range,
                ..
            } = node
                && ctrl_tag.is_opening()
                && let Some(start) = leaves.iter().position(|l| l.start == range.start)
                && let Some(end_idx) = leaves.iter().position(|l| l.start == *end)
                && let Root::Control { end, .. } = &mut leaves[start].root
            {
                *end = Some(end_idx);
            }
        }

        for html_node in html_nodes {
            if let (Some(range), Some(end_tag)) = (html_node.range(), html_node.end_tag_idx())
                && let Some(end_node) = html_nodes.get(end_tag)
                && let Some(start) = leaves.iter().position(|l| l.start == range.start)
                && let Some(end_idx) = leaves.iter().position(|l| l.start == end_node.start())
                && askama::is_inside_same_ctrl(range.start, end_node.start(), askama_nodes)
                && let Root::Tag { end, indent, .. } = &mut leaves[start].root
            {
                *end = Some(end_idx);
                *indent = 1;
            }
        }

        leaves
    }

    fn leaves_from_askama(
        askama_nodes: &[AskamaNode],
        pruned: &HashSet<usize>,
        cfg: &Config,
    ) -> BTreeSet<Leaf> {
        askama_nodes
            .iter()
            .enumerate()
            .filter(|(idx, _)| !pruned.contains(idx))
            .map(|(_, node)| Leaf::from_askama(cfg, node))
            .collect()
    }

    fn leaves_from_html(
        html_nodes: &[HtmlNode],
        askama_nodes: &[AskamaNode],
        source: &str,
        cfg: &Config,
    ) -> (BTreeSet<Leaf>, HashSet<usize>) {
        let mut leaves = BTreeSet::new();
        let mut pruned = HashSet::new();

        for node in html_nodes {
            match node {
                HtmlNode::StartTag { .. }
                | HtmlNode::Void { .. }
                | HtmlNode::SelfClosingTag { .. } => {
                    let Some(range) = node.range() else { continue };

                    if let Some(embed) = node.embed_askm() {
                        pruned.extend(embed.iter().copied());
                        let content = html::format_tag(range, source, askama_nodes, embed, cfg);
                        let root = Root::Tag {
                            indent: 0,
                            is_phrasing: node.is_phrasing(),
                            is_ws_sensitive: node.is_ws_sensitive(),
                            end: None,
                        };
                        leaves.insert(Leaf::grow(root, content, range.start, range.end));
                    } else {
                        leaves.insert(Leaf::from_html(node));
                    }
                }
                HtmlNode::Text { text, .. } | HtmlNode::RawText { text, .. } => {
                    let is_raw = matches!(node, HtmlNode::RawText { .. });
                    let Some(range) = node.range() else { continue };

                    if let Some(embed) = node.embed_askm() {
                        leaves.extend(Leaf::split_text(range, askama_nodes, embed, source, is_raw));
                    } else {
                        leaves.insert(Leaf::from_text_or_raw(text, is_raw, range.start, range.end));
                    }
                }
                HtmlNode::Comment { .. } => {
                    let Some(range) = node.range() else { continue };

                    if let Some(embed) = node.embed_askm() {
                        pruned.extend(embed.iter().copied());
                        let content = html::format_comment(range, source, askama_nodes, embed, cfg);
                        leaves.insert(Leaf::grow(Root::Comment, content, range.start, range.end));
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

    fn preserve_ws(leaves: &mut [Leaf], source: &str) {
        for i in 0..leaves.len() {
            let leaf = &leaves[i];

            if !leaf.preserves_ws() {
                continue;
            }

            // Check if the node itself has internal leading/trailing whitespace
            let node_src = &source[leaf.start..leaf.end];
            let ws_start = node_src.starts_with(char::is_whitespace);
            let ws_end = node_src.ends_with(char::is_whitespace);

            // Get adjacent nodes for context
            let prev = i.checked_sub(1).map(|p| &leaves[p]);
            let next = leaves.get(i + 1);

            // Check if there is whitespace between nodes
            let gap_before =
                prev.is_some_and(|p| source[p.end..leaf.start].contains(char::is_whitespace));
            let gap_after =
                next.is_some_and(|n| source[leaf.end..n.start].contains(char::is_whitespace));

            // Whitespace exists if it's internal to the node OR between nodes
            let ws_before = ws_start || gap_before;
            let ws_after = ws_end || gap_after;

            let (before, after) = if leaf.is_ctrl() {
                (ws_before, ws_after)
            } else {
                (
                    ws_before && prev.is_some_and(Leaf::preserves_ws),
                    ws_after && next.is_some_and(Leaf::preserves_ws),
                )
            };

            leaves[i].ws_before = before;
            leaves[i].ws_after = after;
        }
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
            Style::WrappedText => self.render_wrapped_text(start, end, indent),
            Style::WrappedSequence => self.render_wrapped_sequence(start, end, indent),
            Style::Comment => self.render_comment(start, end),
            Style::Raw => self.render_raw(start, end),
        };

        self.branches.push(Branch { indent, lines });
    }

    // Grow concentric rings
    fn grow_rings(&self, start_idx: usize, end_idx: usize) -> Vec<Ring> {
        let mut rings = Vec::new();
        let mut start = start_idx;

        while start < end_idx {
            let leaf = &self.leaves[start];
            let fallback_to_single_ring = || (Ring::Single(start), start);

            let (ring, last) = match &leaf.root {
                Root::Control { tag, .. } if tag.is_match_arm() => {
                    let mut curr = start + 1;

                    while curr < end_idx {
                        let Some(leaf) = self.leaves.get(curr) else {
                            break;
                        };
                        if leaf.is_ctrl() {
                            break;
                        }
                        curr = leaf.pair().map_or(curr + 1, |end| end + 1);
                    }

                    let end = if self.fits(start, curr - 1) {
                        curr - 1
                    } else {
                        start
                    };
                    (Ring::MatchArm { start, end }, end)
                }
                Root::Control { tag, .. } if tag.is_opening() => {
                    if let Some(end) = leaf.pair() {
                        let inner = self.grow_rings(start + 1, end);
                        (Ring::Paired { start, end, inner }, end)
                    } else {
                        fallback_to_single_ring()
                    }
                }
                Root::Tag { .. } => {
                    if let Some(end) = leaf.pair() {
                        if leaf.is_phrasing()
                            && self.leaves.get(end + 1).is_some_and(Leaf::is_text_sequence)
                            && let Some((ring, next)) = self.try_text_sequence(start, end_idx)
                        {
                            (ring, next)
                        } else {
                            let inner = self.grow_rings(start + 1, end);
                            (Ring::Paired { start, end, inner }, end)
                        }
                    } else {
                        fallback_to_single_ring()
                    }
                }
                Root::RawText => {
                    let count = self.leaves[start..end_idx]
                        .iter()
                        .take_while(|l| matches!(l.root, Root::RawText | Root::Expr))
                        .count();
                    let end = start + count - 1;
                    (Ring::RawText { start, end }, end)
                }
                Root::Text | Root::Entity | Root::Expr => self
                    .try_text_sequence(start, end_idx)
                    .unwrap_or_else(fallback_to_single_ring),
                _ => fallback_to_single_ring(),
            };

            rings.push(ring);
            start = last + 1;
        }

        rings
    }

    fn grow_inner_rings(&self, start_idx: usize, end_idx: usize) -> Vec<Ring> {
        self.grow_rings(start_idx + 1, end_idx)
    }

    fn try_text_sequence(&self, start_idx: usize, end_idx: usize) -> Option<(Ring, usize)> {
        let mut last_idx = start_idx;
        let mut start = start_idx;
        let mut inner_rings = Vec::new();

        // Collect following phrasing content
        while start < end_idx {
            let leaf = &self.leaves[start];

            if leaf.is_phrasing() {
                if let Some(end) = leaf.pair() {
                    // Check if complete element fits inline
                    if !self.fits(start, end) {
                        if start == start_idx {
                            return None;
                        }
                        break;
                    }

                    let inner = self.grow_inner_rings(start, end);
                    if !inner.is_empty() {
                        inner_rings.push(Ring::TextSequence { start, end, inner });
                    }

                    last_idx = end;
                    start = end + 1;
                    continue;
                }

                inner_rings.push(Ring::Single(start));
                last_idx = start;
                start += 1;
            } else {
                if start == start_idx {
                    return None;
                }
                break;
            }
        }

        (last_idx >= start_idx).then_some({
            (
                Ring::TextSequence {
                    start: start_idx,
                    end: last_idx,
                    inner: inner_rings,
                },
                last_idx,
            )
        })
    }

    fn grow_branch_recursive(&mut self, ring: &Ring) {
        let (start, end) = ring.range();

        let fits = self.indent_width(start) + self.width(start, end) <= self.cfg.max_width;

        match ring {
            Ring::Paired { start, end, inner } => {
                let has_block = self.has_block_inner_rings(inner);

                if fits && !has_block {
                    self.grow_branch(*start, *end, &Style::Inline);
                } else {
                    self.grow_branch(*start, *start, &Style::Inline);
                    for ring in inner {
                        self.grow_branch_recursive(ring);
                    }
                    self.grow_branch(*end, *end, &Style::Inline);
                }
            }
            Ring::TextSequence { start, end, inner } => {
                if fits {
                    self.grow_branch(*start, *end, &Style::Inline);
                } else {
                    let mut has_expr = false;
                    let mut all_text_seq = true;

                    for i in *start..=*end {
                        if let Some(leaf) = self.leaves.get(i) {
                            if !leaf.is_text_sequence() {
                                all_text_seq = false;
                                break;
                            }
                            if matches!(leaf.root, Root::Expr) {
                                has_expr = true;
                            }
                        }
                    }

                    if !all_text_seq {
                        self.split_text_sequence(*start, inner);
                    } else if has_expr {
                        self.grow_branch(*start, *end, &Style::WrappedSequence);
                    } else {
                        self.grow_branch(*start, *end, &Style::WrappedText);
                    }
                }
            }
            Ring::RawText { start, end } => {
                self.grow_branch(*start, *end, &Style::Raw);
            }
            Ring::MatchArm { start, end } => {
                self.grow_branch(*start, *end, &Style::Inline);
            }
            Ring::Single(idx) => match &self.leaves[*idx].root {
                Root::Comment => {
                    self.grow_branch(*idx, *idx, &Style::Comment);
                }
                _ => self.grow_branch(*idx, *idx, &Style::Inline),
            },
        }
    }

    fn has_block_inner_rings(&self, inner: &[Ring]) -> bool {
        inner.iter().any(|ring| self.is_ring_block_level(ring))
    }

    fn is_ring_block_level(&self, ring: &Ring) -> bool {
        let first_leaf = &self.leaves[ring.range().0];

        match ring {
            Ring::Paired { .. } | Ring::Single { .. } => first_leaf.is_block_level(),
            Ring::RawText { .. } => true,
            Ring::TextSequence { .. } | Ring::MatchArm { .. } => false,
        }
    }

    fn leaflets_from_leaf(
        &self,
        leaf: &Leaf,
        current: &mut String,
        leaflets: &mut Vec<(String, bool)>,
    ) {
        match &leaf.root {
            Root::Text => {
                let words: Vec<&str> = leaf.content.split_whitespace().collect();

                for (word_idx, word) in words.iter().enumerate() {
                    let is_first_word = word_idx == 0;
                    let is_last_word = word_idx == words.len() - 1;

                    if is_first_word && leaf.ws_before && !current.is_empty() {
                        leaflets.push((current.clone(), true));
                        current.clear();
                    }

                    if !is_first_word && !current.is_empty() {
                        leaflets.push((current.clone(), true));
                        current.clear();
                    }

                    current.push_str(word);

                    if is_last_word && leaf.ws_after {
                        leaflets.push((current.clone(), true));
                        current.clear();
                    }
                }
            }
            Root::Expr | Root::Entity | Root::Tag { .. } => {
                if leaf.ws_before && !current.is_empty() {
                    leaflets.push((current.clone(), true));
                    current.clear();
                }

                current.push_str(&leaf.content);

                if leaf.ws_after {
                    leaflets.push((current.clone(), true));
                    current.clear();
                }
            }
            _ => {}
        }
    }

    fn leaflets_from_rings(&self, inner: &[Ring]) -> Vec<(String, bool)> {
        let mut leaflets: Vec<(String, bool)> = Vec::new();
        let mut current = String::new();

        for ring in inner {
            let (ring_start, ring_end) = ring.range();

            match ring {
                Ring::Single(idx) => {
                    let leaf = &self.leaves[*idx];
                    self.leaflets_from_leaf(leaf, &mut current, &mut leaflets);
                }
                Ring::Paired { .. } | Ring::TextSequence { .. } => {
                    let first_leaf = &self.leaves[ring_start];
                    let last_leaf = &self.leaves[ring_end];

                    if first_leaf.ws_before && !current.is_empty() {
                        leaflets.push((current, true));
                        current = String::new();
                    }

                    current.push_str(&self.branch_content(ring_start, ring_end));

                    let has_ws_after = last_leaf.ws_after;
                    if !current.is_empty() {
                        leaflets.push((current, has_ws_after));
                        current = String::new();
                    }
                }
                _ => {}
            }
        }

        if !current.is_empty() {
            leaflets.push((current, false));
        }

        leaflets
    }

    fn split_text_sequence(&mut self, start: usize, inner: &[Ring]) {
        let leaflets = self.leaflets_from_rings(inner);

        if leaflets.is_empty() {
            return;
        }

        let indent = self.indent_map[start];
        let indent_width = indent * self.cfg.indent_size;
        let available_width = self.cfg.max_width.saturating_sub(indent_width);

        let lines = self.wrap_leaflets(leaflets, available_width);
        self.branches.push(Branch { indent, lines });
    }

    fn fits(&self, start: usize, end: usize) -> bool {
        self.width(start, end) <= self.cfg.max_width
    }

    fn width(&self, start: usize, end: usize) -> usize {
        (start..=end)
            .filter_map(|i| self.leaves.get(i))
            .map(Leaf::chars_count)
            .sum()
    }

    fn has_ws(&self, prev_idx: usize, curr_idx: usize) -> bool {
        let after_prev = self.leaves.get(prev_idx).is_some_and(|l| l.ws_after);
        let before_curr = self.leaves.get(curr_idx).is_some_and(|l| l.ws_before);
        after_prev || before_curr
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
                if let Some(prev) = prev_idx
                    && self.has_ws(prev, leaf_idx)
                {
                    content.push(' ');
                }
                content.push_str(&leaf.content);
                prev_idx = Some(leaf_idx);
            }
        }

        content
    }

    fn render_wrapped_text(&self, start: usize, end: usize, indent: usize) -> Vec<String> {
        use textwrap::{Options, wrap};

        let content = self.branch_content(start, end);
        let indent_width = indent * self.cfg.indent_size;
        let available_width = self.cfg.max_width.saturating_sub(indent_width);
        let options = Options::new(available_width);

        wrap(&content, &options)
            .into_iter()
            .map(std::borrow::Cow::into_owned)
            .collect()
    }

    fn grow_leaflets(&self, start: usize, end: usize) -> Vec<(String, bool)> {
        let mut leaflets: Vec<(String, bool)> = Vec::new();
        let mut current = String::new();

        for i in start..=end {
            let leaf = &self.leaves[i];

            if !leaf.is_text_sequence() {
                continue;
            }

            self.leaflets_from_leaf(leaf, &mut current, &mut leaflets);
        }

        if !current.is_empty() {
            leaflets.push((current, false));
        }

        leaflets
    }

    fn wrap_leaflets(&self, leaflets: Vec<(String, bool)>, available_width: usize) -> Vec<String> {
        let mut lines: Vec<String> = Vec::new();
        let mut current_line = String::new();
        let mut current_width: usize = 0;
        let mut prev_has_space = false;

        for (leaflet, has_space_after) in leaflets {
            let leaflet_width = leaflet.chars().count();
            let need_space = current_width > 0 && prev_has_space;
            let projected_width = current_width + usize::from(need_space) + leaflet_width;

            if projected_width > available_width && current_width > 0 {
                lines.push(current_line);
                current_line = String::new();
                current_width = 0;
            }

            if current_width > 0 && prev_has_space {
                current_line.push(' ');
                current_width += 1;
            }

            current_line.push_str(&leaflet);
            current_width += leaflet_width;
            prev_has_space = has_space_after;
        }

        if !current_line.is_empty() {
            lines.push(current_line);
        }

        lines
    }

    fn render_wrapped_sequence(&self, start: usize, end: usize, indent: usize) -> Vec<String> {
        let indent_width = indent * self.cfg.indent_size;
        let available_width = self.cfg.max_width.saturating_sub(indent_width);
        let leaflets = self.grow_leaflets(start, end);
        self.wrap_leaflets(leaflets, available_width)
    }

    fn render_comment(&self, start: usize, end: usize) -> Vec<String> {
        let content = self.branch_content(start, end);

        if !content.contains('\n') {
            return vec![content];
        }

        let source_lines: Vec<&str> = content.lines().collect();
        let relative_indent = self.indent_as_string(1);
        let mut formatted_lines = Vec::new();

        for (i, line) in source_lines.iter().enumerate() {
            let trimmed = line.trim_start();

            if trimmed.is_empty() {
                formatted_lines.push(String::new());
            } else if i == 0 || i == source_lines.len() - 1 {
                formatted_lines.push(trimmed.to_string());
            } else {
                formatted_lines.push(format!("{}{}", relative_indent, trimmed));
            }
        }

        formatted_lines
    }

    fn render_raw(&self, start: usize, end: usize) -> Vec<String> {
        let content = self.branch_content(start, end);
        let mut lines = Vec::new();
        let mut curr_indent: usize = 0;

        for line in content.lines() {
            if line.is_empty() {
                lines.push(String::new());
                continue;
            }

            let leading_close = usize::from(line.starts_with('}'));
            curr_indent = curr_indent.saturating_sub(leading_close);

            let indent_str = self.indent_as_string(curr_indent);
            lines.push(format!("{}{}", indent_str, line));

            let open_braces = line.matches('{').count() as isize;
            let close_braces = line.matches('}').count() as isize;
            let net_change = open_braces - close_braces + leading_close as isize;
            curr_indent = curr_indent.saturating_add_signed(net_change);
        }

        lines
    }

    fn indent_as_string(&self, indent: usize) -> String {
        " ".repeat(indent * self.cfg.indent_size)
    }

    fn indent_width(&self, idx: usize) -> usize {
        self.indent_map[idx] * self.cfg.indent_size
    }
}
