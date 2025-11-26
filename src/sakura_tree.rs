use std::collections::{BTreeMap, HashMap, HashSet};
use std::ops::Range;

use crate::{
    askama::{self, AskamaNode, ControlTag},
    config::Config,
    html::{self, HtmlNode},
};

#[derive(Debug, Clone)]
pub struct SakuraTree {
    pub config: Config,
    pub leaves: Vec<Leaf>,
    pub branches: Vec<Branch>,
    indent_map: Vec<i32>,
    spacing_map: Vec<(bool, bool)>, // (space_before, space_after)
}

#[derive(Debug, Clone)]
pub enum Leaf {
    AskamaControl {
        content: String,
        tag: ControlTag,
        end_idx: Option<usize>,
    },
    AskamaExpr(String),
    AskamaComment(String),

    HtmlStartTag {
        content: String,
        is_phrasing: bool,
        is_whitespace_sensitive: bool,
        end_idx: Option<usize>,
    },
    HtmlVoidTag {
        content: String,
        is_phrasing: bool,
    },
    HtmlEndTag {
        content: String,
        is_phrasing: bool,
        is_whitespace_sensitive: bool,
    },

    HtmlText(String),
    HtmlEntity(String),
    HtmlRawText(String),
    HtmlComment(String),
    HtmlDoctype(String),
}

#[derive(Debug, Clone)]
pub struct Branch {
    pub start: usize,
    pub end: usize,
    pub style: BranchStyle,
    pub indent: i32,
}

#[derive(Debug, Clone)]
pub enum BranchStyle {
    Inline,
    WrappedText,
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
    fn pair(&self) -> Option<usize> {
        match self {
            Self::AskamaControl {
                end_idx: Some(end), ..
            }
            | Self::HtmlStartTag {
                end_idx: Some(end), ..
            } => Some(*end),
            _ => None,
        }
    }

    fn from_askama(config: &Config, askama_node: &AskamaNode) -> Self {
        let content = askama::format_askama_node(config, askama_node);

        match askama_node {
            AskamaNode::Control { ctrl_tag, .. } => Self::AskamaControl {
                content,
                tag: *ctrl_tag,
                end_idx: None,
            },
            AskamaNode::Expression { .. } => Self::AskamaExpr(content),
            AskamaNode::Comment { .. } => Self::AskamaComment(content),
        }
    }

    fn from_html(html_node: &HtmlNode) -> Self {
        let source = html_node.format();
        match html_node {
            HtmlNode::StartTag { .. } => Self::HtmlStartTag {
                content: source,
                is_phrasing: html_node.is_phrasing(),
                is_whitespace_sensitive: html_node.is_whitespace_sensitive(),
                end_idx: None,
            },
            HtmlNode::Void { .. } | HtmlNode::SelfClosingTag { .. } => Self::HtmlVoidTag {
                content: source,
                is_phrasing: html_node.is_phrasing(),
            },
            HtmlNode::EndTag { .. } | HtmlNode::ErroneousEndTag { .. } => Self::HtmlEndTag {
                content: source,
                is_phrasing: html_node.is_phrasing(),
                is_whitespace_sensitive: html_node.is_whitespace_sensitive(),
            },
            HtmlNode::Text { .. } => Self::HtmlText(source),
            HtmlNode::Entity { .. } => Self::HtmlEntity(source),
            HtmlNode::RawText { .. } => Self::HtmlRawText(source),
            HtmlNode::Comment { .. } => Self::HtmlComment(source),
            HtmlNode::Doctype { .. } => Self::HtmlDoctype(source),
        }
    }

    fn is_phrasing(&self) -> bool {
        matches!(
            self,
            Self::HtmlStartTag {
                is_phrasing: true,
                ..
            } | Self::HtmlVoidTag {
                is_phrasing: true,
                ..
            } | Self::HtmlEndTag {
                is_phrasing: true,
                ..
            } | Self::HtmlText(_)
                | Self::HtmlEntity(_)
                | Self::AskamaExpr(_)
        )
    }

    pub fn content(&self) -> &str {
        match self {
            Self::AskamaControl { content, .. }
            | Self::AskamaExpr(content)
            | Self::AskamaComment(content)
            | Self::HtmlStartTag { content, .. }
            | Self::HtmlVoidTag { content, .. }
            | Self::HtmlEndTag { content, .. }
            | Self::HtmlText(content)
            | Self::HtmlEntity(content)
            | Self::HtmlRawText(content)
            | Self::HtmlComment(content)
            | Self::HtmlDoctype(content) => content,
        }
    }

    pub fn chars_count(&self) -> usize {
        self.content().chars().count()
    }

    fn is_ctrl(&self) -> bool {
        matches!(self, Self::AskamaControl { .. })
    }

    fn is_start_tag(&self) -> bool {
        matches!(self, Self::HtmlStartTag { .. })
    }

    fn is_end_tag(&self) -> bool {
        matches!(self, Self::HtmlEndTag { .. })
    }

    fn preserves_whitespace(&self) -> bool {
        self.is_phrasing()
            || self.is_ctrl()
            || matches!(
                self,
                Self::HtmlRawText(_)
                    | Self::HtmlStartTag {
                        is_whitespace_sensitive: true,
                        ..
                    }
                    | Self::HtmlEndTag {
                        is_whitespace_sensitive: true,
                        ..
                    }
            )
    }

    fn is_text_sequence(&self) -> bool {
        matches!(
            self,
            Self::HtmlText(_) | Self::HtmlEntity(_) | Self::AskamaExpr(_)
        )
    }

    fn is_block_level(&self) -> bool {
        match self {
            Self::AskamaControl { tag, .. } => tag.is_opening(),
            Self::HtmlStartTag { is_phrasing, .. } => !is_phrasing,
            Self::AskamaComment(content) | Self::HtmlComment(content) => content.contains('\n'),
            _ => false,
        }
    }

    fn from_text(text: &str, is_raw: bool) -> Self {
        if is_raw {
            // Normalize raw text by trimming newlines and normalizing line-by-line
            // Preserves blank lines (empty after trimming) to maintain user formatting
            Self::HtmlRawText(
                text.trim_matches('\n')
                    .lines()
                    .map(str::trim)
                    .collect::<Vec<_>>()
                    .join("\n"),
            )
        } else {
            Self::HtmlText(crate::normalize_whitespace(text))
        }
    }
}

impl Branch {
    pub fn grow(start: usize, end: usize, style: BranchStyle, indent: i32) -> Self {
        Self {
            start,
            end,
            style,
            indent,
        }
    }
}

impl SakuraTree {
    pub fn grow(
        askama_nodes: &[AskamaNode],
        html_nodes: &[HtmlNode],
        source: &str,
        config: &Config,
    ) -> Self {
        let leaves = Self::grow_leaves(askama_nodes, html_nodes, source, config);

        let spacing_map = Self::generate_spacing_map(&leaves, source);

        let (leaves, byte_to_leaf_map): (Vec<_>, HashMap<_, _>) = leaves
            .into_iter()
            .enumerate()
            .map(|(i, (start, (leaf, _)))| (leaf, (start, i)))
            .unzip();

        let mut tree = Self {
            config: config.clone(),
            leaves,
            branches: Vec::new(),
            indent_map: Vec::new(),
            spacing_map,
        };

        tree.pair_askama_controls(askama_nodes, &byte_to_leaf_map);
        tree.pair_html_elements(html_nodes, askama_nodes, &byte_to_leaf_map);

        tree.indent_map = tree.generate_indent_map();
        let rings = tree.grow_rings(0, tree.leaves.len());

        for ring in rings {
            tree.grow_branch(&ring);
        }

        tree
    }

    fn grow_leaves(
        askama_nodes: &[AskamaNode],
        html_nodes: &[HtmlNode],
        source: &str,
        config: &Config,
    ) -> Vec<(usize, (Leaf, usize))> {
        let (mut btree, pruned) = Self::leaves_from_html(html_nodes, askama_nodes, source, config);
        btree.extend(Self::leaves_from_askama(askama_nodes, &pruned, config));
        btree.into_iter().collect()
    }

    fn leaves_from_askama(
        askama_nodes: &[AskamaNode],
        pruned: &HashSet<usize>,
        config: &Config,
    ) -> BTreeMap<usize, (Leaf, usize)> {
        let mut leaves = BTreeMap::new();

        for (idx, node) in askama_nodes.iter().enumerate() {
            if !pruned.contains(&idx) {
                leaves.insert(node.start(), (Leaf::from_askama(config, node), node.end()));
            }
        }

        leaves
    }

    fn leaves_from_html(
        html_nodes: &[HtmlNode],
        askama_nodes: &[AskamaNode],
        source: &str,
        config: &Config,
    ) -> (BTreeMap<usize, (Leaf, usize)>, HashSet<usize>) {
        let mut leaves = BTreeMap::new();
        let mut pruned = HashSet::new();

        for node in html_nodes {
            if node.is_start_tag_or_void() {
                let range = node.range().expect("tag must have range");

                let leaf = if let Some(embed_askm) = node.embed_askm() {
                    pruned.extend(embed_askm.iter().copied());

                    let content =
                        html::reconstruct_tag(range, source, askama_nodes, embed_askm, config);
                    let is_phrasing = node.is_phrasing();
                    let is_whitespace_sensitive = node.is_whitespace_sensitive();
                    match node {
                        HtmlNode::StartTag { .. } => Leaf::HtmlStartTag {
                            content,
                            is_phrasing,
                            is_whitespace_sensitive,
                            end_idx: None,
                        },
                        _ => Leaf::HtmlVoidTag {
                            content,
                            is_phrasing,
                        },
                    }
                } else {
                    Leaf::from_html(node)
                };
                leaves.insert(range.start, (leaf, range.end));
            } else if node.is_text() {
                let range = node.range().expect("text must have range");

                if let Some(embed_askm) = node.embed_askm() {
                    leaves.extend(Self::split_text(
                        range,
                        askama_nodes,
                        embed_askm,
                        source,
                        false,
                    ));
                } else {
                    let HtmlNode::Text { text, .. } = node else {
                        unreachable!()
                    };
                    leaves.insert(range.start, (Leaf::from_text(text, false), range.end));
                }
            } else if node.is_raw_text() {
                let range = node.range().expect("raw text must have range");

                if let Some(embed_askm) = node.embed_askm() {
                    leaves.extend(Self::split_text(
                        range,
                        askama_nodes,
                        embed_askm,
                        source,
                        true,
                    ));
                } else {
                    let HtmlNode::RawText { text, .. } = node else {
                        unreachable!()
                    };
                    leaves.insert(range.start, (Leaf::from_text(text, true), range.end));
                }
            } else if node.is_comment() {
                let range = node.range().expect("comment must have range");

                let leaf = if let Some(embed_askm) = node.embed_askm() {
                    pruned.extend(embed_askm.iter().copied());
                    let content =
                        html::reconstruct_comment(range, source, askama_nodes, embed_askm, config);
                    Leaf::HtmlComment(content)
                } else {
                    Leaf::from_html(node)
                };
                leaves.insert(range.start, (leaf, range.end));
            } else if let Some(range) = node.range() {
                // Other node types
                leaves.insert(range.start, (Leaf::from_html(node), range.end));
            } else {
                // Nodes without ranges
                leaves.insert(node.start(), (Leaf::from_html(node), node.start()));
            }
        }

        (leaves, pruned)
    }

    fn split_text(
        range: &Range<usize>,
        askama_nodes: &[AskamaNode],
        embed_askm: &[usize],
        source: &str,
        is_raw: bool,
    ) -> Vec<(usize, (Leaf, usize))> {
        let leaves = {
            let mut segments = Vec::new();
            let mut current_pos = range.start;

            for &idx in embed_askm {
                let askama = &askama_nodes[idx];
                // Insert text before this Askama node (if any)
                if askama.start() > current_pos {
                    segments.push((current_pos, askama.start()));
                }
                current_pos = askama.end();
            }

            // Insert remaining text after last Askama node (if any)
            if current_pos < range.end {
                segments.push((current_pos, range.end));
            }

            segments
        };

        leaves
            .into_iter()
            .filter_map(|(start, end)| {
                let text = &source[start..end];
                // Avoid creating empty text leaves
                (!text.is_empty()).then_some((start, (Leaf::from_text(text, is_raw), end)))
            })
            .collect()
    }

    fn generate_spacing_map(nodes: &[(usize, (Leaf, usize))], source: &str) -> Vec<(bool, bool)> {
        nodes
            .iter()
            .enumerate()
            .map(|(i, (start, (leaf, end)))| {
                Self::should_preserve_whitespace(i, leaf, *start, *end, nodes, source)
            })
            .collect()
    }

    fn should_preserve_whitespace(
        i: usize,
        leaf: &Leaf,
        start: usize,
        end: usize,
        nodes: &[(usize, (Leaf, usize))],
        source: &str,
    ) -> (bool, bool) {
        if !leaf.preserves_whitespace() {
            return (false, false);
        }

        // Check if the node itself has internal leading/trailing whitespace
        let node_src = &source[start..end];
        let internal_start = node_src.starts_with(char::is_whitespace);
        let internal_end = node_src.ends_with(char::is_whitespace);

        // Get adjacent nodes for context
        let prev = i.checked_sub(1).and_then(|p| nodes.get(p));
        let next = nodes.get(i + 1);

        // Check if there is whitespace between nodes
        let before_node = prev.is_some_and(|(_, (_, prev_end))| {
            source[*prev_end..start].contains(char::is_whitespace)
        });

        let after_node = next
            .is_some_and(|(next_start, _)| source[end..*next_start].contains(char::is_whitespace));

        // Whitespace exists if it's internal to the node OR between nodes
        let ws_before = internal_start || before_node;
        let ws_after = internal_end || after_node;

        if leaf.is_ctrl() {
            return (ws_before, ws_after);
        }

        let prev_leaf = prev.map(|(_, (l, _))| l);
        let next_leaf = next.map(|(_, (l, _))| l);

        (
            ws_before && prev_leaf.is_some_and(Leaf::preserves_whitespace),
            ws_after && next_leaf.is_some_and(Leaf::preserves_whitespace),
        )
    }

    fn pair_askama_controls(
        &mut self,
        askama_nodes: &[AskamaNode],
        byte_map: &HashMap<usize, usize>,
    ) {
        for node in askama_nodes {
            if let AskamaNode::Control {
                ctrl_tag,
                close_tag: Some(closing_byte),
                range,
                ..
            } = node
                && ctrl_tag.is_opening()
                && let Some(&opening_idx) = byte_map.get(&range.start)
                && let Some(&closing_idx) = byte_map.get(closing_byte)
                && let Leaf::AskamaControl { end_idx, .. } = &mut self.leaves[opening_idx]
            {
                *end_idx = Some(closing_idx);
            }
        }
    }

    fn pair_html_elements(
        &mut self,
        html_nodes: &[HtmlNode],
        askama_nodes: &[AskamaNode],
        byte_map: &HashMap<usize, usize>,
    ) {
        for html_node in html_nodes {
            if let (Some(range), Some(end_tag_idx)) = (html_node.range(), html_node.end_tag_idx())
                && let Some(end_node) = html_nodes.get(end_tag_idx)
                && let (Some(&start_leaf_idx), Some(&end_leaf_idx)) =
                    (byte_map.get(&range.start), byte_map.get(&end_node.start()))
                && askama::is_inside_same_ctrl(range.start, end_node.start(), askama_nodes)
                && let Leaf::HtmlStartTag { end_idx, .. } = &mut self.leaves[start_leaf_idx]
            {
                *end_idx = Some(end_leaf_idx);
            }
        }
    }

    fn generate_indent_map(&self) -> Vec<i32> {
        let mut indent_map = vec![0; self.leaves.len()];
        let mut curr_indent = 0;

        for (i, leaf) in self.leaves.iter().enumerate() {
            if leaf.is_end_tag() {
                curr_indent = (curr_indent - 1).max(0);
            }

            let (pre_delta, post_delta) = match leaf {
                Leaf::AskamaControl { tag, .. } => tag.indent_delta(),
                _ => (0, 0),
            };

            curr_indent = (curr_indent + pre_delta).max(0);
            indent_map[i] = curr_indent;
            curr_indent = (curr_indent + post_delta).max(0);

            // Only increment indent if it has a matching end tag
            if leaf.is_start_tag() && leaf.pair().is_some() {
                curr_indent += 1;
            }
        }
        indent_map
    }

    // Grow concentric rings
    fn grow_rings(&self, start_idx: usize, end_idx: usize) -> Vec<Ring> {
        let mut rings = Vec::new();
        let mut start = start_idx;

        while start < end_idx {
            let leaf = &self.leaves[start];

            let (ring, next_idx) = match leaf {
                Leaf::AskamaControl { tag, .. } if tag.is_match_arm() => {
                    let mut curr_idx = start + 1;

                    // Scan forward to find where this match arm ends
                    while curr_idx < end_idx {
                        let Some(leaf) = self.leaves.get(curr_idx) else {
                            break;
                        };
                        if leaf.is_ctrl() {
                            break;
                        }
                        if let Some(end) = leaf.pair() {
                            curr_idx = end + 1;
                            continue;
                        }
                        curr_idx += 1;
                    }

                    let fits = self.fits(start, curr_idx - 1);
                    let end = if fits { curr_idx - 1 } else { start };
                    (Ring::MatchArm { start, end }, end + 1)
                }
                Leaf::AskamaControl { tag, .. } if tag.is_opening() => {
                    if let Some(end) = leaf.pair() {
                        let inner = self.grow_rings(start + 1, end);
                        (Ring::Paired { start, end, inner }, end + 1)
                    } else {
                        (Ring::Single(start), start + 1)
                    }
                }
                leaf if leaf.is_start_tag() => {
                    if let Some(end) = leaf.pair() {
                        let is_phrasing_sequence = leaf.is_phrasing()
                            && self.leaves.get(end + 1).is_some_and(Leaf::is_text_sequence);

                        if is_phrasing_sequence
                            && let Some(result) = self.try_text_sequence(start, end_idx)
                        {
                            result
                        } else {
                            let inner = self.grow_rings(start + 1, end);
                            (Ring::Paired { start, end, inner }, end + 1)
                        }
                    } else {
                        (Ring::Single(start), start + 1)
                    }
                }
                Leaf::HtmlRawText(_) => {
                    let count = self.leaves[start..end_idx]
                        .iter()
                        .take_while(|l| matches!(l, Leaf::HtmlRawText(_) | Leaf::AskamaExpr(_)))
                        .count();
                    let end = start + count - 1;
                    (Ring::RawText { start, end }, end + 1)
                }
                leaf if leaf.is_text_sequence() => self
                    .try_text_sequence(start, end_idx)
                    .unwrap_or_else(|| (Ring::Single(start), start + 1)),
                _ => (Ring::Single(start), start + 1),
            };

            rings.push(ring);
            start = next_idx;
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

        (last_idx >= start_idx).then(|| {
            (
                Ring::TextSequence {
                    start: start_idx,
                    end: last_idx,
                    inner: inner_rings,
                },
                last_idx + 1,
            )
        })
    }

    fn grow_branch(&mut self, ring: &Ring) {
        let (start, end) = ring.range();

        let indentation = self.indent(start) as usize * self.config.indent_size;
        let fits = indentation + self.width(start, end) <= self.config.max_width;

        match ring {
            Ring::Paired { start, end, inner } => {
                let has_block = self.has_block_inner_rings(inner);

                if fits && !has_block {
                    self.push_branch(*start, *end, BranchStyle::Inline);
                } else {
                    self.push_branch_with_single_leaf(*start);
                    for ring in inner {
                        self.grow_branch(ring);
                    }
                    self.push_branch_with_single_leaf(*end);
                }
            }
            Ring::TextSequence { start, end, inner } => {
                if fits {
                    self.push_branch(*start, *end, BranchStyle::Inline);
                } else if self.is_all_text_sequence(*start, *end) {
                    self.push_branch(*start, *end, BranchStyle::WrappedText);
                } else {
                    self.split_text_sequence(*start, inner);
                }
            }
            Ring::RawText { start, end } => {
                self.push_branch(*start, *end, BranchStyle::Raw);
            }
            Ring::MatchArm { start, end } => {
                self.push_branch(*start, *end, BranchStyle::Inline);
            }
            Ring::Single(idx) => match &self.leaves[*idx] {
                Leaf::HtmlComment(_) | Leaf::AskamaComment(_) => {
                    self.push_branch(*idx, *idx, BranchStyle::Comment)
                }
                _ => self.push_branch_with_single_leaf(*idx),
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

    fn split_text_sequence(&mut self, start: usize, inner: &[Ring]) {
        let indent = self.indent(start);
        let indent_width = self.indent_width(indent);
        let available_width = self.config.max_width.saturating_sub(indent_width);

        let mut line_start = start;
        let mut line_end = start;

        for ring in inner {
            let (start, end) = ring.range();

            // Calculate width of current line if we add this ring
            let try_width = self.width_with_spaces(line_start, end);

            if try_width > available_width && line_start < start {
                // Emit current line (before adding this ring)
                let line_width = self.width_with_spaces(line_start, line_end);
                let style = self.branch_style(line_start, line_end, line_width, available_width);
                self.branches
                    .push(Branch::grow(line_start, line_end, style, indent));

                // Start new line with this ring
                line_start = start;
                line_end = end;
            } else {
                // Add to current line
                line_end = end;
            }
        }

        // Emit final line
        if line_start <= line_end {
            let line_width = self.width_with_spaces(line_start, line_end);
            let style = self.branch_style(line_start, line_end, line_width, available_width);
            self.branches
                .push(Branch::grow(line_start, line_end, style, indent));
        }
    }

    fn branch_style(
        &self,
        start: usize,
        end: usize,
        line_width: usize,
        available_width: usize,
    ) -> BranchStyle {
        if line_width > available_width && self.is_all_text_sequence(start, end) {
            return BranchStyle::WrappedText;
        }

        BranchStyle::Inline
    }

    fn fits(&self, start: usize, end: usize) -> bool {
        self.width(start, end) <= self.config.max_width
    }

    fn is_all_text_sequence(&self, start: usize, end: usize) -> bool {
        (start..=end).all(|i| self.leaves.get(i).is_some_and(Leaf::is_text_sequence))
    }

    fn width(&self, start: usize, end: usize) -> usize {
        (start..=end)
            .filter_map(|i| self.leaves.get(i))
            .map(Leaf::chars_count)
            .sum()
    }

    fn width_with_spaces(&self, start: usize, end: usize) -> usize {
        let mut width = 0;
        let mut prev_idx = None;

        for leaf_idx in start..=end {
            if let Some(leaf) = self.leaves.get(leaf_idx) {
                if let Some(prev) = prev_idx
                    && self.has_space(prev, leaf_idx)
                {
                    width += 1;
                }
                width += leaf.chars_count();
                prev_idx = Some(leaf_idx);
            }
        }

        width
    }

    pub fn has_space(&self, prev_idx: usize, curr_idx: usize) -> bool {
        let after_prev = self
            .spacing_map
            .get(prev_idx)
            .is_some_and(|(_, after)| *after);
        let before_curr = self
            .spacing_map
            .get(curr_idx)
            .is_some_and(|(before, _)| *before);
        after_prev || before_curr
    }

    fn indent(&self, idx: usize) -> i32 {
        self.indent_map.get(idx).copied().unwrap_or(0)
    }

    fn indent_width(&self, indent: i32) -> usize {
        (indent as usize) * self.config.indent_size
    }

    fn push_branch(&mut self, start: usize, end: usize, style: BranchStyle) {
        let indent = self.indent(start);
        self.branches.push(Branch::grow(start, end, style, indent));
    }

    fn push_branch_with_single_leaf(&mut self, idx: usize) {
        let indent = self.indent(idx);
        self.branches
            .push(Branch::grow(idx, idx, BranchStyle::Inline, indent));
    }
}
