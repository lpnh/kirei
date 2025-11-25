use std::collections::{BTreeMap, HashMap, HashSet};
use std::ops::{Range, RangeInclusive as Twig};

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
    pub twig: Twig<usize>,
    pub style: BranchStyle,
    pub indent: i32,
}

#[derive(Debug, Clone)]
pub enum BranchStyle {
    Inline,
    OpenClose,
    WrappedText,
    Comment,
    Raw,
}

#[derive(Debug, Clone)]
struct Ring {
    twig: Twig<usize>,
    layer: RingLayer,
    inner_rings: Option<Vec<Ring>>,
}

impl Ring {
    fn from_single_with_next(idx: usize) -> (Self, usize) {
        (Self::from_single(idx), idx + 1)
    }

    fn from_single(idx: usize) -> Self {
        Self {
            twig: idx..=idx,
            layer: RingLayer::Single,
            inner_rings: None,
        }
    }

    fn from_paired(
        start_leaf: usize,
        leaf: &Leaf,
        layer: RingLayer,
        inner_rings: Vec<Ring>,
    ) -> (Self, usize) {
        let twig = leaf.pair_range(start_leaf);
        let end_leaf = *twig.end();

        let ring = Self {
            twig,
            layer,
            inner_rings: Some(inner_rings).filter(|r| !r.is_empty()),
        };

        (ring, end_leaf + 1)
    }
}

#[derive(Debug, Clone)]
enum RingLayer {
    Element,
    ControlBlock,
    MatchArm,
    TextSequence,
    RawText,
    Single,
}

impl Leaf {
    fn is_paired(&self) -> bool {
        matches!(
            self,
            Self::AskamaControl {
                end_idx: Some(_),
                ..
            } | Self::HtmlStartTag {
                end_idx: Some(_),
                ..
            }
        )
    }

    fn pair_range(&self, start_idx: usize) -> Twig<usize> {
        match self {
            Self::AskamaControl {
                end_idx: Some(end), ..
            }
            | Self::HtmlStartTag {
                end_idx: Some(end), ..
            } => start_idx..=*end,
            _ => start_idx..=start_idx,
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
    pub fn grow(twig: Twig<usize>, style: BranchStyle, indent: i32) -> Self {
        Self {
            twig,
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

        // Check if there is whitespace between nodes
        let before_node = i > 0 && {
            let (_, (_, prev_end)) = nodes[i - 1];
            source[prev_end..start].contains(char::is_whitespace)
        };

        let after_node = i + 1 < nodes.len() && {
            let (next_start, _) = nodes[i + 1];
            source[end..next_start].contains(char::is_whitespace)
        };

        // Get adjacent nodes for context
        let prev = i
            .checked_sub(1)
            .and_then(|p| nodes.get(p))
            .map(|(_, (l, _))| l);
        let next = nodes.get(i + 1).map(|(_, (l, _))| l);

        // Whitespace exists if it's internal to the node OR between nodes
        let ws_before = internal_start || before_node;
        let ws_after = internal_end || after_node;

        if leaf.is_ctrl() {
            return (ws_before, ws_after);
        }

        (
            ws_before && prev.is_some_and(Leaf::preserves_whitespace),
            ws_after && next.is_some_and(Leaf::preserves_whitespace),
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
            if leaf.is_start_tag() && leaf.pair_range(i).count() > 1 {
                curr_indent += 1;
            }
        }
        indent_map
    }

    // Grow concentric rings
    fn grow_rings(&self, start_idx: usize, end_idx: usize) -> Vec<Ring> {
        let mut rings = Vec::new();
        let mut idx = start_idx;

        while idx < end_idx {
            let leaf = &self.leaves[idx];

            let (ring, next_idx) = match leaf {
                Leaf::AskamaControl { tag, .. } if tag.is_match_arm() => {
                    self.try_match_arm_with_content(idx, end_idx)
                }
                leaf if leaf.is_ctrl() => self
                    .try_askama_block(idx)
                    .unwrap_or_else(|| Ring::from_single_with_next(idx)),
                leaf if leaf.is_start_tag() => self.handle_start_tag(idx, end_idx),
                Leaf::HtmlRawText(_) => self.raw_text(idx, end_idx),
                leaf if leaf.is_text_sequence() => self
                    .try_text_sequence(idx, end_idx)
                    .unwrap_or_else(|| Ring::from_single_with_next(idx)),
                _ => Ring::from_single_with_next(idx),
            };

            rings.push(ring);
            idx = next_idx;
        }

        rings
    }

    fn grow_inner_rings(&self, start_idx: usize, end_idx: usize) -> Vec<Ring> {
        self.grow_rings(start_idx + 1, end_idx)
    }

    fn handle_start_tag(&self, idx: usize, end_idx: usize) -> (Ring, usize) {
        let leaf = &self.leaves[idx];
        let pair = leaf.pair_range(idx);

        // Try text sequence for phrasing elements followed by text/expr
        if leaf.is_phrasing()
            && leaf.is_paired()
            && self
                .leaves
                .get(*pair.end() + 1)
                .is_some_and(Leaf::is_text_sequence)
            && let Some(result) = self.try_text_sequence(idx, end_idx)
        {
            return result;
        }

        // Try complete element, or fall back to single leaf
        self.try_complete_element(idx)
            .unwrap_or_else(|| Ring::from_single_with_next(idx))
    }

    fn try_complete_element(&self, start: usize) -> Option<(Ring, usize)> {
        self.leaves
            .get(start)
            .filter(|leaf| leaf.is_paired())
            .map(|leaf| {
                let inner_rings = self.grow_inner_rings(start, *leaf.pair_range(start).end());
                Ring::from_paired(start, leaf, RingLayer::Element, inner_rings)
            })
    }

    fn try_askama_block(&self, start: usize) -> Option<(Ring, usize)> {
        let Leaf::AskamaControl { tag, .. } = &self.leaves[start] else {
            return None;
        };

        tag.is_opening()
            .then(|| &self.leaves[start])
            .filter(|leaf| leaf.is_paired())
            .map(|leaf| {
                let inner_rings = self.grow_inner_rings(start, *leaf.pair_range(start).end());
                Ring::from_paired(start, leaf, RingLayer::ControlBlock, inner_rings)
            })
    }

    fn try_match_arm_with_content(&self, start_idx: usize, end_idx: usize) -> (Ring, usize) {
        let mut curr_idx = start_idx + 1;

        // Find where the content ends
        while curr_idx < end_idx {
            let Some(leaf) = self.leaves.get(curr_idx) else {
                break;
            };

            // Stop at any control block
            if leaf.is_ctrl() {
                break;
            }

            // If start tag, skip to after the end tag
            if leaf.is_start_tag() && leaf.is_paired() {
                let pair = leaf.pair_range(curr_idx);
                curr_idx = *pair.end() + 1;
                continue;
            }

            curr_idx += 1;
        }

        // All or nothing
        let fits = self.twig_fits(start_idx..=(curr_idx - 1));

        let twig = if fits {
            start_idx..=(curr_idx - 1)
        } else {
            start_idx..=start_idx
        };

        let ring = Ring {
            twig,
            layer: RingLayer::MatchArm,
            inner_rings: None,
        };

        let next_idx = if fits { curr_idx } else { start_idx + 1 };

        (ring, next_idx)
    }

    fn raw_text(&self, idx: usize, end_idx: usize) -> (Ring, usize) {
        let last_idx = self.leaves[idx..]
            .iter()
            .take(end_idx - idx)
            .take_while(|leaf| matches!(leaf, Leaf::HtmlRawText(_) | Leaf::AskamaExpr(_)))
            .count()
            + idx
            - 1;

        let ring = Ring {
            twig: idx..=last_idx,
            layer: RingLayer::RawText,
            inner_rings: None,
        };

        (ring, last_idx + 1)
    }

    fn try_text_sequence(&self, start_idx: usize, end_idx: usize) -> Option<(Ring, usize)> {
        let mut last_idx = start_idx;
        let mut curr_idx = start_idx;
        let mut inner_rings = Vec::new();

        // Collect following phrasing content
        while curr_idx < end_idx {
            let leaf = &self.leaves[curr_idx];

            if leaf.is_start_tag() && leaf.is_paired() && leaf.is_phrasing() {
                let pair = leaf.pair_range(curr_idx);

                // Check if complete element fits inline
                if !self.twig_fits(pair.clone()) {
                    if curr_idx == start_idx {
                        return None;
                    }
                    break;
                }

                let elem_inner = self.grow_inner_rings(curr_idx, *pair.end());
                inner_rings.push(Ring {
                    twig: pair.clone(),
                    layer: RingLayer::Element,
                    inner_rings: Some(elem_inner).filter(|r| !r.is_empty()),
                });

                last_idx = *pair.end();
                curr_idx = *pair.end() + 1;
                continue;
            }

            if leaf.is_phrasing() {
                inner_rings.push(Ring::from_single(curr_idx));
                last_idx = curr_idx;
                curr_idx += 1;
            } else {
                if curr_idx == start_idx {
                    return None;
                }
                break;
            }
        }

        (last_idx >= start_idx).then(|| {
            let ring = Ring {
                twig: start_idx..=last_idx,
                layer: RingLayer::TextSequence,
                inner_rings: Some(inner_rings),
            };

            (ring, curr_idx)
        })
    }

    fn grow_branch(&mut self, ring: &Ring) {
        let indentation = self.indent(*ring.twig.start()) as usize * self.config.indent_size;
        let fits = indentation + self.twig_width(ring.twig.clone()) <= self.config.max_width;

        match &ring.layer {
            RingLayer::Element | RingLayer::ControlBlock => {
                // Check if inner content has block-level elements or control blocks
                let has_block = ((*ring.twig.start() + 1).min(*ring.twig.end())..*ring.twig.end())
                    .any(|i| {
                        self.leaves.get(i).is_some_and(|leaf| match leaf {
                            Leaf::HtmlStartTag { is_phrasing, .. } => !is_phrasing,
                            Leaf::AskamaControl { tag, .. } => tag.is_opening(),
                            Leaf::HtmlRawText(_) => true,
                            _ => false,
                        })
                    });

                if fits && !has_block {
                    self.push_branch(&ring.twig, BranchStyle::Inline);
                } else {
                    self.grow_open_close_branches(&ring.twig, ring.inner_rings.as_ref());
                }
            }
            RingLayer::TextSequence => {
                if fits {
                    self.push_branch(&ring.twig, BranchStyle::Inline);
                } else {
                    let is_text_and_entity_only = ring
                        .twig
                        .clone()
                        .all(|i| self.leaves.get(i).is_some_and(Leaf::is_text_sequence));

                    if is_text_and_entity_only {
                        self.push_branch(&ring.twig, BranchStyle::WrappedText);
                    } else if let Some(inner) = ring.inner_rings.as_ref() {
                        self.split_text_sequence(&ring.twig, inner);
                    }
                }
            }
            RingLayer::RawText => {
                self.push_branch(&ring.twig, BranchStyle::Raw);
            }
            RingLayer::MatchArm => {
                self.push_branch(&ring.twig, BranchStyle::Inline);
            }
            RingLayer::Single => {
                let leaf = &self.leaves[*ring.twig.start()];
                let style = match leaf {
                    Leaf::HtmlRawText(_) => BranchStyle::Raw,
                    Leaf::HtmlComment(_) | Leaf::AskamaComment(_) => BranchStyle::Comment,
                    _ => BranchStyle::Inline,
                };
                self.push_branch(&ring.twig, style);
            }
        }
    }

    fn grow_open_close_branches(&mut self, twig: &Twig<usize>, inner: Option<&Vec<Ring>>) {
        let indent = self.indent(*twig.start());

        self.branches.push(Branch::grow(
            *twig.start()..=*twig.start(),
            BranchStyle::OpenClose,
            indent,
        ));

        if let Some(inner_rings) = inner {
            for ring in inner_rings {
                self.grow_branch(ring);
            }
        }

        self.branches.push(Branch::grow(
            *twig.end()..=*twig.end(),
            BranchStyle::OpenClose,
            indent,
        ));
    }

    fn split_text_sequence(&mut self, twig: &Twig<usize>, inner: &[Ring]) {
        let indent = self.indent(*twig.start());
        let indent_width = self.indent_width(indent);
        let available_width = self.config.max_width.saturating_sub(indent_width);

        let mut line_start = *twig.start();
        let mut line_end = *twig.start();

        for ring in inner {
            // Calculate width of current line if we add this ring
            let try_twig = line_start..=*ring.twig.end();
            let try_width = self.twig_width_with_spaces(&try_twig);

            if try_width > available_width && line_start < *ring.twig.start() {
                // Emit current line (before adding this ring)
                let line_width = self.twig_width_with_spaces(&(line_start..=line_end));
                let style =
                    self.branch_style_from_line(line_start..=line_end, line_width, available_width);
                self.branches
                    .push(Branch::grow(line_start..=line_end, style, indent));

                // Start new line with this ring
                line_start = *ring.twig.start();
                line_end = *ring.twig.end();
            } else {
                // Add to current line
                line_end = *ring.twig.end();
            }
        }

        // Emit final line
        if line_start <= line_end {
            let line_width = self.twig_width_with_spaces(&(line_start..=line_end));
            let style =
                self.branch_style_from_line(line_start..=line_end, line_width, available_width);
            self.branches
                .push(Branch::grow(line_start..=line_end, style, indent));
        }
    }

    fn branch_style_from_line(
        &self,
        twig: Twig<usize>,
        line_width: usize,
        available_width: usize,
    ) -> BranchStyle {
        if line_width > available_width {
            let is_text_entity_only = twig
                .into_iter()
                .all(|i| self.leaves.get(i).is_some_and(Leaf::is_text_sequence));

            if is_text_entity_only {
                return BranchStyle::WrappedText;
            }
        }

        BranchStyle::Inline
    }

    fn twig_fits(&self, twig: Twig<usize>) -> bool {
        self.twig_width(twig) <= self.config.max_width
    }

    fn twig_width(&self, indices: impl IntoIterator<Item = usize>) -> usize {
        indices
            .into_iter()
            .filter_map(|i| self.leaves.get(i))
            .map(Leaf::chars_count)
            .sum()
    }

    fn twig_width_with_spaces(&self, twig: &Twig<usize>) -> usize {
        let mut width = 0;
        let mut prev_idx = None;

        for leaf_idx in twig.clone() {
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

    fn push_branch(&mut self, twig: &Twig<usize>, style: BranchStyle) {
        let indent = self.indent(*twig.start());
        self.branches
            .push(Branch::grow(twig.clone(), style, indent));
    }
}
