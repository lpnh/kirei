use std::collections::{BTreeMap, HashMap};
use std::ops::{Range, RangeInclusive};

use crate::{
    askama::{self, AskamaNode, ControlTag},
    config::Config,
    html::{self, HtmlNode},
};

#[derive(Debug, Clone)]
pub struct SakuraTree {
    pub config: Config,
    pub leaves: Vec<Leaf>,
    pub rings: Vec<Ring>,
    pub branches: Vec<Branch>,
    twigs: Vec<Twig>,
}

#[derive(Debug, Clone)]
pub enum Leaf {
    AskamaControl {
        content: String,
        tag: ControlTag,
    },
    AskamaExpr {
        content: String,
        space_before: bool,
        space_after: bool,
    },
    AskamaComment(String),

    HtmlStartTag {
        content: String,
        is_inline: bool,
    },
    HtmlVoidTag {
        content: String,
        is_inline: bool,
    },
    HtmlEndTag(String),

    HtmlText(String),
    HtmlEntity(String),
    HtmlRawText(String),
    HtmlComment(String),
    HtmlDoctype(String),
}

#[derive(Debug, Clone)]
pub enum Ring {
    // Compound
    Element(Twig, Option<Vec<Ring>>),
    ControlBlock(Twig, Option<Vec<Ring>>),
    MatchArm(Twig, Option<Vec<Ring>>),
    TextSequence(Twig, Vec<Ring>),

    // Atomic
    RawText(Twig),
    Comment(Twig),
    Other(Twig),
}

// Maps start_leaf index to end_leaf index
#[derive(Debug, Clone, Copy)]
pub struct Twig(usize, usize);

impl Twig {
    #[inline]
    pub fn start(self) -> usize {
        self.0
    }
    #[inline]
    pub fn end(self) -> usize {
        self.1
    }
    #[inline]
    pub fn has_same_idx(self) -> bool {
        self.0 == self.1
    }
    #[inline]
    pub fn indices(self) -> RangeInclusive<usize> {
        self.0..=self.1
    }
}

impl From<usize> for Twig {
    fn from(idx: usize) -> Self {
        Self(idx, idx)
    }
}

impl From<(usize, usize)> for Twig {
    fn from((start, end): (usize, usize)) -> Self {
        Self(start, end)
    }
}

#[derive(Debug, Clone)]
pub struct Branch {
    pub twig: Twig,
    pub style: BranchStyle,
    pub indent: i32,
}

#[derive(Debug, Clone)]
pub enum BranchStyle {
    Inline,
    OpenClose,
    WrappedText,
    MultilineComment,
    Raw,
}

// Maps relationships between Askama and Html node ranges
struct PruneMap {
    // Maps Html node index → Askama indices
    // These Askama nodes will be reconstructed as part of the Html node's content
    // `<div class="{{ foo }}">` → single HtmlStartTag leaf with embedded Askama
    askama_in_consumers: HashMap<usize, Vec<usize>>,

    // Maps Html text range → Askama indices
    // These Askama nodes will become separate leaves alongside text fragments
    // "Hello {{ name }}!" → splits into 3 leaves: `HtmlText`, `AskamaExpr`, `HtmlText`
    askama_in_splitters: HashMap<Range<usize>, Vec<usize>>,
}

impl PruneMap {
    fn new(askama_nodes: &[AskamaNode], html_nodes: &[HtmlNode]) -> Self {
        let askama_in_consumers = Self::find_consumers(askama_nodes, html_nodes);
        let askama_in_splitters = Self::find_splitters(askama_nodes, html_nodes);

        Self {
            askama_in_consumers,
            askama_in_splitters,
        }
    }

    // Check if this Askama node will be consumed by an Html node
    fn will_be_consumed(&self, askama_idx: usize) -> bool {
        self.askama_in_consumers
            .values()
            .any(|indices| indices.contains(&askama_idx))
    }

    // Check if this Html node consumes Askama (needs reconstruction)
    fn is_consumer(&self, html_idx: usize) -> bool {
        self.askama_in_consumers.contains_key(&html_idx)
    }

    // Check if this text range should be split (contains Askama)
    fn is_splitter(&self, range: &Range<usize>) -> bool {
        self.askama_in_splitters.contains_key(range)
    }

    // Find all consuming Html nodes (tags, comments) and the Askama they contain
    fn find_consumers(
        askama_nodes: &[AskamaNode],
        html_nodes: &[HtmlNode],
    ) -> HashMap<usize, Vec<usize>> {
        html_nodes
            .iter()
            .enumerate()
            .filter_map(|(html_idx, node)| {
                // Extract range from consuming nodes
                let (HtmlNode::StartTag { range, .. }
                | HtmlNode::Void { range, .. }
                | HtmlNode::SelfClosingTag { range, .. }
                | HtmlNode::Comment { range, .. }) = node
                else {
                    return None; // Not a consumer
                };

                // Find all Askama nodes within this Html node's range
                let askama_indices: Vec<usize> = askama_nodes
                    .iter()
                    .enumerate()
                    .filter(|(_, a)| a.start() >= range.start && a.end() <= range.end)
                    .map(|(idx, _)| idx)
                    .collect();

                (!askama_indices.is_empty()).then_some((html_idx, askama_indices))
            })
            .collect()
    }

    // Find all splitting text nodes and the Askama they contain
    fn find_splitters(
        askama_nodes: &[AskamaNode],
        html_nodes: &[HtmlNode],
    ) -> HashMap<Range<usize>, Vec<usize>> {
        html_nodes
            .iter()
            .filter_map(|node| match node {
                // Only Text and RawText nodes split their content
                HtmlNode::Text { range, .. } | HtmlNode::RawText { range, .. } => {
                    Some(range.clone())
                }
                _ => None, // Not a splitter
            })
            .filter_map(|range| {
                // Find all Askama nodes within this text range
                let indices: Vec<usize> = askama_nodes
                    .iter()
                    .enumerate()
                    .filter(|(_, a)| a.start() >= range.start && a.end() <= range.end)
                    .map(|(idx, _)| idx)
                    .collect();

                (!indices.is_empty()).then_some((range, indices))
            })
            .collect()
    }
}

impl Leaf {
    fn from_askama(config: &Config, askama_node: &AskamaNode, source: &str) -> Self {
        let content = askama::format_askama_node(config, askama_node);

        match askama_node {
            AskamaNode::Control { ctrl_tag, .. } => Self::AskamaControl {
                content,
                tag: *ctrl_tag,
            },
            AskamaNode::Expression { .. } => {
                let start = askama_node.start();
                let end = askama_node.end();

                let space_before = start > 0
                    && source[..start]
                        .chars()
                        .last()
                        .is_some_and(char::is_whitespace);

                let space_after = end < source.len()
                    && source[end..]
                        .chars()
                        .next()
                        .is_some_and(char::is_whitespace);

                Self::AskamaExpr {
                    content,
                    space_before,
                    space_after,
                }
            }
            AskamaNode::Comment { .. } => Self::AskamaComment(content),
        }
    }

    fn from_html(html_node: &HtmlNode) -> Self {
        let source = html_node.to_string();
        match html_node {
            HtmlNode::StartTag { name, .. } => Self::HtmlStartTag {
                content: source,
                is_inline: html::is_inline_tag_name(name),
            },
            HtmlNode::Void { name, .. } | HtmlNode::SelfClosingTag { name, .. } => {
                Self::HtmlVoidTag {
                    content: source,
                    is_inline: html::is_inline_tag_name(name),
                }
            }
            HtmlNode::EndTag { .. } | HtmlNode::ErroneousEndTag { .. } => Self::HtmlEndTag(source),
            HtmlNode::Text { .. } => Self::HtmlText(source),
            HtmlNode::Entity { .. } => Self::HtmlEntity(source),
            HtmlNode::RawText { .. } => Self::HtmlRawText(source),
            HtmlNode::Comment { .. } => Self::HtmlComment(source),
            HtmlNode::Doctype { .. } => Self::HtmlDoctype(source),
        }
    }

    fn is_inline_level(&self) -> bool {
        match self {
            Self::HtmlStartTag { is_inline, .. } | Self::HtmlVoidTag { is_inline, .. } => {
                *is_inline
            }
            Self::HtmlText(_) | Self::HtmlEntity(_) => true,
            _ => false,
        }
    }

    pub fn content(&self) -> &str {
        match self {
            Self::AskamaControl { content, .. }
            | Self::AskamaExpr { content, .. }
            | Self::AskamaComment(content)
            | Self::HtmlStartTag { content, .. }
            | Self::HtmlVoidTag { content, .. }
            | Self::HtmlEndTag(content)
            | Self::HtmlText(content)
            | Self::HtmlEntity(content)
            | Self::HtmlRawText(content)
            | Self::HtmlComment(content)
            | Self::HtmlDoctype(content) => content,
        }
    }

    pub fn is_ctrl(&self) -> bool {
        matches!(self, Self::AskamaControl { .. })
    }

    pub fn is_expr(&self) -> bool {
        matches!(self, Self::AskamaExpr { .. })
    }

    fn is_text_or_expr(&self) -> bool {
        matches!(self, Self::HtmlText(_) | Self::AskamaExpr { .. })
    }

    pub fn chars_count(&self) -> usize {
        self.content().chars().count()
    }

    fn from_text_fragment(fragment: &str, is_raw: bool) -> Self {
        if is_raw {
            // Normalize raw text by trimming newlines and normalizing line-by-line
            // Preserves blank lines (empty after trimming) to maintain user formatting
            Self::HtmlRawText(
                fragment
                    .trim_matches('\n')
                    .lines()
                    .map(str::trim)
                    .collect::<Vec<_>>()
                    .join("\n"),
            )
        } else {
            Self::HtmlText(crate::normalize_whitespace(fragment))
        }
    }
}

impl Branch {
    pub fn grow(twig: Twig, style: BranchStyle, indent: i32) -> Self {
        Self {
            twig,
            style,
            indent,
        }
    }
}

impl SakuraTree {
    pub fn twig_has_same_idx(&self, idx: usize) -> bool {
        self.twigs[idx].has_same_idx()
    }

    pub fn grow(
        askama_nodes: &[AskamaNode],
        html_nodes: &[HtmlNode],
        source: &str,
        config: &Config,
    ) -> Self {
        let mut tree = Self {
            config: config.clone(),
            leaves: Vec::new(),
            rings: Vec::new(),
            branches: Vec::new(),
            twigs: Vec::new(),
        };

        let prune_map = PruneMap::new(askama_nodes, html_nodes);

        let mut leaves: BTreeMap<usize, Leaf> = BTreeMap::new();
        Self::leaves_from_askama(&mut leaves, askama_nodes, &prune_map, source, config);
        Self::leaves_from_html(
            &mut leaves,
            html_nodes,
            askama_nodes,
            &prune_map,
            source,
            config,
        );

        // Map byte positions to leaf indices
        let byte_to_leaf_map: HashMap<usize, usize> = leaves
            .iter()
            .enumerate()
            .map(|(leaf_idx, (start, _))| (*start, leaf_idx))
            .collect();

        tree.leaves = leaves.into_values().collect();

        // Initialize twigs with same index for each leaf
        tree.twigs = (0..tree.leaves.len()).map(Twig::from).collect();

        // Control block pairings and boundaries
        let mut ctrl_boundaries = Vec::new();

        for node in askama_nodes {
            if let AskamaNode::Control {
                ctrl_tag,
                close_tag: Some(closing_byte),
                range,
                ..
            } = node
                && ctrl_tag.is_opening()
                && let Some(&opening_idx) = byte_to_leaf_map.get(&range.start)
                && let Some(&closing_idx) = byte_to_leaf_map.get(closing_byte)
            {
                tree.twigs[opening_idx] = Twig(opening_idx, closing_idx);
                ctrl_boundaries.push((opening_idx, closing_idx));
            }
        }

        // Update twigs for paired start/end tags
        for html_node in html_nodes {
            if let HtmlNode::StartTag {
                range,
                end_tag_idx: Some(end_html_idx),
                ..
            } = html_node
                && let Some(end_node) = html_nodes.get(*end_html_idx)
            {
                let end_start = end_node.start();

                if let (Some(&start_leaf_idx), Some(&end_leaf_idx)) = (
                    byte_to_leaf_map.get(&range.start),
                    byte_to_leaf_map.get(&end_start),
                ) {
                    // Skip if this Html element crosses a control block boundary
                    if ctrl_boundaries.iter().any(|&(ctrl_start, ctrl_end)| {
                        let start_inside = start_leaf_idx > ctrl_start && start_leaf_idx < ctrl_end;
                        let end_inside = end_leaf_idx > ctrl_start && end_leaf_idx < ctrl_end;
                        start_inside != end_inside
                    }) {
                        continue;
                    }

                    tree.twigs[start_leaf_idx] = Twig(start_leaf_idx, end_leaf_idx);
                }
            }
        }

        if let Some(rings) = tree.grow_rings(0, tree.leaves.len()) {
            tree.rings.extend(rings);
        }

        tree
    }

    fn leaves_from_askama(
        leaves: &mut BTreeMap<usize, Leaf>,
        askama_nodes: &[AskamaNode],
        prune_map: &PruneMap,
        source: &str,
        config: &Config,
    ) {
        for (idx, node) in askama_nodes.iter().enumerate() {
            // Only create standalone leaves for Askama that will not be consumed by Html nodes
            if !prune_map.will_be_consumed(idx) {
                leaves.insert(node.start(), Leaf::from_askama(config, node, source));
            }
        }
    }

    fn leaves_from_html(
        leaves: &mut BTreeMap<usize, Leaf>,
        html_nodes: &[HtmlNode],
        askama_nodes: &[AskamaNode],
        prune_map: &PruneMap,
        source: &str,
        config: &Config,
    ) {
        for (idx, node) in html_nodes.iter().enumerate() {
            match node {
                HtmlNode::StartTag { range, name, .. }
                | HtmlNode::Void { range, name, .. }
                | HtmlNode::SelfClosingTag { range, name, .. } => {
                    // Tags are consumers. Check if this one contains Askama
                    let leaf = if prune_map.is_consumer(idx) {
                        let content = html::reconstruct_tag(range, source, askama_nodes, config);
                        let is_inline = html::is_inline_tag_name(name);
                        match node {
                            HtmlNode::StartTag { .. } => Leaf::HtmlStartTag { content, is_inline },
                            _ => Leaf::HtmlVoidTag { content, is_inline },
                        }
                    } else {
                        Leaf::from_html(node)
                    };
                    leaves.insert(range.start, leaf);
                }
                HtmlNode::Text { range, text, .. } => {
                    // Text are splitters. Check if this one contains Askama
                    if prune_map.is_splitter(range) {
                        Self::split_text(leaves, range, askama_nodes, prune_map, source, false);
                    } else {
                        leaves.insert(range.start, Leaf::from_text_fragment(text, false));
                    }
                }
                HtmlNode::RawText { range, text, .. } => {
                    // RawText are splitters. Check if this one contains Askama
                    if prune_map.is_splitter(range) {
                        Self::split_text(leaves, range, askama_nodes, prune_map, source, true);
                    } else {
                        leaves.insert(range.start, Leaf::from_text_fragment(text, true));
                    }
                }
                HtmlNode::Comment { range, .. } => {
                    // Comments are consumers. Check if this one contains Askama
                    let leaf = if prune_map.is_consumer(idx) {
                        let content =
                            html::reconstruct_comment(range, source, askama_nodes, config);
                        Leaf::HtmlComment(content)
                    } else {
                        Leaf::from_html(node)
                    };
                    leaves.insert(range.start, leaf);
                }
                _ => {
                    // Let's pretend other node types don't contain Askama for now
                    leaves.insert(node.start(), Leaf::from_html(node));
                }
            }
        }
    }

    fn split_text(
        leaves: &mut BTreeMap<usize, Leaf>,
        range: &Range<usize>,
        askama_nodes: &[AskamaNode],
        prune_map: &PruneMap,
        source: &str,
        is_raw: bool,
    ) {
        let askama_indices = prune_map
            .askama_in_splitters
            .get(range)
            .expect("text range should contain Askama");

        let mut current_pos = range.start;
        for &idx in askama_indices {
            let askama = &askama_nodes[idx];
            // Insert text fragment before this Askama node (if any)
            if askama.start() > current_pos {
                let fragment = &source[current_pos..askama.start()];
                leaves.insert(current_pos, Leaf::from_text_fragment(fragment, is_raw));
            }
            current_pos = askama.end();
        }

        // Insert remaining text fragment after last Askama node (if any)
        if current_pos < range.end {
            let fragment = &source[current_pos..range.end];
            leaves.insert(current_pos, Leaf::from_text_fragment(fragment, is_raw));
        }
    }

    // Grow concentric rings
    fn grow_rings(&self, start_idx: usize, end_idx: usize) -> Option<Vec<Ring>> {
        if start_idx >= end_idx {
            return None;
        }

        let mut rings = Vec::new();
        let mut idx = start_idx;

        while idx < end_idx {
            let leaf = &self.leaves[idx];

            let (ring, next_idx) = match leaf {
                Leaf::AskamaControl { tag, .. } if tag.is_match_arm() => {
                    self.match_arm_with_content(idx, end_idx)
                }
                Leaf::AskamaControl { .. } => self
                    .try_askama_block(idx)
                    .unwrap_or_else(|| (self.with_single_leaf(idx), idx + 1)),
                Leaf::HtmlStartTag { .. } => self.handle_start_tag(idx, end_idx),
                Leaf::HtmlRawText(_) => {
                    // Collect consecutive RawText and Askama leaves into one ring
                    let last_idx = self.leaves[idx..]
                        .iter()
                        .take(end_idx - idx)
                        .take_while(|leaf| {
                            matches!(
                                leaf,
                                Leaf::HtmlRawText(_)
                                    | Leaf::AskamaExpr { .. }
                                    | Leaf::AskamaComment(_)
                            )
                        })
                        .count()
                        + idx
                        - 1;

                    (Ring::RawText((idx, last_idx).into()), last_idx + 1)
                }
                leaf if leaf.is_text_or_expr() => self
                    .try_text_sequence(idx, end_idx)
                    .unwrap_or_else(|| (self.with_single_leaf(idx), idx + 1)),
                _ => (self.with_single_leaf(idx), idx + 1),
            };

            rings.push(ring);
            idx = next_idx;
        }

        Some(rings)
    }

    fn with_single_leaf(&self, idx: usize) -> Ring {
        let leaf = &self.leaves[idx];
        let twig = idx.into();

        match leaf {
            Leaf::HtmlRawText(_) => Ring::RawText(twig),
            Leaf::HtmlComment(_) | Leaf::AskamaComment(_) => Ring::Comment(twig),
            _ => Ring::Other(twig),
        }
    }

    fn handle_start_tag(&self, idx: usize, end_idx: usize) -> (Ring, usize) {
        let leaf = &self.leaves[idx];

        // Try text sequence for inline elements followed by text/expr
        if leaf.is_inline_level()
            && !self.twigs[idx].has_same_idx()
            && self
                .leaves
                .get(self.twigs[idx].end() + 1)
                .is_some_and(Leaf::is_text_or_expr)
            && let Some(result) = self.try_text_sequence(idx, end_idx)
        {
            return result;
        }

        // Try complete element, or fall back to single leaf
        self.try_complete_element(idx)
            .unwrap_or_else(|| (self.with_single_leaf(idx), idx + 1))
    }

    fn try_complete_element(&self, start_leaf: usize) -> Option<(Ring, usize)> {
        let twig = self.twigs[start_leaf];
        (!twig.has_same_idx()).then(|| {
            let end_leaf = twig.end();
            let inner = self.grow_rings(start_leaf + 1, end_leaf);
            (Ring::Element(twig, inner), end_leaf + 1)
        })
    }

    fn try_askama_block(&self, start_idx: usize) -> Option<(Ring, usize)> {
        let Leaf::AskamaControl { tag, .. } = &self.leaves[start_idx] else {
            return None;
        };

        tag.is_opening()
            .then(|| self.twigs[start_idx])
            .filter(|twig| !twig.has_same_idx())
            .map(|twig| {
                let end_leaf = twig.end();
                let inner = self.grow_rings(start_idx + 1, end_leaf);
                (Ring::ControlBlock(twig, inner), end_leaf + 1)
            })
    }

    // Build a when block with its inline content as inner rings
    fn match_arm_with_content(&self, start_idx: usize, end_idx: usize) -> (Ring, usize) {
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
            if let Leaf::HtmlStartTag { .. } = leaf
                && !self.twigs[curr_idx].has_same_idx()
            {
                curr_idx = self.twigs[curr_idx].end() + 1;
                continue;
            }

            curr_idx += 1;
        }

        let content_end_idx = curr_idx;

        // All or nothing
        let twig = (start_idx, content_end_idx - 1).into();
        let temp_ring = Ring::TextSequence(twig, Vec::new());
        let fits = temp_ring.total_chars(self) <= self.config.max_width;

        if fits {
            let inner = self.grow_rings(start_idx + 1, content_end_idx);
            (Ring::MatchArm(start_idx.into(), inner), content_end_idx)
        } else {
            (Ring::MatchArm(start_idx.into(), None), start_idx + 1)
        }
    }

    fn try_text_sequence(&self, start_idx: usize, end_idx: usize) -> Option<(Ring, usize)> {
        let mut last_idx = start_idx;
        let mut curr_idx = start_idx + 1;
        let mut inner_rings = Vec::new();

        // Process first element
        if let Some(leaf) = self.leaves.get(start_idx) {
            if let Leaf::HtmlStartTag { .. } = leaf
                && !self.twigs[start_idx].has_same_idx()
            {
                // Check if complete element fits inline
                let elem_twig = self.twigs[start_idx];
                let elem_width: usize = elem_twig
                    .indices()
                    .filter_map(|i| self.leaves.get(i))
                    .map(Leaf::chars_count)
                    .sum();

                if elem_width > self.config.max_width {
                    return None;
                }

                let elem_inner = self.grow_rings(start_idx + 1, elem_twig.end());
                inner_rings.push(Ring::Element(elem_twig, elem_inner));
                last_idx = elem_twig.end();
                curr_idx = elem_twig.end() + 1;
            } else {
                inner_rings.push(self.with_single_leaf(start_idx));
            }
        }

        // Collect following inline content
        while curr_idx < end_idx {
            let leaf = &self.leaves[curr_idx];

            if let Leaf::HtmlStartTag { .. } = leaf
                && !self.twigs[curr_idx].has_same_idx()
                && leaf.is_inline_level()
            {
                // Check if complete element fits inline
                let elem_twig = self.twigs[curr_idx];
                let elem_width: usize = elem_twig
                    .indices()
                    .filter_map(|i| self.leaves.get(i))
                    .map(Leaf::chars_count)
                    .sum();

                if elem_width > self.config.max_width {
                    break;
                }

                let elem_inner = self.grow_rings(curr_idx + 1, elem_twig.end());
                inner_rings.push(Ring::Element(elem_twig, elem_inner));
                last_idx = elem_twig.end();
                curr_idx = elem_twig.end() + 1;
                continue;
            }

            let can_include = leaf.is_expr() || leaf.is_inline_level();

            if can_include {
                inner_rings.push(self.with_single_leaf(curr_idx));
                last_idx = curr_idx;
                curr_idx += 1;
            } else {
                break;
            }
        }

        (last_idx >= start_idx).then(|| {
            let ring = Ring::TextSequence((start_idx, last_idx).into(), inner_rings);
            (ring, curr_idx)
        })
    }
}

impl Ring {
    pub fn total_chars(&self, tree: &SakuraTree) -> usize {
        self.twig()
            .indices()
            .filter_map(|i| tree.leaves.get(i))
            .map(Leaf::chars_count)
            .sum()
    }

    // Check if any inner rings contain block-level structure
    pub fn has_block(&self, tree: &SakuraTree) -> bool {
        match self {
            Self::Element(_, Some(inner)) => {
                // Check if any inner ring is a block-level element
                inner.iter().any(|node| match node {
                    Self::Element(twig, _) => {
                        let start = twig.start();
                        !tree.leaves[start].is_inline_level()
                    }
                    Self::ControlBlock(_, _) | Self::RawText(_) => true,
                    _ => node.has_block(tree),
                })
            }
            Self::ControlBlock(_, Some(inner)) => inner.iter().any(|node| node.has_block(tree)),
            Self::MatchArm(_, Some(inner)) => inner.iter().any(|node| node.has_block(tree)),
            Self::TextSequence(_, inner) => inner.iter().any(|node| node.has_block(tree)),
            Self::RawText(_) => true,
            _ => false,
        }
    }

    pub fn twig(&self) -> Twig {
        match self {
            Self::Element(twig, _)
            | Self::TextSequence(twig, _)
            | Self::ControlBlock(twig, _)
            | Self::RawText(twig)
            | Self::Comment(twig)
            | Self::Other(twig)
            | Self::MatchArm(twig, None) => *twig,
            Self::MatchArm(twig, Some(inner)) => {
                // Get the last leaf from the last inner ring
                let first = twig.start();
                let last = inner.last().map_or(first, |ring| ring.twig().end());

                (first, last).into()
            }
        }
    }
}
