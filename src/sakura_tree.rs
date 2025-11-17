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
}

#[derive(Debug, Clone)]
pub enum Leaf {
    AskamaControl {
        content: String,
        tag: ControlTag,
        end_idx: Option<usize>,
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
        end_idx: Option<usize>,
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
    MultilineComment,
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

    fn from_askama(config: &Config, askama_node: &AskamaNode, source: &str) -> Self {
        let content = askama::format_askama_node(config, askama_node);

        match askama_node {
            AskamaNode::Control { ctrl_tag, .. } => Self::AskamaControl {
                content,
                tag: *ctrl_tag,
                end_idx: None,
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
            HtmlNode::StartTag { .. } => Self::HtmlStartTag {
                content: source,
                is_inline: html_node.is_inline(),
                end_idx: None,
            },
            HtmlNode::Void { .. } | HtmlNode::SelfClosingTag { .. } => Self::HtmlVoidTag {
                content: source,
                is_inline: html_node.is_inline(),
            },
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

    pub fn chars_count(&self) -> usize {
        self.content().chars().count()
    }

    pub fn is_ctrl(&self) -> bool {
        matches!(self, Self::AskamaControl { .. })
    }

    pub fn is_expr(&self) -> bool {
        matches!(self, Self::AskamaExpr { .. })
    }

    pub fn is_match_arm(&self) -> bool {
        matches!(self, Self::AskamaControl { tag, .. } if tag.is_match_arm())
    }

    pub fn has_space_before(&self) -> bool {
        matches!(
            self,
            Self::AskamaExpr {
                space_before: true,
                ..
            }
        )
    }

    pub fn has_space_after(&self) -> bool {
        matches!(
            self,
            Self::AskamaExpr {
                space_after: true,
                ..
            }
        )
    }

    pub fn is_text(&self) -> bool {
        matches!(self, Self::HtmlText(_))
    }

    pub fn is_entity(&self) -> bool {
        matches!(self, Self::HtmlEntity(_))
    }

    pub fn is_start_tag(&self) -> bool {
        matches!(self, Self::HtmlStartTag { .. })
    }

    pub fn is_end_tag(&self) -> bool {
        matches!(self, Self::HtmlEndTag(_))
    }

    pub fn is_text_or_entity(&self) -> bool {
        self.is_text() || self.is_entity()
    }

    fn is_text_or_expr(&self) -> bool {
        self.is_text() || self.is_expr()
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
        let mut tree = Self {
            config: config.clone(),
            leaves: Vec::new(),
            branches: Vec::new(),
        };

        // Grow Html leaves and pruned Askama indices inside tags or comments
        let (mut leaves, pruned) = Self::leaves_from_html(html_nodes, askama_nodes, source, config);

        // Grow standalone leaves for Askama nodes NOT in the pruned set
        leaves.extend(Self::leaves_from_askama(
            askama_nodes,
            &pruned,
            source,
            config,
        ));

        // Map byte positions to leaf indices
        let byte_to_leaf_map: HashMap<usize, usize> = leaves
            .iter()
            .enumerate()
            .map(|(leaf_idx, (start, _))| (*start, leaf_idx))
            .collect();

        tree.leaves = leaves.into_values().collect();

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
                if let Leaf::AskamaControl { end_idx, .. } = &mut tree.leaves[opening_idx] {
                    *end_idx = Some(closing_idx);
                }
                ctrl_boundaries.push((opening_idx, closing_idx));
            }
        }

        for html_node in html_nodes {
            if let (Some(range), Some(end_html_idx)) =
                (html_node.range(), html_node.end_tag_index())
                && let Some(end_node) = html_nodes.get(end_html_idx)
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

                    if let Leaf::HtmlStartTag { end_idx, .. } = &mut tree.leaves[start_leaf_idx] {
                        *end_idx = Some(end_leaf_idx);
                    }
                }
            }
        }

        let rings = tree.grow_rings(0, tree.leaves.len());
        let indent_map = tree.analyze_indentation_structure();

        for ring in rings {
            tree.grow_branch(&ring, &indent_map);
        }

        tree
    }

    fn leaves_from_askama(
        askama_nodes: &[AskamaNode],
        pruned: &HashSet<usize>,
        source: &str,
        config: &Config,
    ) -> BTreeMap<usize, Leaf> {
        let mut leaves = BTreeMap::new();

        for (idx, node) in askama_nodes.iter().enumerate() {
            if !pruned.contains(&idx) {
                leaves.insert(node.start(), Leaf::from_askama(config, node, source));
            }
        }

        leaves
    }

    fn leaves_from_html(
        html_nodes: &[HtmlNode],
        askama_nodes: &[AskamaNode],
        source: &str,
        config: &Config,
    ) -> (BTreeMap<usize, Leaf>, HashSet<usize>) {
        let mut leaves = BTreeMap::new();
        let mut pruned = HashSet::new();

        for node in html_nodes {
            if node.is_start_tag_or_void() {
                let range = node.range().expect("tag must have range");

                let leaf = if let Some(embed_askm) = node.embed_askm() {
                    pruned.extend(embed_askm.iter().copied());

                    let content =
                        html::reconstruct_tag(range, source, askama_nodes, embed_askm, config);
                    let is_inline = node.is_inline();
                    match node {
                        HtmlNode::StartTag { .. } => Leaf::HtmlStartTag {
                            content,
                            is_inline,
                            end_idx: None,
                        },
                        _ => Leaf::HtmlVoidTag { content, is_inline },
                    }
                } else {
                    Leaf::from_html(node)
                };
                leaves.insert(range.start, leaf);
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
                    leaves.insert(range.start, Leaf::from_text(text, false));
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
                    leaves.insert(range.start, Leaf::from_text(text, true));
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
                leaves.insert(range.start, leaf);
            } else if let Some(range) = node.range() {
                // Other node types
                leaves.insert(range.start, Leaf::from_html(node));
            } else {
                // Nodes without ranges
                leaves.insert(node.start(), Leaf::from_html(node));
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
    ) -> Vec<(usize, Leaf)> {
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
                (!text.is_empty()).then_some((start, Leaf::from_text(text, is_raw)))
            })
            .collect()
    }

    // Grow concentric rings
    fn grow_rings(&self, start_idx: usize, end_idx: usize) -> Vec<Ring> {
        let mut rings = Vec::new();
        let mut idx = start_idx;

        while idx < end_idx {
            let leaf = &self.leaves[idx];

            let (ring, next_idx) = match leaf {
                leaf if leaf.is_match_arm() => self.try_match_arm_with_content(idx, end_idx),
                leaf if leaf.is_ctrl() => self
                    .try_askama_block(idx)
                    .unwrap_or_else(|| Ring::from_single_with_next(idx)),
                leaf if leaf.is_start_tag() => self.handle_start_tag(idx, end_idx),
                Leaf::HtmlRawText(_) => self.raw_text(idx, end_idx),
                leaf if leaf.is_text_or_expr() => self
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

        // Try text sequence for inline elements followed by text/expr
        if leaf.is_inline_level()
            && leaf.is_paired()
            && self
                .leaves
                .get(*pair.end() + 1)
                .is_some_and(Leaf::is_text_or_expr)
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

        let content_end_idx = curr_idx;

        // All or nothing
        let fits = self.twig_fits(start_idx..content_end_idx);

        let twig = if fits && content_end_idx > start_idx + 1 {
            start_idx..=(content_end_idx - 1)
        } else {
            start_idx..=start_idx
        };

        let ring = Ring {
            twig,
            layer: RingLayer::MatchArm,
            inner_rings: None,
        };

        let next_idx = if fits { content_end_idx } else { start_idx + 1 };

        (ring, next_idx)
    }

    fn raw_text(&self, idx: usize, end_idx: usize) -> (Ring, usize) {
        let last_idx = self.leaves[idx..]
            .iter()
            .take(end_idx - idx)
            .take_while(|leaf| {
                matches!(leaf, Leaf::HtmlRawText(_) | Leaf::AskamaComment(_)) || leaf.is_expr()
            })
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

        // Collect following inline content
        while curr_idx < end_idx {
            let leaf = &self.leaves[curr_idx];

            if leaf.is_start_tag() && leaf.is_paired() && leaf.is_inline_level() {
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

            let can_include = leaf.is_expr() || leaf.is_inline_level();

            if can_include {
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

    fn analyze_indentation_structure(&self) -> Vec<i32> {
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

    fn grow_branch(&mut self, ring: &Ring, indent_map: &[i32]) {
        let fits = self.twig_fits(ring.twig.clone());

        match &ring.layer {
            RingLayer::Element | RingLayer::ControlBlock => {
                // Check if inner content has block-level elements or control blocks
                let has_block = ((*ring.twig.start() + 1).min(*ring.twig.end())..*ring.twig.end())
                    .any(|i| {
                        self.leaves.get(i).is_some_and(|leaf| match leaf {
                            Leaf::HtmlStartTag { is_inline, .. } => !is_inline,
                            Leaf::AskamaControl { tag, .. } => tag.is_opening(),
                            Leaf::HtmlRawText(_) => true,
                            _ => false,
                        })
                    });

                if fits && !has_block {
                    self.push_branch(&ring.twig, BranchStyle::Inline, indent_map);
                } else {
                    self.grow_open_close_branches(
                        &ring.twig,
                        ring.inner_rings.as_ref(),
                        indent_map,
                    );
                }
            }
            RingLayer::TextSequence => {
                if fits {
                    self.push_branch(&ring.twig, BranchStyle::Inline, indent_map);
                } else {
                    let is_text_and_entity_only = ring
                        .twig
                        .clone()
                        .all(|i| self.leaves.get(i).is_some_and(Leaf::is_text_or_entity));

                    if is_text_and_entity_only {
                        self.push_branch(&ring.twig, BranchStyle::WrappedText, indent_map);
                    } else if let Some(inner) = ring.inner_rings.as_ref() {
                        self.split_text_sequence(&ring.twig, inner, indent_map);
                    }
                }
            }
            RingLayer::RawText => {
                self.push_branch(&ring.twig, BranchStyle::Raw, indent_map);
            }
            RingLayer::MatchArm => {
                self.push_branch(&ring.twig, BranchStyle::Inline, indent_map);
            }
            RingLayer::Single => {
                let leaf = &self.leaves[*ring.twig.start()];
                let style = match leaf {
                    Leaf::HtmlRawText(_) => BranchStyle::Raw,
                    Leaf::HtmlComment(_) | Leaf::AskamaComment(_) => {
                        if fits {
                            BranchStyle::Inline
                        } else {
                            BranchStyle::MultilineComment
                        }
                    }
                    _ => BranchStyle::Inline,
                };
                self.push_branch(&ring.twig, style, indent_map);
            }
        }
    }

    fn grow_open_close_branches(
        &mut self,
        twig: &Twig<usize>,
        inner: Option<&Vec<Ring>>,
        indent_map: &[i32],
    ) {
        let indent = indent_map.get(*twig.start()).copied().unwrap_or(0);

        self.branches.push(Branch::grow(
            *twig.start()..=*twig.start(),
            BranchStyle::OpenClose,
            indent,
        ));

        if let Some(inner_rings) = inner {
            for ring in inner_rings {
                self.grow_branch(ring, indent_map);
            }
        }

        self.branches.push(Branch::grow(
            *twig.end()..=*twig.end(),
            BranchStyle::OpenClose,
            indent,
        ));
    }

    fn split_text_sequence(&mut self, twig: &Twig<usize>, inner: &[Ring], indent_map: &[i32]) {
        let indent = indent_map.get(*twig.start()).copied().unwrap_or(0);
        let indent_width = (indent as usize) * self.config.indent_size;
        let available_width = self.config.max_width.saturating_sub(indent_width);

        let mut line_start = *twig.start();
        let mut line_end = *twig.start();
        let mut line_width = 0;

        for ring in inner {
            let ring_width = self.twig_width(ring.twig.clone());
            let total_width = line_width + ring_width;

            if total_width > available_width && line_width > 0 {
                // Emit current line
                self.branches.push(Branch::grow(
                    line_start..=line_end,
                    BranchStyle::Inline,
                    indent,
                ));

                // Start new line
                line_start = *ring.twig.start();
                line_end = *ring.twig.end();
                line_width = ring_width;
            } else {
                // Add to current line
                line_end = *ring.twig.end();
                line_width = total_width;
            }
        }

        // Emit final line
        if line_width > 0 {
            self.branches.push(Branch::grow(
                line_start..=line_end,
                BranchStyle::Inline,
                indent,
            ));
        }
    }

    fn twig_fits(&self, indices: impl IntoIterator<Item = usize>) -> bool {
        self.twig_width(indices) <= self.config.max_width
    }

    fn twig_width(&self, indices: impl IntoIterator<Item = usize>) -> usize {
        indices
            .into_iter()
            .filter_map(|i| self.leaves.get(i))
            .map(Leaf::chars_count)
            .sum()
    }

    fn push_branch(&mut self, twig: &Twig<usize>, style: BranchStyle, indent_map: &[i32]) {
        let indent = indent_map.get(*twig.start()).copied().unwrap_or(0);
        self.branches
            .push(Branch::grow(twig.clone(), style, indent));
    }
}
