use std::collections::{BTreeMap, HashMap};
use std::ops::RangeInclusive;

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
    // Maps start_leaf index to end_leaf index
    twigs: Vec<Twig>,
}

#[derive(Debug, Clone)]
pub enum Leaf {
    AskamaControl { content: String, tag: ControlTag },
    AskamaExpr(String),
    AskamaComment(String),

    HtmlStartTag { content: String, is_inline: bool },
    HtmlVoidTag { content: String, is_inline: bool },
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
    InlineText(Twig),
    Other(Twig),
}

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
    SingleHtmlText,
    MultilineComment,
    Raw,
}

impl Leaf {
    fn from_askama(config: &Config, askama_node: &AskamaNode) -> Self {
        let content = askama::format_askama_node(config, askama_node);
        match askama_node {
            AskamaNode::Control { ctrl_tag, .. } => Self::AskamaControl {
                content,
                tag: *ctrl_tag,
            },
            AskamaNode::Expression { .. } => Self::AskamaExpr(content),
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
            HtmlNode::Text(_) => Self::HtmlText(source),
            HtmlNode::Entity(_) => Self::HtmlEntity(source),
            HtmlNode::RawText(_) => Self::HtmlRawText(source),
            HtmlNode::Comment(_) => Self::HtmlComment(source),
            HtmlNode::Doctype(_) => Self::HtmlDoctype(source),
        }
    }

    fn from_html_raw_text(text: &str) -> Self {
        let trimmed = text.trim_matches('\n');
        let normalized_content = if trimmed.is_empty() {
            String::new()
        } else {
            trimmed
                .lines()
                .map(str::trim)
                .collect::<Vec<_>>()
                .join("\n")
        };

        Self::HtmlRawText(normalized_content)
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
            | Self::AskamaExpr(content)
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
        matches!(self, Self::AskamaExpr(_))
    }

    pub fn chars_count(&self) -> usize {
        self.content().chars().count()
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
    pub fn grow(askama_nodes: &[AskamaNode], html_nodes: &[HtmlNode], config: Config) -> Self {
        let mut tree = Self {
            config,
            leaves: Vec::new(),
            rings: Vec::new(),
            branches: Vec::new(),
            twigs: Vec::new(),
        };

        let mut html_to_leaf_map: HashMap<usize, usize> = HashMap::new();

        // Convert each HtmlNode to Leaf while replacing placeholders
        for (i, html_node) in html_nodes.iter().enumerate() {
            html_to_leaf_map.insert(i, tree.leaves.len());

            // Dispatch based on node type
            match html_node {
                HtmlNode::Text(text) => {
                    let placeholders = Self::find_placeholders(text, askama_nodes);
                    if placeholders.is_empty() {
                        tree.leaves.push(Leaf::HtmlText(text.to_string()));
                    } else {
                        tree.replace_placeholders_in_text(text, placeholders, askama_nodes);
                    }
                }
                HtmlNode::Entity(_) | HtmlNode::Comment(_) | HtmlNode::Doctype(_) => {
                    tree.leaves.push(Leaf::from_html(html_node));
                }
                HtmlNode::RawText(text) => {
                    let processed = askama::replace_placeholder_in_raw_text(text, askama_nodes);
                    tree.leaves.push(Leaf::from_html_raw_text(&processed));
                }
                _ => {
                    let processed = html_node.clone().replace_placeholder(askama_nodes);
                    tree.leaves.push(Leaf::from_html(&processed));
                }
            }
        }

        // Initialize twigs with same index for each leaf
        tree.twigs = (0..tree.leaves.len()).map(Twig::from).collect();

        // Update twigs for start tags that have matching end tags
        for (html_idx, html_node) in html_nodes.iter().enumerate() {
            if let HtmlNode::StartTag {
                end_tag_idx: Some(end_html_idx),
                ..
            } = html_node
                && let (Some(&start_leaf_idx), Some(&end_leaf_idx)) = (
                    html_to_leaf_map.get(&html_idx),
                    html_to_leaf_map.get(end_html_idx),
                )
            {
                tree.twigs[start_leaf_idx] = Twig(start_leaf_idx, end_leaf_idx);
            }
        }

        if let Some(rings) = tree.grow_rings(0, tree.leaves.len()) {
            tree.rings.extend(rings);
        }

        tree
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
                    .try_askama_block(idx, end_idx)
                    .unwrap_or_else(|| (self.with_single_leaf(idx), idx + 1)),
                Leaf::HtmlStartTag { .. } => self.handle_start_tag(idx, end_idx),
                Leaf::HtmlRawText(_) => (Ring::RawText(idx.into()), idx + 1),
                Leaf::HtmlText(_) | Leaf::AskamaExpr(_) => self
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
            Leaf::HtmlText(_) | Leaf::HtmlEntity(_) => Ring::InlineText(twig),
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
                .is_some_and(|next| matches!(next, Leaf::HtmlText(_) | Leaf::AskamaExpr(_)))
            && let Some(result) = self.try_text_sequence(idx, end_idx)
        {
            return result;
        }

        // Try complete element, or fall back to single leaf
        self.try_complete_element(idx)
            .unwrap_or_else(|| (self.with_single_leaf(idx), idx + 1))
    }

    fn try_complete_element(&self, start_leaf: usize) -> Option<(Ring, usize)> {
        // Get the end_leaf for this element from twigs
        let twig = self.twigs[start_leaf];
        if twig.has_same_idx() {
            return None;
        }
        let end_leaf = twig.end();

        // Recursively grow inner rings
        let inner = self.grow_rings(start_leaf + 1, end_leaf);

        let ring = Ring::Element(twig, inner);

        // Next index for parent is after the end tag
        Some((ring, end_leaf + 1))
    }

    fn try_askama_block(&self, start_idx: usize, end_idx: usize) -> Option<(Ring, usize)> {
        let Leaf::AskamaControl { tag: ctrl_tag, .. } = &self.leaves[start_idx] else {
            return None;
        };

        if !ctrl_tag.is_opening() {
            return None;
        }

        let expected_close = ctrl_tag.matching_close()?;

        // Look for matching close block, tracking nesting depth
        let mut curr_idx = start_idx + 1;
        let mut depth: u32 = 0;

        while curr_idx < end_idx && depth < 200 {
            let leaf = self.leaves.get(curr_idx)?;

            // Check if this is another opening of the same type (nested block)
            if let Leaf::AskamaControl { tag, .. } = leaf
                && ctrl_tag.same_kind(*tag)
                && tag.is_opening()
            {
                depth += 1;
            } else if let Leaf::AskamaControl { tag, .. } = leaf
                && *tag == expected_close
            {
                if depth == 0 {
                    // Found our matching close at depth 0
                    let end_leaf = curr_idx;

                    // Recursively grow inner rings for leaves between open and close indices
                    let inner = self.grow_rings(start_idx + 1, end_leaf);

                    let twig = (start_idx, end_leaf).into();
                    let ring = Ring::ControlBlock(twig, inner);
                    return Some((ring, end_leaf + 1));
                }
                // This closes a nested block, decrement depth
                depth = depth.saturating_sub(1);
            }
            curr_idx += 1;
        }
        // Didn't find a matching close tag
        None
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

        (last_idx > start_idx).then(|| {
            let ring = Ring::TextSequence((start_idx, last_idx).into(), inner_rings);
            (ring, curr_idx)
        })
    }

    fn find_placeholders(
        text: &str,
        askama_nodes: &[AskamaNode],
    ) -> BTreeMap<usize, (String, usize)> {
        let mut placeholders = BTreeMap::new();

        for (idx, askama_node) in askama_nodes.iter().enumerate() {
            let placeholder = askama_node.placeholder(idx);

            // Find all occurrences of this placeholder
            let mut search_pos = 0;
            while let Some(pos) = text[search_pos..].find(&placeholder) {
                let absolute_pos = search_pos + pos;
                placeholders.insert(absolute_pos, (placeholder.clone(), idx));
                search_pos = absolute_pos + placeholder.len();
            }
        }

        placeholders
    }

    fn replace_placeholders_in_text(
        &mut self,
        text: &str,
        placeholders: BTreeMap<usize, (String, usize)>,
        askama_nodes: &[AskamaNode],
    ) {
        let config = self.config.clone();
        let mut last_end = 0;

        for (pos, (placeholder, askama_idx)) in placeholders {
            // Add text before this placeholder
            if pos > last_end {
                let text_segment = &text[last_end..pos];
                self.leaves.push(Leaf::HtmlText(text_segment.to_string()));
            }

            // Add the Askama node
            self.leaves
                .push(Leaf::from_askama(&config, &askama_nodes[askama_idx]));

            last_end = pos + placeholder.len();
        }

        // Add any remaining text after the last placeholder
        if last_end < text.len() {
            let remaining = &text[last_end..];
            self.leaves.push(Leaf::HtmlText(remaining.to_string()));
        }
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
            | Self::InlineText(twig)
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
