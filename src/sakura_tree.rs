use std::collections::{BTreeMap, HashMap};
use std::ops::RangeInclusive;

use crate::{
    askama::{self, AskamaNode},
    config::Config,
    html::HtmlNode,
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
pub struct Leaf {
    // The source of this leaf (Askama or HTML)
    pub root: Root,
    // The rendered content for this leaf
    pub content: String,
}

#[derive(Debug, Clone)]
pub enum Root {
    Askama(AskamaNode),
    Html(HtmlNode),
}

#[derive(Debug, Clone)]
pub enum Ring {
    // Compound
    Element(Twig, Option<Vec<Ring>>),
    ControlBlock(Twig, Option<Vec<Ring>>),
    MatchArm(Twig, Option<Vec<Ring>>),

    // Atomic
    TextSequence(Twig),
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
    Multiple,
    AskamaComment,
    Raw,
}

impl Leaf {
    fn from_askama(config: &Config, askama_node: AskamaNode) -> Self {
        let content = askama::format_askama_node(config, &askama_node);
        Self {
            content,
            root: Root::Askama(askama_node),
        }
    }

    fn from_html(html_node: HtmlNode) -> Self {
        let content = html_node.to_string();
        Self {
            content,
            root: Root::Html(html_node),
        }
    }

    fn from_html_text(text: &str) -> Self {
        let html_node = HtmlNode::Text(text.to_string());
        Self {
            content: text.to_string(),
            root: Root::Html(html_node),
        }
    }

    fn from_html_raw_text(text: &str) -> Self {
        let html_node = HtmlNode::RawText(text.to_string());

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

        Self {
            content: normalized_content,
            root: Root::Html(html_node),
        }
    }

    pub fn is_html_text(&self) -> bool {
        matches!(self.root, Root::Html(HtmlNode::Text(_)))
    }

    fn chars_count(&self) -> usize {
        self.content.chars().count()
    }
    pub fn maybe_askama_node(&self) -> Option<&AskamaNode> {
        match &self.root {
            Root::Askama(node) => Some(node),
            Root::Html(_) => None,
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
    pub fn grow(askama_nodes: &[AskamaNode], html_nodes: &[HtmlNode], config: Config) -> Self {
        let mut tree = Self {
            config,
            leaves: Vec::new(),
            rings: Vec::new(),
            branches: Vec::new(),
            twigs: Vec::new(),
        };

        let mut html_to_leaf_map: HashMap<usize, usize> = HashMap::new();

        // Convert each HtmlNode to SakuraLeaf while replacing placeholders
        for (i, html_node) in html_nodes.iter().enumerate() {
            // Record the mapping before processing
            let leaf_idx = tree.leaves.len();
            html_to_leaf_map.insert(i, leaf_idx);

            // Dispatch based on node type
            match html_node {
                HtmlNode::Text(text) => {
                    let placeholders = Self::find_placeholders(text, askama_nodes);
                    if placeholders.is_empty() {
                        tree.leaves.push(Leaf::from_html_text(text));
                    } else {
                        tree.replace_placeholders_in_text(text, placeholders);
                    }
                }
                HtmlNode::RawText(text) => {
                    let processed = askama::replace_placeholder_in_raw_text(text, askama_nodes);
                    tree.leaves.push(Leaf::from_html_raw_text(&processed));
                }
                HtmlNode::Entity(_) | HtmlNode::Comment(_) | HtmlNode::Doctype(_) => {
                    tree.leaves.push(Leaf::from_html(html_node.clone()));
                }
                _ => {
                    let processed = html_node.clone().replace_placeholder(askama_nodes);
                    tree.leaves.push(Leaf::from_html(processed));
                }
            }
        }

        // Initialize twigs with same index for each leaf
        tree.twigs = (0..tree.leaves.len()).map(|i| Twig(i, i)).collect();

        // Convert html end_tag index to end_leaf index
        for (leaf_idx, leaf) in tree.leaves.iter().enumerate() {
            if let Root::Html(HtmlNode::StartTag {
                end_tag_idx: Some(elem_end_tag_idx),
                ..
            }) = &leaf.root
                && let Some(&end_leaf) = html_to_leaf_map.get(elem_end_tag_idx)
            {
                tree.twigs[leaf_idx] = Twig(leaf_idx, end_leaf);
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

            let (ring, next_idx) = match &leaf.root {
                Root::Askama(node) if node.is_match_arm() => {
                    self.match_arm_with_content(idx, end_idx)
                }
                Root::Askama(node) if !node.is_expr() => self
                    .try_askama_block(idx, node, end_idx)
                    .unwrap_or_else(|| (self.with_single_leaf(idx), idx + 1)),
                Root::Html(HtmlNode::StartTag { .. }) => self.handle_start_tag(idx, end_idx),
                Root::Html(HtmlNode::RawText(_)) => (Ring::RawText(idx.into()), idx + 1),
                Root::Html(HtmlNode::Text(_)) | Root::Askama(_) => self
                    .try_text_sequence(idx, end_idx)
                    .unwrap_or_else(|| (self.with_single_leaf(idx), idx + 1)),
                Root::Html(_) => (self.with_single_leaf(idx), idx + 1),
            };

            rings.push(ring);
            idx = next_idx;
        }

        Some(rings)
    }

    fn with_single_leaf(&self, idx: usize) -> Ring {
        let leaf = &self.leaves[idx];
        let twig = idx.into();

        match &leaf.root {
            Root::Html(HtmlNode::RawText(_)) => Ring::RawText(twig),
            Root::Html(HtmlNode::Comment(_)) => Ring::Comment(twig),
            Root::Askama(node) if node.is_comment() => Ring::Comment(twig),
            Root::Html(HtmlNode::Text(_) | HtmlNode::Entity(_)) => Ring::InlineText(twig),
            _ => Ring::Other(twig),
        }
    }

    fn handle_start_tag(&self, idx: usize, end_idx: usize) -> (Ring, usize) {
        let leaf = &self.leaves[idx];

        // Try text sequence for inline elements followed by text/expr
        let Root::Html(html_node) = &leaf.root else {
            unreachable!()
        };
        if html_node.is_inline_level()
            && !self.twigs[idx].has_same_idx()
            && self
                .leaves
                .get(self.twigs[idx].end() + 1)
                .is_some_and(|next| match &next.root {
                    Root::Html(HtmlNode::Text(_)) => true,
                    Root::Askama(node) => node.is_expr(),
                    Root::Html(_) => false,
                })
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

    fn try_askama_block(
        &self,
        start_idx: usize,
        askama_node: &AskamaNode,
        end_idx: usize,
    ) -> Option<(Ring, usize)> {
        // Must have an opening control tag
        let open_tag = askama_node.get_ctrl_tag()?;
        if open_tag.boundary() != askama::Boundary::Open {
            return None;
        }
        // Look for matching close block, tracking nesting depth
        let mut curr_idx = start_idx + 1;
        let mut depth: u32 = 0;

        while curr_idx < end_idx && depth < 200 {
            let leaf = self.leaves.get(curr_idx)?;

            if let Root::Askama(node) = &leaf.root
                && let Some(tag) = node.get_ctrl_tag()
            {
                // Check if this is another opening of the same type (nested block)
                if tag.boundary() == askama::Boundary::Open && open_tag.same_kind(tag) {
                    depth += 1;
                } else if tag.boundary() == askama::Boundary::Close && open_tag.matches_close(tag) {
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

            // Stop at next When, Else, or Endmatch
            if let Root::Askama(node) = &leaf.root
                && let Some(tag) = node.get_ctrl_tag()
                && matches!(
                    tag,
                    askama::ControlTag::When(_)
                        | askama::ControlTag::Else(_)
                        | askama::ControlTag::Endmatch(_)
                )
            {
                break;
            }

            // If start tag, skip to after the end tag
            if matches!(leaf.root, Root::Html(HtmlNode::StartTag { .. }))
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
        let temp_ring = Ring::TextSequence(twig);
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

        // If starting with a StartTag, include all leaves up to its end tag
        if let Some(leaf) = self.leaves.get(start_idx)
            && matches!(leaf.root, Root::Html(HtmlNode::StartTag { .. }))
            && !self.twigs[start_idx].has_same_idx()
        {
            last_idx = self.twigs[start_idx].end();
            curr_idx = self.twigs[start_idx].end() + 1;
        }

        // Collect following inline content
        while curr_idx < end_idx {
            let leaf = &self.leaves[curr_idx];

            if matches!(leaf.root, Root::Html(HtmlNode::StartTag { .. }))
                && !self.twigs[curr_idx].has_same_idx()
                && let Root::Html(html_node) = &leaf.root
                && html_node.is_inline_level()
            {
                last_idx = self.twigs[curr_idx].end();
                curr_idx = self.twigs[curr_idx].end() + 1;
                continue;
            }

            let can_include = match &leaf.root {
                Root::Askama(node) => node.is_expr(),
                Root::Html(html_node) => html_node.is_inline_level(),
            };

            if can_include {
                last_idx = curr_idx;
                curr_idx += 1;
            } else {
                break;
            }
        }

        if last_idx > start_idx {
            let ring = Ring::TextSequence((start_idx, last_idx).into());
            return Some((ring, curr_idx));
        }

        None
    }

    fn find_placeholders(
        text: &str,
        askama_nodes: &[AskamaNode],
    ) -> BTreeMap<usize, (String, AskamaNode)> {
        let mut placeholders = BTreeMap::new();

        for (idx, askama_node) in askama_nodes.iter().enumerate() {
            let placeholder = askama_node.placeholder(idx);

            // Find all occurrences of this placeholder
            let mut search_pos = 0;
            while let Some(pos) = text[search_pos..].find(&placeholder) {
                let absolute_pos = search_pos + pos;
                placeholders.insert(absolute_pos, (placeholder.clone(), askama_node.clone()));
                search_pos = absolute_pos + placeholder.len();
            }
        }

        placeholders
    }

    fn replace_placeholders_in_text(
        &mut self,
        text: &str,
        placeholders: BTreeMap<usize, (String, AskamaNode)>,
    ) {
        let config = self.config.clone();
        let mut last_end = 0;

        for (pos, (placeholder, askama_node)) in placeholders {
            // Add text before this placeholder
            if pos > last_end {
                let text_segment = &text[last_end..pos];
                self.leaves.push(Leaf::from_html_text(text_segment));
            }

            // Add the Askama node
            self.leaves
                .push(Leaf::from_askama(&config, askama_node.clone()));

            last_end = pos + placeholder.len();
        }

        // Add any remaining text after the last placeholder
        if last_end < text.len() {
            let remaining = &text[last_end..];
            self.leaves.push(Leaf::from_html_text(remaining));
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
                        let Root::Html(html_node) = &tree.leaves[start].root else {
                            unreachable!()
                        };
                        !html_node.is_inline_level()
                    }
                    Self::ControlBlock(_, _) | Self::RawText(_) => true,
                    _ => node.has_block(tree),
                })
            }
            Self::ControlBlock(_, Some(inner)) => inner.iter().any(|node| node.has_block(tree)),
            Self::MatchArm(_, Some(inner)) => inner.iter().any(|node| node.has_block(tree)),
            Self::RawText(_) => true,
            _ => false,
        }
    }

    pub fn twig(&self) -> Twig {
        match self {
            Self::Element(twig, _)
            | Self::TextSequence(twig)
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
