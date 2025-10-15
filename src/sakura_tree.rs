use std::collections::{BTreeMap, HashMap};
use std::iter::once;

use crate::{
    askama::{self, AskamaNode},
    config::Config,
    html::HtmlNode,
};

#[derive(Debug, Clone)]
pub(crate) struct SakuraTree {
    pub(crate) config: Config,
    pub(crate) leaves: Vec<Leaf>,
    pub(crate) rings: Vec<Ring>,
    pub(crate) branches: Vec<Branch>,
    // Maps start_leaf index to end_leaf index
    pub(crate) twigs: HashMap<usize, usize>,
}

#[derive(Debug, Clone)]
pub(crate) struct Leaf {
    // The source of this leaf (Askama or HTML)
    pub(crate) root: Root,
    // The rendered content for this leaf
    pub(crate) content: String,
}

#[derive(Debug, Clone)]
pub(crate) enum Root {
    Askama(AskamaNode),
    Html(HtmlNode),
}

#[derive(Debug, Clone)]
pub(crate) struct Ring {
    pub(crate) layer: Layer,
    pub(crate) total_chars: usize,
}

#[derive(Debug, Clone)]
pub(crate) enum Layer {
    // Complete HTML element with start/end tags
    CompleteElement {
        start_leaf: usize,
        inner_rings: Vec<Ring>,
        end_leaf: usize,
        is_semantic_inline: bool,
    },
    // Sequence of text content and expressions
    TextSequence {
        leaves: Vec<usize>,
    },
    // Raw text (inside style/script elements)
    ScriptStyle {
        leaf: usize,
    },
    // Askama control block with open/close tag indices
    ControlBlock {
        open_leaf: usize,
        inner_rings: Vec<Ring>,
        close_leaf: usize,
    },
    // Empty Askama control block with open/close tag indices
    EmptyControlBlock {
        open_leaf: usize,
        close_leaf: usize,
    },
    // Fallback leaf (self-closing elements, standalone text, etc.)
    Standalone {
        leaf: usize,
    },
}

#[derive(Debug, Clone)]
pub(crate) struct Branch {
    // Indices of leaves that belong to this branch
    pub(crate) leaves: Vec<usize>,
    // A formatting style hint for the entire branch
    pub(crate) style: BranchStyle,
    // Indentation level for this branch
    pub(crate) indent: i32,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum BranchStyle {
    Inline,
    MultiLine,
    Wrapped,
    Raw,
}

impl Leaf {
    pub(crate) fn from_askama(config: &Config, askama_node: AskamaNode) -> Self {
        let content = askama::format_askama_node(config, &askama_node);
        Self {
            content,
            root: Root::Askama(askama_node),
        }
    }

    pub(crate) fn from_html(html_node: HtmlNode) -> Self {
        let content = html_node.to_string();
        Self {
            content,
            root: Root::Html(html_node),
        }
    }

    pub(crate) fn from_html_text(text: &str) -> Self {
        let html_node = HtmlNode::Text(text.to_string());
        Self {
            content: text.to_string(),
            root: Root::Html(html_node),
        }
    }

    pub(crate) fn from_html_raw_text(text: &str) -> Self {
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

    pub(crate) fn is_html_text(&self) -> bool {
        matches!(self.root, Root::Html(HtmlNode::Text(_)))
    }
    pub(crate) fn is_html_entity(&self) -> bool {
        matches!(self.root, Root::Html(HtmlNode::Entity(_)))
    }
    pub(crate) fn chars_count(&self) -> usize {
        self.content.chars().count()
    }
    pub(crate) fn maybe_askama_node(&self) -> Option<&AskamaNode> {
        match &self.root {
            Root::Askama(node) => Some(node),
            Root::Html(_) => None,
        }
    }
}

impl Branch {
    pub(crate) fn grow(leaves: Vec<usize>, style: BranchStyle, indent: i32) -> Self {
        Self {
            leaves,
            style,
            indent,
        }
    }
}

impl SakuraTree {
    pub(crate) fn grow(
        askama_nodes: &[AskamaNode],
        html_nodes: &[HtmlNode],
        config: Config,
    ) -> Self {
        let mut tree = Self {
            config,
            leaves: Vec::new(),
            rings: Vec::new(),
            branches: Vec::new(),
            twigs: HashMap::new(),
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

        // Convert metadata from html indices to leaf indices and store as twigs
        for (leaf_idx, leaf) in tree.leaves.iter().enumerate() {
            if let Root::Html(HtmlNode::StartTag {
                end_tag_idx: Some(elem_end_tag_idx),
                ..
            }) = &leaf.root
            {
                // Convert html end_tag index to end_leaf index
                if let Some(&end_leaf) = html_to_leaf_map.get(elem_end_tag_idx) {
                    tree.twigs.insert(leaf_idx, end_leaf);
                }
            }
        }

        let rings = tree.grow_rings(0, tree.leaves.len());
        tree.rings.extend(rings);

        tree
    }

    // Grow concentric rings
    fn grow_rings(&self, start_idx: usize, end_idx: usize) -> Vec<Ring> {
        let mut rings = Vec::new();
        let mut idx = start_idx;

        while idx < end_idx {
            if let Some(leaf) = self.leaves.get(idx) {
                match &leaf.root {
                    // Try to build when clause (they need to claim their inline content)
                    Root::Askama(askama_node) if !askama_node.is_expr() => {
                        // Check if this is a "when" clause (inner match block)
                        // These should be inline with their content
                        if let Some(tag) = askama_node.get_ctrl_tag()
                            && tag.boundary() == askama::Boundary::Inner
                        {
                            // Build text sequence starting with this when clause
                            // This will collect the when clause + all its inline content
                            if let Some((ring, next_idx)) =
                                self.try_when_clause_with_inline_content(idx, end_idx)
                            {
                                rings.push(ring);
                                idx = next_idx;
                                continue;
                            }
                        } else {
                            // Regular control block (if/for/match/etc)
                            if let Some((ring, next_idx)) =
                                self.try_askama_block(idx, askama_node, end_idx)
                            {
                                rings.push(ring);
                                idx = next_idx;
                                continue;
                            }
                        }
                    }
                    // Try to build complete HTML elements
                    Root::Html(HtmlNode::StartTag { .. }) => {
                        if let Some((ring, next_idx)) = self.try_complete_element(idx) {
                            rings.push(ring);
                            idx = next_idx;
                            continue;
                        }
                    }
                    // Standalone for void or self-closing elements
                    Root::Html(HtmlNode::Void { .. } | HtmlNode::SelfClosingTag { .. }) => {
                        let layer = Layer::Standalone { leaf: idx };
                        let ring = Ring::new(layer, self);
                        rings.push(ring);
                        idx += 1;
                        continue;
                    }
                    // Raw text (script/style content)
                    Root::Html(HtmlNode::RawText(_)) => {
                        let layer = Layer::ScriptStyle { leaf: idx };
                        let ring = Ring::new(layer, self);
                        rings.push(ring);
                        idx += 1;
                        continue;
                    }
                    // Try to build text sequences
                    Root::Html(HtmlNode::Text(_)) | Root::Askama(_) => {
                        if let Some((ring, next_idx)) = self.try_text_sequence(idx, end_idx) {
                            rings.push(ring);
                            idx = next_idx;
                            continue;
                        }
                    }
                    Root::Html(_) => {}
                }
            }

            // Fallback: Standalone
            let layer = Layer::Standalone { leaf: idx };
            let ring = Ring::new(layer, self);
            rings.push(ring);
            idx += 1;
        }

        rings
    }

    fn try_complete_element(&self, start_leaf: usize) -> Option<(Ring, usize)> {
        // Get the end_leaf for this element
        let &end_leaf = self.twigs.get(&start_leaf)?;

        // Recursively grow inner rings
        let inner_rings = self.grow_rings(start_leaf + 1, end_leaf);

        // Get inline status from the start tag
        let leaf = self.leaves.get(start_leaf)?;
        let is_semantic_inline = match &leaf.root {
            Root::Html(html_node) => html_node.is_inline_level(),
            Root::Askama(_) => false,
        };

        let layer = Layer::CompleteElement {
            start_leaf,
            inner_rings,
            end_leaf,
            is_semantic_inline,
        };
        let ring = Ring::new(layer, self);

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
                        let close_leaf = curr_idx;

                        // Recursively grow inner rings for leaves between open and close indices
                        let inner_rings = self.grow_rings(start_idx + 1, close_leaf);

                        let layer = if inner_rings.is_empty() {
                            Layer::EmptyControlBlock {
                                open_leaf: start_idx,
                                close_leaf,
                            }
                        } else {
                            Layer::ControlBlock {
                                open_leaf: start_idx,
                                inner_rings,
                                close_leaf,
                            }
                        };
                        let ring = Ring::new(layer, self);
                        return Some((ring, close_leaf + 1));
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

    // Build a when clause with all its inline content (including complete inline elements)
    // All or nothing: either all content fits (up to 2x max line length), or the when clause stays alone
    fn try_when_clause_with_inline_content(
        &self,
        start_idx: usize,
        end_idx: usize,
    ) -> Option<(Ring, usize)> {
        // TODO: new configuration value?
        let permissive_limit = self.config.max_width * 3 / 2;

        let mut candidate_leaves = vec![start_idx];
        let mut curr_idx = start_idx + 1;

        // 1. Collect all potential leaves until we hit a control block/comment
        while curr_idx < end_idx {
            let Some(leaf) = self.leaves.get(curr_idx) else {
                break;
            };

            // Break immediately on Askama control blocks or comments
            if matches!(&leaf.root, Root::Askama(node) if node.is_ctrl() || node.is_comment()) {
                break;
            }

            // For StartTags, include the complete element without splitting it
            if matches!(leaf.root, Root::Html(HtmlNode::StartTag { .. })) {
                // Try to find the complete element boundaries
                if let Some(&end_leaf) = self.twigs.get(&curr_idx) {
                    // Include all leaves from current to end
                    for idx in curr_idx..=end_leaf {
                        candidate_leaves.push(idx);
                    }
                    curr_idx = end_leaf + 1;
                    continue;
                }
            }

            // For everything else (text, entities, expressions, etc.)
            candidate_leaves.push(curr_idx);
            curr_idx += 1;
        }

        // 2. Calculate total chars for all candidates
        let total_chars: usize = candidate_leaves
            .iter()
            .filter_map(|&idx| self.leaves.get(idx))
            .map(Leaf::chars_count)
            .sum();

        // 3. All or nothing
        let final_leaves = if total_chars > permissive_limit {
            vec![start_idx]
        } else {
            candidate_leaves
        };

        if final_leaves.len() > 1 {
            let layer = Layer::TextSequence {
                leaves: final_leaves,
            };
            let ring = Ring::new(layer, self);
            return Some((ring, curr_idx));
        }

        None
    }

    fn try_text_sequence(&self, start_idx: usize, end_idx: usize) -> Option<(Ring, usize)> {
        let mut leaves = vec![start_idx];

        // Collect text content and expressions that can be grouped
        let mut curr_idx = start_idx + 1;
        while curr_idx < end_idx {
            let Some(leaf) = self.leaves.get(curr_idx) else {
                break;
            };

            let can_include = match &leaf.root {
                Root::Html(HtmlNode::Text(_)) => !leaf.content.trim().is_empty(),
                Root::Html(HtmlNode::Entity(_)) => true,
                Root::Askama(node) => node.is_expr(),
                Root::Html(html_node) => html_node.is_inline_level(),
            };

            if can_include {
                leaves.push(curr_idx);
                curr_idx += 1;
            } else {
                break;
            }
        }

        if leaves.len() > 1 {
            let layer = Layer::TextSequence { leaves };
            let ring = Ring::new(layer, self);
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
    pub(crate) fn new(layer: Layer, tree: &SakuraTree) -> Self {
        let total_chars = Self::calculate_total_chars(&layer, tree);
        Self { layer, total_chars }
    }

    fn calculate_total_chars(layer: &Layer, tree: &SakuraTree) -> usize {
        let leaf_indices = layer.all_leaf_indices();
        leaf_indices
            .iter()
            .filter_map(|&i| tree.leaves.get(i))
            .map(Leaf::chars_count)
            .sum()
    }

    pub(crate) fn all_leaf_indices(&self) -> Vec<usize> {
        self.layer.all_leaf_indices()
    }

    // Check if the inner rings contain any semantic multi-line content
    pub(crate) fn inner_has_multi_line_content(&self) -> bool {
        match &self.layer {
            Layer::CompleteElement { inner_rings, .. }
            | Layer::ControlBlock { inner_rings, .. } => inner_rings.iter().any(|ring| {
                match &ring.layer {
                    // Look for non-semantic inline elements
                    Layer::CompleteElement {
                        is_semantic_inline, ..
                    } => !is_semantic_inline || ring.inner_has_multi_line_content(),
                    // Control blocks and script/style elements are always multi-line
                    Layer::ControlBlock { .. } | Layer::ScriptStyle { .. } => true,
                    _ => ring.inner_has_multi_line_content(),
                }
            }),
            _ => false,
        }
    }
}

impl Layer {
    pub(crate) fn all_leaf_indices(&self) -> Vec<usize> {
        match self {
            Self::CompleteElement {
                start_leaf,
                inner_rings,
                end_leaf,
                ..
            } => once(*start_leaf)
                .chain(inner_rings.iter().flat_map(Ring::all_leaf_indices))
                .chain((*end_leaf != *start_leaf).then_some(*end_leaf)) // Avoid duplicates for void elements
                .collect(),
            Self::TextSequence { leaves } => leaves.clone(),
            Self::ControlBlock {
                open_leaf,
                inner_rings,
                close_leaf,
                ..
            } => once(*open_leaf)
                .chain(inner_rings.iter().flat_map(Ring::all_leaf_indices))
                .chain(once(*close_leaf))
                .collect(),
            Self::EmptyControlBlock {
                open_leaf,
                close_leaf,
                ..
            } => vec![*open_leaf, *close_leaf],
            Self::ScriptStyle { leaf } | Self::Standalone { leaf } => vec![*leaf],
        }
    }
}
