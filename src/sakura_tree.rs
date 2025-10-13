use std::collections::{BTreeMap, HashMap};

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
    pub(crate) twigs: HashMap<usize, Twig>,
}

#[derive(Debug, Clone)]
pub(crate) struct Twig {
    pub(crate) end_leaf: usize,
    pub(crate) chars_count: usize,
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
        chars_count: usize,
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
    pub(crate) leaf_indices: Vec<usize>,
    // A formatting style hint for the entire branch
    pub(crate) style: BranchStyle,
    // Indentation level for this branch
    pub(crate) indent_level: i32,
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
        let normalized_content = Self::normalize_raw_text_preserving_lines(text);
        Self {
            content: normalized_content,
            root: Root::Html(html_node),
        }
    }

    fn normalize_raw_text_preserving_lines(text: &str) -> String {
        let trimmed = text.trim_matches('\n');
        if trimmed.is_empty() {
            return String::new();
        }
        trimmed
            .lines()
            .map(str::trim)
            .collect::<Vec<_>>()
            .join("\n")
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
    pub(crate) fn grow(leaf_indices: Vec<usize>, style: BranchStyle, indent_level: i32) -> Self {
        Self {
            leaf_indices,
            style,
            indent_level,
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
            let leaf_index = tree.leaf_count();
            html_to_leaf_map.insert(i, leaf_index);

            // Dispatch based on node type
            process_html_node(html_node, &mut tree, askama_nodes);
        }

        // Convert metadata from html indices to leaf indices and store as twigs
        for (leaf_idx, leaf) in tree.leaves.iter().enumerate() {
            if let Root::Html(HtmlNode::StartTag {
                element_metadata: Some(html_metadata),
                ..
            }) = &leaf.root
            {
                // Convert html end_tag index to end_leaf index
                if let Some(&end_leaf) = html_to_leaf_map.get(&html_metadata.end_tag_index) {
                    tree.twigs.insert(
                        leaf_idx,
                        Twig {
                            chars_count: html_metadata.chars_count,
                            end_leaf,
                        },
                    );
                }
            }
        }

        tree.grow_layers();

        tree
    }

    pub(crate) fn grow_leaf(&mut self, leaf: Leaf) {
        self.leaves.push(leaf);
    }
    pub(crate) fn grow_layer(&mut self, ring: Ring) {
        self.rings.push(ring);
    }
    pub(crate) fn grow_branch(&mut self, branch: Branch) {
        self.branches.push(branch);
    }
    pub(crate) fn leaf_count(&self) -> usize {
        self.leaves.len()
    }
    pub(crate) fn get_leaf(&self, index: usize) -> Option<&Leaf> {
        self.leaves.get(index)
    }
    pub(crate) fn iter_leaves(&self) -> std::slice::Iter<'_, Leaf> {
        self.leaves.iter()
    }
    pub(crate) fn iter_rings(&self) -> std::slice::Iter<'_, Ring> {
        self.rings.iter()
    }
    pub(crate) fn iter_branches(&self) -> std::slice::Iter<'_, Branch> {
        self.branches.iter()
    }

    fn grow_layers(&mut self) {
        let rings = self.grow_rings(0, self.leaf_count());

        for ring in rings {
            self.grow_layer(ring);
        }
    }

    // Grow concentric rings
    fn grow_rings(&self, start_index: usize, end_index: usize) -> Vec<Ring> {
        let mut rings = Vec::new();
        let mut index = start_index;

        while index < end_index {
            if let Some(leaf) = self.get_leaf(index) {
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
                            if let Some((ring, next_index)) =
                                self.try_when_clause_with_inline_content(index, end_index)
                            {
                                rings.push(ring);
                                index = next_index;
                                continue;
                            }
                        } else {
                            // Regular control block (if/for/match/etc)
                            if let Some((ring, next_index)) =
                                self.try_askama_block(index, askama_node, end_index)
                            {
                                rings.push(ring);
                                index = next_index;
                                continue;
                            }
                        }
                    }
                    // Try to build complete HTML elements
                    Root::Html(HtmlNode::StartTag { .. }) => {
                        if let Some((ring, next_index)) = self.try_complete_element(index) {
                            rings.push(ring);
                            index = next_index;
                            continue;
                        }
                    }
                    // Standalone for void or self-closing elements
                    Root::Html(HtmlNode::Void { .. } | HtmlNode::SelfClosingTag { .. }) => {
                        let layer = Layer::Standalone { leaf: index };
                        let ring = Ring::new(layer, self);
                        rings.push(ring);
                        index += 1;
                        continue;
                    }
                    // Raw text (script/style content)
                    Root::Html(HtmlNode::RawText(_)) => {
                        let layer = Layer::ScriptStyle { leaf: index };
                        let ring = Ring::new(layer, self);
                        rings.push(ring);
                        index += 1;
                        continue;
                    }
                    // Try to build text sequences
                    Root::Html(HtmlNode::Text(_)) | Root::Askama(_) => {
                        if let Some((ring, next_index)) = self.try_text_sequence(index, end_index) {
                            rings.push(ring);
                            index = next_index;
                            continue;
                        }
                    }
                    Root::Html(_) => {}
                }
            }

            // Fallback: Standalone
            let layer = Layer::Standalone { leaf: index };
            let ring = Ring::new(layer, self);
            rings.push(ring);
            index += 1;
        }

        rings
    }

    fn try_complete_element(&self, start_leaf: usize) -> Option<(Ring, usize)> {
        // Get the twig for this element
        let twig = self.twigs.get(&start_leaf)?;
        let end_leaf = twig.end_leaf;
        let chars_count = twig.chars_count;

        // Recursively grow inner rings
        let inner_rings = self.grow_rings(start_leaf + 1, end_leaf);

        // Get inline status from the start tag
        let leaf = self.get_leaf(start_leaf)?;
        let is_semantic_inline = match &leaf.root {
            Root::Html(html_node) => html_node.is_inline_level(),
            Root::Askama(_) => false,
        };

        let layer = Layer::CompleteElement {
            chars_count,
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
        start_index: usize,
        askama_node: &AskamaNode,
        end_index: usize,
    ) -> Option<(Ring, usize)> {
        // Must have an opening control tag
        let open_tag = askama_node.get_ctrl_tag()?;
        if open_tag.boundary() != askama::Boundary::Open {
            return None;
        }
        // Look for matching close block, tracking nesting depth
        let mut current_index = start_index + 1;
        let mut depth: u32 = 0;

        while current_index < end_index && depth < 200 {
            let leaf = self.get_leaf(current_index)?;

            if let Root::Askama(node) = &leaf.root
                && let Some(tag) = node.get_ctrl_tag()
            {
                // Check if this is another opening of the same type (nested block)
                if tag.boundary() == askama::Boundary::Open && open_tag.same_kind(tag) {
                    depth += 1;
                } else if tag.boundary() == askama::Boundary::Close && open_tag.matches_close(tag) {
                    if depth == 0 {
                        // Found our matching close at depth 0
                        let close_leaf = current_index;

                        // Recursively grow inner rings for leaves between open and close indices
                        let inner_rings = self.grow_rings(start_index + 1, close_leaf);

                        let layer = if inner_rings.is_empty() {
                            Layer::EmptyControlBlock {
                                open_leaf: start_index,
                                close_leaf,
                            }
                        } else {
                            Layer::ControlBlock {
                                open_leaf: start_index,
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
            current_index += 1;
        }
        // Didn't find a matching close tag
        None
    }

    // Find the end leaf index of a complete element
    fn find_complete_element_end(&self, start_leaf: usize) -> Option<usize> {
        self.twigs.get(&start_leaf).map(|twig| twig.end_leaf)
    }

    // Build a when clause with all its inline content (including complete inline elements)
    // All or nothing: either all content fits (up to 2x max line length), or the when clause stays alone
    fn try_when_clause_with_inline_content(
        &self,
        start_index: usize,
        end_index: usize,
    ) -> Option<(Ring, usize)> {
        // TODO: new configuration value?
        let permissive_limit = self.config.max_line_length * 3 / 2;

        let mut candidate_leaves = vec![start_index];
        let mut current_index = start_index + 1;

        // 1. Collect all potential leaves until we hit a control block/comment
        while current_index < end_index {
            let Some(leaf) = self.get_leaf(current_index) else {
                break;
            };

            // Break immediately on Askama control blocks or comments
            if matches!(&leaf.root, Root::Askama(node) if node.is_ctrl() || node.is_comment()) {
                break;
            }

            // For StartTags, include the complete element without splitting it
            if matches!(leaf.root, Root::Html(HtmlNode::StartTag { .. })) {
                // Try to find the complete element boundaries
                if let Some(end_leaf) = self.find_complete_element_end(current_index) {
                    // Include all leaves from current to end
                    for idx in current_index..=end_leaf {
                        candidate_leaves.push(idx);
                    }
                    current_index = end_leaf + 1;
                    continue;
                }
            }

            // For everything else (text, entities, expressions, etc.)
            candidate_leaves.push(current_index);
            current_index += 1;
        }

        // 2. Calculate total chars for all candidates
        let total_chars: usize = candidate_leaves
            .iter()
            .filter_map(|&idx| self.get_leaf(idx))
            .map(Leaf::chars_count)
            .sum();

        // 3. All or nothing
        let final_leaves = if total_chars > permissive_limit {
            vec![start_index]
        } else {
            candidate_leaves
        };

        if final_leaves.len() > 1 {
            let layer = Layer::TextSequence {
                leaves: final_leaves,
            };
            let ring = Ring::new(layer, self);
            return Some((ring, current_index));
        }

        None
    }

    fn try_text_sequence(&self, start_index: usize, end_index: usize) -> Option<(Ring, usize)> {
        let mut leaves = vec![start_index];

        // Collect text content and expressions that can be grouped
        let mut current_index = start_index + 1;
        while current_index < end_index {
            let Some(leaf) = self.get_leaf(current_index) else {
                break;
            };

            let can_include = match &leaf.root {
                Root::Html(HtmlNode::Text(_)) => !leaf.content.trim().is_empty(),
                Root::Html(HtmlNode::Entity(_)) => true,
                Root::Askama(node) => node.is_expr(),
                Root::Html(html_node) => html_node.is_inline_level(),
            };

            if can_include {
                leaves.push(current_index);
                current_index += 1;
            } else {
                break;
            }
        }

        if leaves.len() > 1 {
            let layer = Layer::TextSequence { leaves };
            let ring = Ring::new(layer, self);
            return Some((ring, current_index));
        }

        None
    }
}

impl Ring {
    pub(crate) fn new(layer: Layer, tree: &SakuraTree) -> Self {
        let total_chars = match &layer {
            Layer::CompleteElement { chars_count, .. } => *chars_count,
            _ => Self::calculate_total_chars(&layer, tree),
        };
        Self { layer, total_chars }
    }

    fn calculate_total_chars(layer: &Layer, tree: &SakuraTree) -> usize {
        let leaf_indices = layer.all_leaf_indices();
        leaf_indices
            .iter()
            .filter_map(|&i| tree.get_leaf(i))
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
            Layer::CompleteElement {
                start_leaf,
                inner_rings,
                end_leaf,
                ..
            } => {
                let mut indices = vec![*start_leaf];
                for ring in inner_rings {
                    indices.extend(ring.all_leaf_indices());
                }
                // Avoid duplicates for void elements
                if end_leaf != start_leaf {
                    indices.push(*end_leaf);
                }
                indices
            }
            Layer::TextSequence { leaves } => leaves.clone(),
            Layer::ControlBlock {
                open_leaf,
                inner_rings,
                close_leaf,
                ..
            } => {
                let mut indices = vec![*open_leaf];
                for ring in inner_rings {
                    indices.extend(ring.all_leaf_indices());
                }
                indices.push(*close_leaf);
                indices
            }
            Layer::EmptyControlBlock {
                open_leaf,
                close_leaf,
                ..
            } => {
                vec![*open_leaf, *close_leaf]
            }
            Layer::ScriptStyle { leaf } | Layer::Standalone { leaf } => {
                vec![*leaf]
            }
        }
    }
}

fn process_html_node(html_node: &HtmlNode, tree: &mut SakuraTree, askama_nodes: &[AskamaNode]) {
    match html_node {
        HtmlNode::Text(text) => process_text_node(text, tree, askama_nodes),
        HtmlNode::RawText(text) => process_raw_text_node(text, tree, askama_nodes),
        HtmlNode::Entity(_) | HtmlNode::Comment(_) | HtmlNode::Doctype(_) => {
            // Simple nodes - direct conversion
            tree.grow_leaf(Leaf::from_html(html_node.clone()));
        }
        _ => process_element_node(html_node, tree, askama_nodes),
    }
}

fn process_text_node(text: &str, tree: &mut SakuraTree, askama_nodes: &[AskamaNode]) {
    let placeholders = find_placeholders(text, askama_nodes);

    if placeholders.is_empty() {
        tree.grow_leaf(Leaf::from_html_text(text));
    } else {
        replace_placeholders_in_text(text, placeholders, tree);
    }
}

fn process_raw_text_node(text: &str, tree: &mut SakuraTree, askama_nodes: &[AskamaNode]) {
    let processed = askama::replace_placeholder_in_raw_text(text, askama_nodes);
    tree.grow_leaf(Leaf::from_html_raw_text(&processed));
}

fn process_element_node(html_node: &HtmlNode, tree: &mut SakuraTree, askama_nodes: &[AskamaNode]) {
    let processed = html_node.clone().replace_placeholder(askama_nodes);
    tree.grow_leaf(Leaf::from_html(processed));
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
    text: &str,
    placeholders: BTreeMap<usize, (String, AskamaNode)>,
    tree: &mut SakuraTree,
) {
    let config = tree.config.clone();
    let mut last_end = 0;

    for (pos, (placeholder, askama_node)) in placeholders {
        // Add text before this placeholder
        if pos > last_end {
            let text_segment = &text[last_end..pos];
            tree.grow_leaf(Leaf::from_html_text(text_segment));
        }

        // Add the Askama node
        tree.grow_leaf(Leaf::from_askama(&config, askama_node.clone()));

        last_end = pos + placeholder.len();
    }

    // Add any remaining text after the last placeholder
    if last_end < text.len() {
        let remaining = &text[last_end..];
        tree.grow_leaf(Leaf::from_html_text(remaining));
    }
}
