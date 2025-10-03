use crate::{
    askama::{self, AskamaNode},
    config::Config,
    html::HtmlNode,
};

#[derive(Debug, Clone)]
pub(crate) struct SakuraTree {
    pub(crate) config: Config,
    pub(crate) leaves: Vec<SakuraLeaf>,
    pub(crate) trunk_rings: Vec<TrunkRing>,
    pub(crate) branches: Vec<SakuraBranch>,
}

#[derive(Debug, Clone)]
pub(crate) struct LeafElementMetadata {
    pub(crate) chars_count: usize,
    pub(crate) end_leaf_index: usize,
}

#[derive(Debug, Clone)]
pub(crate) struct SakuraLeaf {
    // The source of this leaf (Askama or HTML)
    pub(crate) source: NodeSource,
    // The rendered content for this leaf
    pub(crate) content: String,
    // Element metadata (only for StartTag nodes with matching end tags)
    pub(crate) element_metadata: Option<LeafElementMetadata>,
}

#[derive(Debug, Clone)]
pub(crate) struct TrunkRing {
    pub(crate) shape: TrunkShape,
    pub(crate) total_chars: usize,
}

#[derive(Debug, Clone)]
pub(crate) enum TrunkShape {
    // Complete HTML element with start/end tags
    CompleteElement {
        chars_count: usize,
        start_leaf: usize,
        children: Vec<TrunkRing>,
        end_leaf: usize,
        is_semantic_inline: bool,
    },
    // Sequence of text content and expressions
    TextSequence {
        leaves: Vec<usize>,
    },
    // Raw text (inside style/script tags)
    ScriptStyle {
        leaves: Vec<usize>,
    },
    // Askama control block with matched open/close tags
    ControlBlock {
        open_leaf: usize,
        children: Vec<TrunkRing>,
        close_leaf: usize,
    },
    // Empty Askama control block (open/close and no content)
    EmptyAskamaBlock {
        open_leaf: usize,
        close_leaf: usize,
    },
    // Fallback leaf (self-closing elements, standalone text, etc.)
    Standalone {
        leaf: usize,
    },
}

#[derive(Debug, Clone)]
pub(crate) struct SakuraBranch {
    // Indices of leaves that belong to this branch
    pub(crate) leaf_indices: Vec<usize>,
    // A formatting style hint for the entire branch
    pub(crate) style: FormatStyle,
    // Indentation level for this branch
    pub(crate) indent_level: i32,
}

#[derive(Debug, Clone)]
pub(crate) enum NodeSource {
    Askama(AskamaNode),
    Html(HtmlNode),
}

#[derive(Default, Debug, Clone, PartialEq)]
pub(crate) enum FormatStyle {
    #[default]
    Inline,
    MultiLine,
    WrappedText,
    RawText,
}

impl SakuraLeaf {
    pub(crate) fn from_askama(config: &Config, askama_node: AskamaNode) -> Self {
        let content = askama::format_askama_node(config, &askama_node);
        Self {
            content,
            source: NodeSource::Askama(askama_node),
            element_metadata: None,
        }
    }

    pub(crate) fn from_html(html_node: HtmlNode) -> Self {
        let content = html_node.to_string();
        Self {
            content,
            source: NodeSource::Html(html_node),
            element_metadata: None,
        }
    }

    // Create a new unprocessed leaf from HTML text content
    pub(crate) fn from_html_text(text: &str) -> Self {
        let html_node = HtmlNode::Text(text.to_string());

        // Normalize HTML text content
        let normalized_content = askama::normalize_whitespace(text);
        Self {
            content: normalized_content,
            source: NodeSource::Html(html_node),
            element_metadata: None,
        }
    }

    // Constructor for raw text content
    pub(crate) fn from_html_raw_text(text: &str) -> Self {
        let html_node = HtmlNode::RawText(text.to_string());

        // Preserve original line structure for raw text - only normalize within lines
        let normalized_content = Self::normalize_raw_text_preserving_lines(text);
        Self {
            content: normalized_content,
            source: NodeSource::Html(html_node),
            element_metadata: None,
        }
    }

    pub(crate) fn from_html_text_preserving_edges(text: &str) -> Self {
        let html_node = HtmlNode::Text(text.to_string());
        Self {
            content: text.to_string(),
            source: NodeSource::Html(html_node),
            element_metadata: None,
        }
    }

    pub(crate) fn from_template_text(text: &str) -> Self {
        let html_node = HtmlNode::Text(text.to_string());
        Self {
            content: text.to_string(),
            source: NodeSource::Html(html_node),
            element_metadata: None,
        }
    }

    pub(crate) fn from_text_preserving_leading_only(text: &str) -> Self {
        let html_node = HtmlNode::Text(text.to_string());
        Self {
            content: text.to_string(),
            source: NodeSource::Html(html_node),
            element_metadata: None,
        }
    }

    // Normalize raw text content while preserving line structure
    // Only normalizes whitespace within each line, preserves line breaks
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

    pub(crate) fn is_askama(&self) -> bool {
        matches!(self.source, NodeSource::Askama(_))
    }
    pub(crate) fn is_html_text(&self) -> bool {
        matches!(self.source, NodeSource::Html(HtmlNode::Text(_)))
    }
    pub(crate) fn is_html_raw_text(&self) -> bool {
        matches!(self.source, NodeSource::Html(HtmlNode::RawText(_)))
    }
    pub(crate) fn is_html_entity(&self) -> bool {
        let is_it = matches!(self.source, NodeSource::Html(HtmlNode::Entity(_)));
        is_it
    }
    pub(crate) fn chars_count(&self) -> usize {
        self.content.chars().count()
    }
    pub(crate) fn maybe_askama_node(&self) -> Option<&AskamaNode> {
        match &self.source {
            NodeSource::Askama(node) => Some(node),
            NodeSource::Html(_) => None,
        }
    }
}

impl SakuraBranch {
    pub(crate) fn new(leaf_indices: Vec<usize>, style: FormatStyle, indent_level: i32) -> Self {
        Self {
            leaf_indices,
            style,
            indent_level,
        }
    }
}

use std::collections::HashMap;

impl SakuraTree {
    pub(crate) fn grow(
        askama_nodes: &[AskamaNode],
        html_nodes: &[HtmlNode],
        config: Config,
    ) -> Self {
        let mut tree = Self {
            config,
            leaves: Vec::new(),
            trunk_rings: Vec::new(),
            branches: Vec::new(),
        };

        // Build index map to translate from html_nodes to leaf indices
        let mut html_to_leaf_map: HashMap<usize, usize> = HashMap::new();

        // Convert each HtmlNode to SakuraLeaf and replace placeholder
        for (i, html_node) in html_nodes.iter().enumerate() {
            // Record the mapping before processing
            let leaf_index = tree.leaf_count();
            html_to_leaf_map.insert(i, leaf_index);

            // Dispatch based on node type
            process_html_node(html_node, &mut tree, askama_nodes);
        }

        // Convert metadata from html indices to leaf indices
        for leaf in &mut tree.leaves {
            if let NodeSource::Html(HtmlNode::StartTag {
                element_metadata: Some(html_metadata),
                ..
            }) = &leaf.source
            {
                // Convert end_tag_html_index to end_leaf_index
                if let Some(&end_leaf_index) =
                    html_to_leaf_map.get(&html_metadata.end_tag_html_index)
                {
                    leaf.element_metadata = Some(LeafElementMetadata {
                        chars_count: html_metadata.chars_count,
                        end_leaf_index,
                    });
                }
            }
        }

        tree.grow_trunk();

        tree
    }

    pub(crate) fn push_leaf(&mut self, leaf: SakuraLeaf) {
        self.leaves.push(leaf);
    }
    pub(crate) fn new_layer(&mut self, ring: TrunkRing) {
        self.trunk_rings.push(ring);
    }
    pub(crate) fn push_branch(&mut self, branch: SakuraBranch) {
        self.branches.push(branch);
    }
    pub(crate) fn leaf_count(&self) -> usize {
        self.leaves.len()
    }
    pub(crate) fn get_leaf(&self, index: usize) -> Option<&SakuraLeaf> {
        self.leaves.get(index)
    }
    pub(crate) fn iter_leaves(&self) -> std::slice::Iter<'_, SakuraLeaf> {
        self.leaves.iter()
    }
    pub(crate) fn trunk(&self) -> std::slice::Iter<'_, TrunkRing> {
        self.trunk_rings.iter()
    }
    pub(crate) fn iter_branches(&self) -> std::slice::Iter<'_, SakuraBranch> {
        self.branches.iter()
    }

    // Check if a leaf index is within a script or style element context
    pub(crate) fn is_script_or_style_context(&self, index: usize) -> bool {
        // First check if this leaf itself is raw text
        if let Some(leaf) = self.get_leaf(index)
            && leaf.is_html_raw_text()
        {
            return true;
        }

        // Look for <style> or <script> start tag before this index
        for i in (0..index).rev() {
            if let Some(leaf) = self.get_leaf(i)
                && let NodeSource::Html(html_node) = &leaf.source
            {
                if html_node.is_opening_tag() && html_node.is_style_or_script_element() {
                    return true;
                }

                if html_node.is_closing_tag() && html_node.is_style_or_script_element() {
                    return false;
                }
            }
        }

        false
    }

    // Grow trunk
    fn grow_trunk(&mut self) {
        let trunk = self.grow_rings(0, self.leaf_count());

        for ring in trunk {
            self.new_layer(ring);
        }
    }

    // Grow concentric rings
    fn grow_rings(&self, start_index: usize, end_index: usize) -> Vec<TrunkRing> {
        let mut trunk = Vec::new();
        let mut index = start_index;

        while index < end_index {
            if let Some(leaf) = self.get_leaf(index) {
                match &leaf.source {
                    // Try to build when clause (they need to claim their inline content)
                    NodeSource::Askama(askama_node) if !askama_node.is_expr() => {
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
                                trunk.push(ring);
                                index = next_index;
                                continue;
                            }
                        } else {
                            // Regular control block (if/for/match/etc)
                            if let Some((ring, next_index)) =
                                self.try_askama_block(index, askama_node, end_index)
                            {
                                trunk.push(ring);
                                index = next_index;
                                continue;
                            }
                        }
                    }
                    // Try to build complete HTML elements
                    NodeSource::Html(HtmlNode::StartTag { .. }) => {
                        if let Some((ring, next_index)) = self.try_complete_element(index) {
                            trunk.push(ring);
                            index = next_index;
                            continue;
                        }
                    }
                    // Standalone for void or self-closing elements
                    NodeSource::Html(HtmlNode::Void { .. } | HtmlNode::SelfClosingTag { .. }) => {
                        let shape = TrunkShape::Standalone { leaf: index };
                        let ring = TrunkRing::new(shape, self);
                        trunk.push(ring);
                        index += 1;
                        continue;
                    }
                    // Try to build text sequences
                    NodeSource::Html(HtmlNode::Text(_) | HtmlNode::RawText(_))
                    | NodeSource::Askama(_) => {
                        // First check if we're in script/style context (RawText)
                        if self.is_script_or_style_context(index)
                            && let Some((ring, next_index)) =
                                self.try_script_style(index, end_index)
                        {
                            trunk.push(ring);
                            index = next_index;
                            continue;
                        }

                        // Otherwise try regular text sequence
                        if let Some((ring, next_index)) = self.try_text_sequence(index, end_index) {
                            trunk.push(ring);
                            index = next_index;
                            continue;
                        }
                    }
                    NodeSource::Html(_) => {}
                }
            }

            // Fallback: Standalone
            let shape = TrunkShape::Standalone { leaf: index };
            let ring = TrunkRing::new(shape, self);
            trunk.push(ring);
            index += 1;
        }

        trunk
    }

    fn try_complete_element(&self, start_leaf_index: usize) -> Option<(TrunkRing, usize)> {
        // Get the leaf and extract metadata
        let leaf = self.get_leaf(start_leaf_index)?;
        let metadata = leaf.element_metadata.as_ref()?;

        let end_leaf_index = metadata.end_leaf_index;
        let chars_count = metadata.chars_count;

        // Recursively build child trunks
        let children = self.grow_rings(start_leaf_index + 1, end_leaf_index);

        // Get inline status from the start tag
        let is_semantic_inline = match &leaf.source {
            NodeSource::Html(html_node) => html_node.is_inline_level(),
            NodeSource::Askama(_) => false,
        };

        let shape = TrunkShape::CompleteElement {
            chars_count,
            start_leaf: start_leaf_index,
            children,
            end_leaf: end_leaf_index,
            is_semantic_inline,
        };
        let ring = TrunkRing::new(shape, self);

        // Next index for parent is after the end tag
        Some((ring, end_leaf_index + 1))
    }

    fn try_askama_block(
        &self,
        start_index: usize,
        askama_node: &AskamaNode,
        end_index: usize,
    ) -> Option<(TrunkRing, usize)> {
        if let Some(open_tag) = askama_node.get_ctrl_tag()
            && open_tag.boundary() == askama::Boundary::Open
        {
            // Look for matching close block, tracking nesting depth
            let mut current_index = start_index + 1;
            let mut depth = 0;

            while current_index < end_index {
                if let Some(leaf) = self.get_leaf(current_index)
                    && let NodeSource::Askama(node) = &leaf.source
                    && let Some(tag) = node.get_ctrl_tag()
                {
                    // Check if this is another opening of the same type (nested block)
                    if tag.boundary() == askama::Boundary::Open && open_tag.same_kind(tag) {
                        depth += 1;
                    }
                    // Check if this is a closing block
                    else if tag.boundary() == askama::Boundary::Close
                        && open_tag.matches_close(tag)
                    {
                        if depth == 0 {
                            // Found our matching close at depth 0
                            let close_leaf_index = current_index;

                            // Recursively build child trunks from content between open and close
                            let children = self.grow_rings(start_index + 1, close_leaf_index);

                            let shape = if children.is_empty() {
                                TrunkShape::EmptyAskamaBlock {
                                    open_leaf: start_index,
                                    close_leaf: close_leaf_index,
                                }
                            } else {
                                TrunkShape::ControlBlock {
                                    open_leaf: start_index,
                                    children,
                                    close_leaf: close_leaf_index,
                                }
                            };
                            let ring = TrunkRing::new(shape, self);
                            return Some((ring, close_leaf_index + 1));
                        }
                        // This closes a nested block, decrement depth
                        depth -= 1;
                    }
                }
                current_index += 1;
            }
        }

        None
    }

    // Find the end leaf index of a complete element
    fn find_complete_element_end(&self, start_leaf_index: usize) -> Option<usize> {
        self.get_leaf(start_leaf_index)?
            .element_metadata
            .as_ref()
            .map(|metadata| metadata.end_leaf_index)
    }

    // Build a when clause with all its inline content (including complete inline elements)
    // All or nothing: either all content fits (up to 2x max line length), or the when clause stays alone
    fn try_when_clause_with_inline_content(
        &self,
        start_index: usize,
        end_index: usize,
    ) -> Option<(TrunkRing, usize)> {
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
            if matches!(&leaf.source, NodeSource::Askama(node) if node.is_ctrl() || node.is_comment())
            {
                break;
            }

            // For StartTags, include the complete element without splitting it
            if matches!(leaf.source, NodeSource::Html(HtmlNode::StartTag { .. })) {
                // Try to find the complete element boundaries
                if let Some(end_leaf_index) = self.find_complete_element_end(current_index) {
                    // Include all leaves from current to end
                    for idx in current_index..=end_leaf_index {
                        candidate_leaves.push(idx);
                    }
                    current_index = end_leaf_index + 1;
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
            .map(SakuraLeaf::chars_count)
            .sum();

        // 3. All or nothing
        let final_leaves = if total_chars > permissive_limit {
            vec![start_index]
        } else {
            candidate_leaves
        };

        if final_leaves.len() > 1 {
            let shape = TrunkShape::TextSequence {
                leaves: final_leaves,
            };
            let ring = TrunkRing::new(shape, self);
            return Some((ring, current_index));
        }

        None
    }

    // Build a raw text sequence (for style/script content) using Wire's logic
    fn try_script_style(&self, start_index: usize, end_index: usize) -> Option<(TrunkRing, usize)> {
        let mut leaves = vec![start_index];

        // Look ahead for consecutive raw text content (including Askama expressions within)
        for i in (start_index + 1)..end_index {
            if let Some(leaf) = self.get_leaf(i) {
                let is_raw_text = self.is_script_or_style_context(i);
                let is_text_or_expr = match &leaf.source {
                    NodeSource::Html(html_node) => html_node.is_text_like(),
                    NodeSource::Askama(askama_node) => askama_node.is_expr(),
                };

                // Include both text content and Askama expressions in raw text context
                if is_raw_text && is_text_or_expr {
                    leaves.push(i);
                } else {
                    break;
                }
            } else {
                break;
            }
        }

        // Return ScriptStyle for single leaf
        if !leaves.is_empty() {
            let next_index = leaves.last().copied().unwrap_or(start_index) + 1;
            let shape = TrunkShape::ScriptStyle { leaves };
            let ring = TrunkRing::new(shape, self);
            return Some((ring, next_index));
        }

        None
    }

    fn try_text_sequence(
        &self,
        start_index: usize,
        end_index: usize,
    ) -> Option<(TrunkRing, usize)> {
        let mut leaves = vec![start_index];

        // Collect text content and expressions that can be grouped
        let mut current_index = start_index + 1;
        while current_index < end_index {
            if let Some(leaf) = self.get_leaf(current_index) {
                let can_include = match &leaf.source {
                    NodeSource::Html(HtmlNode::Text(_)) => !leaf.content.trim().is_empty(),
                    NodeSource::Html(HtmlNode::Entity(_)) => true,
                    NodeSource::Askama(node) => {
                        // Include expressions always
                        if node.is_expr() {
                            true
                        } else if node.is_when_clause() {
                            // Never include when clauses in text sequences
                            false
                        } else {
                            // Stop at other control blocks
                            false
                        }
                    }
                    NodeSource::Html(html_node) => html_node.is_inline_level(),
                };

                if can_include {
                    leaves.push(current_index);
                    current_index += 1;
                } else {
                    break;
                }
            } else {
                break;
            }
        }

        // Only create text sequence if we have multiple leaves or significant content
        if leaves.len() > 1 {
            let shape = TrunkShape::TextSequence { leaves };
            let ring = TrunkRing::new(shape, self);
            return Some((ring, current_index));
        }

        None
    }
}

impl TrunkRing {
    pub(crate) fn new(shape: TrunkShape, tree: &SakuraTree) -> Self {
        let total_chars = match &shape {
            TrunkShape::CompleteElement { chars_count, .. } => *chars_count,
            _ => Self::calculate_total_chars(&shape, tree),
        };
        Self { shape, total_chars }
    }

    fn calculate_total_chars(shape: &TrunkShape, tree: &SakuraTree) -> usize {
        let leaf_indices = shape.all_leaf_indices();
        leaf_indices
            .iter()
            .filter_map(|&i| tree.get_leaf(i))
            .map(SakuraLeaf::chars_count)
            .sum()
    }

    pub(crate) fn all_leaf_indices(&self) -> Vec<usize> {
        self.shape.all_leaf_indices()
    }

    pub(crate) fn contains_block_children(&self) -> bool {
        match &self.shape {
            TrunkShape::CompleteElement { children, .. }
            | TrunkShape::ControlBlock { children, .. } => {
                children.iter().any(|child| {
                    // Check if this child itself is a block-level element
                    if let TrunkShape::CompleteElement {
                        is_semantic_inline, ..
                    } = &child.shape
                        && !is_semantic_inline
                    {
                        return true;
                    }
                    // Recursively check child's children
                    child.contains_block_children()
                })
            }
            _ => false,
        }
    }

    pub(crate) fn has_control_blocks(&self) -> bool {
        match &self.shape {
            TrunkShape::CompleteElement { children, .. }
            | TrunkShape::ControlBlock { children, .. } => children.iter().any(|child| {
                matches!(&child.shape, TrunkShape::ControlBlock { .. })
                    || child.has_control_blocks()
            }),
            _ => false,
        }
    }
}

impl TrunkShape {
    pub(crate) fn all_leaf_indices(&self) -> Vec<usize> {
        match self {
            TrunkShape::CompleteElement {
                start_leaf,
                children,
                end_leaf,
                ..
            } => {
                let mut indices = vec![*start_leaf];
                for child in children {
                    indices.extend(child.all_leaf_indices());
                }
                // Avoid duplicates for void elements
                if end_leaf != start_leaf {
                    indices.push(*end_leaf);
                }
                indices
            }
            TrunkShape::TextSequence { leaves } | TrunkShape::ScriptStyle { leaves } => {
                leaves.clone()
            }
            TrunkShape::ControlBlock {
                open_leaf,
                children,
                close_leaf,
                ..
            } => {
                let mut indices = vec![*open_leaf];
                for child in children {
                    indices.extend(child.all_leaf_indices());
                }
                indices.push(*close_leaf);
                indices
            }
            TrunkShape::EmptyAskamaBlock {
                open_leaf,
                close_leaf,
                ..
            } => {
                vec![*open_leaf, *close_leaf]
            }
            TrunkShape::Standalone { leaf } => {
                vec![*leaf]
            }
        }
    }
}

// Html processing
use std::collections::BTreeMap;

fn process_html_node(html_node: &HtmlNode, tree: &mut SakuraTree, askama_nodes: &[AskamaNode]) {
    match html_node {
        HtmlNode::Text(text) => process_text_node(text, tree, askama_nodes),
        HtmlNode::RawText(text) => process_raw_text_node(text, tree, askama_nodes),
        HtmlNode::Entity(_) | HtmlNode::Comment(_) | HtmlNode::Doctype(_) => {
            // Simple nodes - direct conversion
            tree.push_leaf(SakuraLeaf::from_html(html_node.clone()));
        }
        _ => process_element_node(html_node, tree, askama_nodes),
    }
}

fn process_text_node(text: &str, tree: &mut SakuraTree, askama_nodes: &[AskamaNode]) {
    let placeholders = find_placeholders(text, askama_nodes);

    if placeholders.is_empty() {
        tree.push_leaf(SakuraLeaf::from_html_text(text));
    } else {
        replace_placeholders_in_text(text, placeholders, tree);
    }
}

fn process_raw_text_node(text: &str, tree: &mut SakuraTree, askama_nodes: &[AskamaNode]) {
    let processed = askama::replace_placeholder_in_raw_text(text, askama_nodes);
    tree.push_leaf(SakuraLeaf::from_html_raw_text(&processed));
}

fn process_element_node(html_node: &HtmlNode, tree: &mut SakuraTree, askama_nodes: &[AskamaNode]) {
    if should_add_spacing_after_when(tree) {
        tree.push_leaf(SakuraLeaf::from_html_text_preserving_edges(" "));
    }

    let processed = html_node.clone().replace_placeholder(askama_nodes);
    tree.push_leaf(SakuraLeaf::from_html(processed));
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
    let mut prev_was_expr_or_when = false;

    for (pos, (placeholder, askama_node)) in placeholders {
        // Add text before this placeholder
        if pos > last_end {
            let text_segment = &text[last_end..pos];
            let next_is_expr = askama_node.is_expr();
            let leaf = create_text_leaf(text_segment, prev_was_expr_or_when, next_is_expr);
            tree.push_leaf(leaf);
        }

        // Add the Askama node
        tree.push_leaf(SakuraLeaf::from_askama(&config, askama_node.clone()));

        // Update flag for next segment
        prev_was_expr_or_when = askama_node.is_expr() || askama_node.is_when_clause();
        last_end = pos + placeholder.len();
    }

    // Add any remaining text after the last placeholder
    if last_end < text.len() {
        let remaining = &text[last_end..];
        let leaf = create_text_leaf(remaining, prev_was_expr_or_when, false);
        tree.push_leaf(leaf);
    }
}

// Create appropriate text leaf based on context
fn create_text_leaf(text: &str, after_expr_or_when: bool, before_expr: bool) -> SakuraLeaf {
    match (after_expr_or_when, before_expr) {
        // Between expressions or after when before expression: preserve both edges
        (true, true) => SakuraLeaf::from_html_text_preserving_edges(text),
        // After expression/when but not before expression: preserve leading only
        (true, false) => SakuraLeaf::from_text_preserving_leading_only(text),
        // Default
        _ => SakuraLeaf::from_template_text(text),
    }
}

// Check if spacing should be added after a when clause
fn should_add_spacing_after_when(tree: &SakuraTree) -> bool {
    tree.leaves
        .last()
        .and_then(|leaf| {
            if leaf.is_askama() {
                leaf.maybe_askama_node()
            } else {
                None
            }
        })
        .is_some_and(AskamaNode::is_when_clause)
}
