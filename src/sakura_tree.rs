use std::collections::{BTreeMap, HashMap};
use std::iter::once;

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
    pub twigs: HashMap<usize, usize>,
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
pub struct Ring {
    pub layer: Layer,
    pub total_chars: usize,
    pub has_block: bool,
}

#[derive(Debug, Clone)]
pub enum Layer {
    // Complete HTML element with start/end tags
    CompleteElement {
        start_leaf: usize,
        inner_rings: Vec<Ring>,
        end_leaf: usize,
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
        start_leaf: usize,
        inner_rings: Vec<Ring>,
        end_leaf: usize,
    },
    // Empty Askama control block with open/close tag indices
    EmptyControlBlock {
        start_leaf: usize,
        end_leaf: usize,
    },
    // Askama when block with optional inline content
    MatchArm {
        leaf: usize,
        inner_rings: Vec<Ring>,
    },
    // Fallback leaf (self-closing elements, standalone text, etc.)
    Standalone {
        leaf: usize,
    },
}

#[derive(Debug, Clone)]
pub struct Branch {
    // Indices of leaves that belong to this branch
    pub leaves: Vec<usize>,
    // A formatting style hint for the entire branch
    pub style: BranchStyle,
    // Indentation level for this branch
    pub indent: i32,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BranchStyle {
    Inline,
    MultiLine,
    Wrapped,
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
    pub fn is_html_entity(&self) -> bool {
        matches!(self.root, Root::Html(HtmlNode::Entity(_)))
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
    pub fn grow(leaves: Vec<usize>, style: BranchStyle, indent: i32) -> Self {
        Self {
            leaves,
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

        // Convert html end_tag index to end_leaf index
        for (leaf_idx, leaf) in tree.leaves.iter().enumerate() {
            if let Root::Html(HtmlNode::StartTag {
                end_tag_idx: Some(elem_end_tag_idx),
                ..
            }) = &leaf.root
                && let Some(&end_leaf) = html_to_leaf_map.get(elem_end_tag_idx)
            {
                tree.twigs.insert(leaf_idx, end_leaf);
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
            let leaf = &self.leaves[idx];

            let (ring, next_idx) = match &leaf.root {
                Root::Askama(node) if node.is_when_block() => {
                    self.match_arm_with_content(idx, end_idx)
                }
                Root::Askama(node) if !node.is_expr() => self
                    .try_askama_block(idx, node, end_idx)
                    .unwrap_or_else(|| (Ring::new(Layer::Standalone { leaf: idx }, self), idx + 1)),
                Root::Html(HtmlNode::StartTag { .. }) => self.handle_start_tag(idx, end_idx),
                Root::Html(HtmlNode::Void { .. } | HtmlNode::SelfClosingTag { .. }) => {
                    (Ring::new(Layer::Standalone { leaf: idx }, self), idx + 1)
                }
                Root::Html(HtmlNode::RawText(_)) => {
                    (Ring::new(Layer::ScriptStyle { leaf: idx }, self), idx + 1)
                }
                Root::Html(HtmlNode::Text(_)) | Root::Askama(_) => self
                    .try_text_sequence(idx, end_idx)
                    .unwrap_or_else(|| (Ring::new(Layer::Standalone { leaf: idx }, self), idx + 1)),
                _ => (Ring::new(Layer::Standalone { leaf: idx }, self), idx + 1),
            };

            rings.push(ring);
            idx = next_idx;
        }

        rings
    }

    fn handle_start_tag(&self, idx: usize, end_idx: usize) -> (Ring, usize) {
        let leaf = &self.leaves[idx];

        // Try text sequence for inline elements followed by text/expr
        let Root::Html(html_node) = &leaf.root else {
            unreachable!()
        };
        if html_node.is_inline_level()
            && let Some(&end_leaf) = self.twigs.get(&idx)
            && self
                .leaves
                .get(end_leaf + 1)
                .is_some_and(|next| match &next.root {
                    Root::Html(HtmlNode::Text(_)) => true,
                    Root::Askama(node) => node.is_expr(),
                    _ => false,
                })
            && let Some(result) = self.try_text_sequence(idx, end_idx)
        {
            return result;
        }

        // Try complete element, or fall back to standalone for malformed HTML
        self.try_complete_element(idx)
            .unwrap_or_else(|| (Ring::new(Layer::Standalone { leaf: idx }, self), idx + 1))
    }

    fn try_complete_element(&self, start_leaf: usize) -> Option<(Ring, usize)> {
        // Get the end_leaf for this element
        let &end_leaf = self.twigs.get(&start_leaf)?;

        // Recursively grow inner rings
        let inner_rings = self.grow_rings(start_leaf + 1, end_leaf);

        let layer = Layer::CompleteElement {
            start_leaf,
            inner_rings,
            end_leaf,
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
                        let end_leaf = curr_idx;

                        // Recursively grow inner rings for leaves between open and close indices
                        let inner_rings = self.grow_rings(start_idx + 1, end_leaf);

                        let layer = if inner_rings.is_empty() {
                            Layer::EmptyControlBlock {
                                start_leaf: start_idx,
                                end_leaf,
                            }
                        } else {
                            Layer::ControlBlock {
                                start_leaf: start_idx,
                                inner_rings,
                                end_leaf,
                            }
                        };
                        let ring = Ring::new(layer, self);
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

        // Find the end of the inline content
        while curr_idx < end_idx {
            let Some(leaf) = self.leaves.get(curr_idx) else {
                break;
            };

            // Stop at next control block or comment
            if matches!(&leaf.root, Root::Askama(node) if node.is_ctrl() || node.is_comment()) {
                break;
            }

            // For StartTags, skip to after the end tag
            if matches!(leaf.root, Root::Html(HtmlNode::StartTag { .. }))
                && let Some(&end_leaf) = self.twigs.get(&curr_idx)
            {
                curr_idx = end_leaf + 1;
                continue;
            }

            curr_idx += 1;
        }

        // Build rings for the content (if any)
        let inner_rings = if curr_idx > start_idx + 1 {
            self.grow_rings(start_idx + 1, curr_idx)
        } else {
            Vec::new()
        };

        let layer = Layer::MatchArm {
            leaf: start_idx,
            inner_rings,
        };
        let ring = Ring::new(layer, self);
        (ring, curr_idx)
    }

    fn try_text_sequence(&self, start_idx: usize, end_idx: usize) -> Option<(Ring, usize)> {
        let mut leaves = vec![start_idx];
        let mut curr_idx = start_idx + 1;

        // If starting with a StartTag, include all leaves up to its end tag
        if let Some(leaf) = self.leaves.get(start_idx)
            && matches!(leaf.root, Root::Html(HtmlNode::StartTag { .. }))
            && let Some(&end_leaf) = self.twigs.get(&start_idx)
        {
            for idx in (start_idx + 1)..=end_leaf {
                leaves.push(idx);
            }
            curr_idx = end_leaf + 1;
        }

        // Collect following inline content
        while curr_idx < end_idx {
            let leaf = &self.leaves[curr_idx];

            if matches!(leaf.root, Root::Html(HtmlNode::StartTag { .. }))
                && let Some(&end_leaf) = self.twigs.get(&curr_idx)
                && let Root::Html(html_node) = &leaf.root
                && html_node.is_inline_level()
            {
                for idx in curr_idx..=end_leaf {
                    leaves.push(idx);
                }
                curr_idx = end_leaf + 1;
                continue;
            }

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
    fn new(layer: Layer, tree: &SakuraTree) -> Self {
        let total_chars = Self::calculate_total_chars(&layer, tree);
        let has_block = Self::inner_has_block(&layer, tree);
        Self {
            layer,
            total_chars,
            has_block,
        }
    }

    fn calculate_total_chars(layer: &Layer, tree: &SakuraTree) -> usize {
        let leaf_indices = layer.all_leaf_indices();
        leaf_indices
            .iter()
            .filter_map(|&i| tree.leaves.get(i))
            .map(Leaf::chars_count)
            .sum()
    }

    // Check if any inner rings contain block-level structure
    fn inner_has_block(layer: &Layer, tree: &SakuraTree) -> bool {
        match layer {
            Layer::CompleteElement { inner_rings, .. }
            | Layer::ControlBlock { inner_rings, .. } => {
                inner_rings.iter().any(|ring| match &ring.layer {
                    // Check if inner element is block-level
                    Layer::CompleteElement { start_leaf, .. } => {
                        let Root::Html(html_node) = &tree.leaves[*start_leaf].root else {
                            unreachable!()
                        };
                        !html_node.is_inline_level()
                    }
                    // Control blocks and script/style are always block-level
                    Layer::ControlBlock { .. } | Layer::ScriptStyle { .. } => true,
                    // Recurse for other layers
                    _ => ring.has_block,
                })
            }
            Layer::MatchArm { inner_rings, .. } => inner_rings.iter().any(|ring| ring.has_block),
            Layer::ScriptStyle { .. } => true,
            _ => false,
        }
    }

    pub fn all_leaf_indices(&self) -> Vec<usize> {
        self.layer.all_leaf_indices()
    }
}

impl Layer {
    fn all_leaf_indices(&self) -> Vec<usize> {
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
                start_leaf,
                inner_rings,
                end_leaf,
                ..
            } => once(*start_leaf)
                .chain(inner_rings.iter().flat_map(Ring::all_leaf_indices))
                .chain(once(*end_leaf))
                .collect(),
            Self::EmptyControlBlock {
                start_leaf,
                end_leaf,
                ..
            } => vec![*start_leaf, *end_leaf],
            Self::MatchArm { leaf, inner_rings } => once(*leaf)
                .chain(inner_rings.iter().flat_map(Ring::all_leaf_indices))
                .collect(),
            Self::ScriptStyle { leaf } | Self::Standalone { leaf } => vec![*leaf],
        }
    }
}
