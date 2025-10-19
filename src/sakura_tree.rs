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
pub enum Ring {
    // Compound
    Element {
        start_leaf: usize,
        inner: Vec<Ring>,
        end_leaf: usize,
    },
    ControlBlock {
        start_leaf: usize,
        inner: Vec<Ring>,
        end_leaf: usize,
    },
    EmptyBlock {
        start_leaf: usize,
        end_leaf: usize,
    },
    TextSequence {
        leaves: Vec<usize>,
    },
    MatchArm {
        leaf: usize,
        inner: Vec<Ring>,
    },

    // Atomic
    RawText(usize),
    Comment(usize),
    InlineText(usize),
    Other(usize),
}

#[derive(Debug, Clone)]
pub struct Branch {
    pub leaves: Vec<usize>,
    pub style: BranchStyle,
    pub indent: i32,
}

#[derive(Debug, Clone, PartialEq)]
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
                    .unwrap_or_else(|| (self.with_single_leaf(idx), idx + 1)),
                Root::Html(HtmlNode::StartTag { .. }) => self.handle_start_tag(idx, end_idx),
                Root::Html(HtmlNode::RawText(_)) => (Ring::RawText(idx), idx + 1),
                Root::Html(HtmlNode::Text(_)) | Root::Askama(_) => self
                    .try_text_sequence(idx, end_idx)
                    .unwrap_or_else(|| (self.with_single_leaf(idx), idx + 1)),
                Root::Html(_) => (self.with_single_leaf(idx), idx + 1),
            };

            rings.push(ring);
            idx = next_idx;
        }

        rings
    }

    fn with_single_leaf(&self, idx: usize) -> Ring {
        let leaf = &self.leaves[idx];

        match &leaf.root {
            Root::Html(HtmlNode::RawText(_)) => Ring::RawText(idx),
            Root::Html(HtmlNode::Comment(_)) => Ring::Comment(idx),
            Root::Askama(node) if node.is_comment() => Ring::Comment(idx),
            Root::Html(HtmlNode::Text(_) | HtmlNode::Entity(_)) => Ring::InlineText(idx),
            _ => Ring::Other(idx),
        }
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
        // Get the end_leaf for this element
        let &end_leaf = self.twigs.get(&start_leaf)?;

        // Recursively grow inner rings
        let inner = self.grow_rings(start_leaf + 1, end_leaf);

        let ring = Ring::Element {
            start_leaf,
            inner,
            end_leaf,
        };

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

                        let ring = if inner.is_empty() {
                            Ring::EmptyBlock {
                                start_leaf: start_idx,
                                end_leaf,
                            }
                        } else {
                            Ring::ControlBlock {
                                start_leaf: start_idx,
                                inner,
                                end_leaf,
                            }
                        };
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
        let inner = if curr_idx > start_idx + 1 {
            self.grow_rings(start_idx + 1, curr_idx)
        } else {
            Vec::new()
        };

        let ring = Ring::MatchArm {
            leaf: start_idx,
            inner,
        };
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
            let ring = Ring::TextSequence { leaves };
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
        let leaf_indices = self.all_leaf_indices();
        leaf_indices
            .iter()
            .filter_map(|&i| tree.leaves.get(i))
            .map(Leaf::chars_count)
            .sum()
    }

    // Check if any inner rings contain block-level structure
    pub fn has_block(&self, tree: &SakuraTree) -> bool {
        match self {
            Ring::Element { inner, .. } => {
                // Check if any inner ring is a block-level element
                inner.iter().any(|node| match node {
                    Ring::Element { start_leaf, .. } => {
                        let Root::Html(html_node) = &tree.leaves[*start_leaf].root else {
                            unreachable!()
                        };
                        !html_node.is_inline_level()
                    }
                    Ring::ControlBlock { .. } | Ring::RawText { .. } => true,
                    _ => node.has_block(tree),
                })
            }
            Ring::ControlBlock { inner, .. } => inner.iter().any(|node| node.has_block(tree)),
            Ring::MatchArm { inner, .. } => inner.iter().any(|node| node.has_block(tree)),
            Ring::RawText { .. } => true,
            _ => false,
        }
    }

    pub fn all_leaf_indices(&self) -> Vec<usize> {
        match self {
            Ring::Element {
                start_leaf,
                inner,
                end_leaf,
            } => once(*start_leaf)
                .chain(inner.iter().flat_map(Ring::all_leaf_indices))
                .chain((*end_leaf != *start_leaf).then_some(*end_leaf))
                .collect(),
            Ring::TextSequence { leaves } => leaves.clone(),
            Ring::ControlBlock {
                start_leaf,
                inner,
                end_leaf,
            } => once(*start_leaf)
                .chain(inner.iter().flat_map(Ring::all_leaf_indices))
                .chain(once(*end_leaf))
                .collect(),
            Ring::EmptyBlock {
                start_leaf,
                end_leaf,
            } => vec![*start_leaf, *end_leaf],
            Ring::MatchArm { leaf, inner } => once(*leaf)
                .chain(inner.iter().flat_map(Ring::all_leaf_indices))
                .collect(),
            Ring::RawText(leaf)
            | Ring::Comment(leaf)
            | Ring::InlineText(leaf)
            | Ring::Other(leaf) => vec![*leaf],
        }
    }
}
