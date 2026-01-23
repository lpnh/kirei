use std::collections::{BTreeSet, HashMap, HashSet};

use super::{askama::AskamaNode, css::CssNode, html::HtmlNode};

use crate::sakura::leaf::{self, Leaf, Root};

pub struct Seed {
    pub askama: Vec<AskamaNode>,
    pub html: Vec<HtmlNode>,
    pub css: Vec<CssNode>,
    pub src: String,
}

impl Seed {
    pub fn grow_leaves(&self) -> Vec<Leaf> {
        let (askama_nodes, html_nodes, css_nodes) = (&self.askama, &self.html, &self.css);

        let (mut leaves, mut pruned) = Self::from_html(html_nodes, askama_nodes, &self.src);

        let (css_leaves, css_pruned) = Self::from_css(css_nodes, askama_nodes);

        leaves.extend(css_leaves);
        pruned.extend(css_pruned);

        leaves.extend(Self::from_askama(askama_nodes, &pruned));

        let mut leaves: Vec<Leaf> = leaves.into_iter().collect();

        for leaf in &mut leaves {
            leaf.ws_before = Self::source_has_ws(&self.src, leaf.start.wrapping_sub(1));
            leaf.ws_after = Self::source_has_ws(&self.src, leaf.end);
        }

        let start_idx: HashMap<usize, usize> = leaves
            .iter()
            .enumerate()
            .map(|(i, leaf)| (leaf.start, i))
            .collect();

        let pairs = askama_nodes
            .iter()
            .filter_map(|node| match node {
                AskamaNode::Control { range, end, .. } => Some((range.start, *end)),
                _ => None,
            })
            .chain(html_nodes.iter().filter_map(|node| match node {
                HtmlNode::Start { range, end, .. } => Some((range.start, *end)),
                _ => None,
            }))
            .chain(css_nodes.iter().filter_map(|node| match node {
                CssNode::RuleSet { range, end, .. } | CssNode::AtRule { range, end, .. } => {
                    Some((range.start, *end))
                }
                _ => None,
            }));

        for (start, end) in pairs {
            if let Some(&idx) = start_idx.get(&start) {
                leaves[idx].pair = end.and_then(|end| start_idx.get(&end)).copied();
            }
        }

        leaves
    }

    fn from_askama(askama_nodes: &[AskamaNode], pruned: &HashSet<usize>) -> BTreeSet<Leaf> {
        askama_nodes
            .iter()
            .enumerate()
            .filter(|(idx, _)| !pruned.contains(idx))
            .map(|(_, node)| leaf::Leaf::from_askama(node))
            .collect()
    }

    fn from_html(
        html: &[HtmlNode],
        askama: &[AskamaNode],
        src: &str,
    ) -> (BTreeSet<Leaf>, HashSet<usize>) {
        let mut leaves = BTreeSet::new();
        let mut pruned = HashSet::new();

        for node in html {
            match node {
                HtmlNode::Start { .. } | HtmlNode::Void { .. } | HtmlNode::SelfClosing { .. } => {
                    let Some(range) = node.range() else { continue };
                    match Self::find_embedded(range, askama) {
                        None => leaves.insert(leaf::Leaf::from_html(node)),
                        Some(embed) => {
                            pruned.extend(embed.iter().copied());
                            let root = Root::Tag {
                                indent: node.indent(),
                                is_phrasing: node.is_phrasing(),
                                is_ws_sensitive: node.is_ws_sensitive(),
                            };
                            let content = leaf::format_embed(
                                range,
                                src,
                                askama,
                                &embed,
                                leaf::normalize_embed,
                            );
                            leaves.insert(leaf::Leaf::grow(root, content, range.start, range.end))
                        }
                    };
                }
                HtmlNode::Text { text, .. } => {
                    let Some(range) = node.range() else { continue };
                    match Self::find_embedded(range, askama) {
                        None => {
                            leaves.insert(leaf::Leaf::from_text(text, range.start, range.end));
                        }
                        Some(embed) => {
                            let mut curr_pos = range.start;
                            for &idx in &embed {
                                let node = &askama[idx];
                                if node.start() > curr_pos {
                                    leaves.insert(leaf::Leaf::from_text(
                                        &src[curr_pos..node.start()],
                                        curr_pos,
                                        node.start(),
                                    ));
                                }
                                leaves.insert(leaf::Leaf::from_askama(node));
                                curr_pos = node.end();
                            }
                            if curr_pos < range.end {
                                leaves.insert(leaf::Leaf::from_text(
                                    &src[curr_pos..range.end],
                                    curr_pos,
                                    range.end,
                                ));
                            }
                        }
                    }
                }
                HtmlNode::Raw { .. } | HtmlNode::Comment { .. } => {
                    let Some(range) = node.range() else { continue };
                    match Self::find_embedded(range, askama) {
                        None => {
                            leaves.insert(leaf::Leaf::from_html(node));
                        }
                        Some(embed) => {
                            pruned.extend(embed.iter().copied());
                            let root = match node {
                                HtmlNode::Raw { .. } => Root::Script,
                                HtmlNode::Comment { .. } => Root::Comment,
                                _ => unreachable!(),
                            };
                            let content =
                                leaf::format_embed(range, src, askama, &embed, str::to_string);
                            leaves.insert(leaf::Leaf::grow(root, content, range.start, range.end));
                        }
                    }
                }
                _ => {
                    leaves.insert(leaf::Leaf::from_html(node));
                }
            }
        }

        (leaves, pruned)
    }

    fn from_css(
        css_nodes: &[CssNode],
        askama_nodes: &[AskamaNode],
    ) -> (BTreeSet<Leaf>, HashSet<usize>) {
        let mut leaves = BTreeSet::new();
        let mut pruned = HashSet::new();

        for node in css_nodes {
            if let Some(range) = node.range()
                && let Some(embed) = Self::find_embedded(range, askama_nodes)
            {
                if matches!(node, CssNode::Comment { .. }) {
                    continue;
                }
                pruned.extend(embed);
            }
            leaves.insert(leaf::Leaf::from_css(node));
        }

        (leaves, pruned)
    }

    fn source_has_ws(src: &str, pos: usize) -> bool {
        src.as_bytes().get(pos).is_some_and(u8::is_ascii_whitespace)
    }

    fn find_embedded(
        range: &std::ops::Range<usize>,
        askama_nodes: &[AskamaNode],
    ) -> Option<Vec<usize>> {
        let indices: Vec<usize> = askama_nodes
            .iter()
            .enumerate()
            .filter_map(|(i, n)| (n.start() >= range.start && n.end() <= range.end).then_some(i))
            .collect();

        if indices.is_empty() {
            None
        } else {
            Some(indices)
        }
    }
}
