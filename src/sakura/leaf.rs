use std::cmp::Ordering;

use crate::{
    config::Config,
    normalize_ws,
    parse::{
        askama::{self, AskamaNode, ControlTag},
        css::CssNode,
        html::HtmlNode,
    },
};

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Leaf {
    pub root: Root,
    pub content: String,
    pub ws_before: bool,
    pub ws_after: bool,
    pub start: usize,
    pub end: usize,
    pub pair: Option<usize>,
}

impl Ord for Leaf {
    fn cmp(&self, other: &Self) -> Ordering {
        self.start.cmp(&other.start)
    }
}

impl PartialOrd for Leaf {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Root {
    Control {
        tag: ControlTag,
    },
    Expr,
    Comment,

    Tag {
        indent: isize,
        is_phrasing: bool,
        is_ws_sensitive: bool,
    },

    Text,
    Entity,
    Script,

    CssBlock {
        indent: isize,
    },
    CssText,
}

impl Leaf {
    pub fn grow(root: Root, content: String, start: usize, end: usize) -> Self {
        Self {
            root,
            content,
            ws_before: false,
            ws_after: false,
            start,
            end,
            pair: None,
        }
    }

    pub fn from_askama(askama_node: &AskamaNode) -> Self {
        let content = askama::format_askama_node(askama_node);

        let root = match askama_node {
            AskamaNode::Control { tag, .. } => Root::Control { tag: *tag },
            AskamaNode::Expression { .. } => Root::Expr,
            AskamaNode::Comment { .. } => Root::Comment,
        };

        Self::grow(root, content, askama_node.start(), askama_node.end())
    }

    pub fn from_html(html_node: &HtmlNode) -> Self {
        let content = html_node.format();
        let start = html_node.start();
        let end = html_node.range().map_or(start, |r| r.end);
        let root = match html_node {
            HtmlNode::Start { .. }
            | HtmlNode::End { .. }
            | HtmlNode::Void { .. }
            | HtmlNode::SelfClosing { .. }
            | HtmlNode::Doctype { .. } => Root::Tag {
                indent: html_node.indent(),
                is_phrasing: html_node.is_phrasing(),
                is_ws_sensitive: html_node.is_ws_sensitive(),
            },
            HtmlNode::Text { .. } => Root::Text,
            HtmlNode::Entity { .. } => Root::Entity,
            HtmlNode::Raw { .. } => Root::Script,
            HtmlNode::Comment { .. } => Root::Comment,
        };

        Self::grow(root, content, start, end)
    }

    pub fn from_css(css_node: &CssNode) -> Self {
        let content = css_node.content();
        let start = css_node.start();
        let end = css_node.range().map_or(start, |r| r.end);
        let root = match css_node {
            CssNode::RuleSet { .. } | CssNode::AtRule { .. } => Root::CssBlock { indent: 1 },
            CssNode::End { .. } => Root::CssBlock { indent: -1 },
            CssNode::Declaration { .. } => Root::CssText,
            CssNode::Comment { .. } => Root::Comment,
        };

        Self::grow(root, content, start, end)
    }

    pub fn from_text(text: &str, start: usize, end: usize) -> Self {
        Self::grow(Root::Text, normalize_ws(text), start, end)
    }

    pub fn is_ctrl(&self) -> bool {
        matches!(self.root, Root::Control { .. })
    }

    pub fn is_block(&self) -> bool {
        matches!(
            self.root,
            Root::Tag {
                is_phrasing: false,
                ..
            } | Root::Control { .. }
        )
    }

    pub fn preserves_ws(&self) -> bool {
        self.can_be_inline()
            || matches!(
                self.root,
                Root::Tag {
                    is_ws_sensitive: true,
                    ..
                }
            )
    }

    pub fn can_be_inline(&self) -> bool {
        matches!(
            self.root,
            Root::Tag {
                is_phrasing: true,
                ..
            } | Root::Control { .. }
                | Root::Expr
                | Root::Text
                | Root::Entity
        )
    }
}

pub fn normalize_embed(fragment: &str) -> String {
    if let Some(rest) = fragment.strip_suffix('>') {
        format!("{}>", normalize_preserving_ends(rest).trim_end())
    } else {
        normalize_preserving_ends(fragment)
    }
}

fn normalize_preserving_ends(text: &str) -> String {
    let normalized = normalize_ws(text);
    match (
        text.starts_with(char::is_whitespace),
        text.ends_with(char::is_whitespace),
    ) {
        (true, true) => format!(" {} ", normalized),
        (true, false) => format!(" {}", normalized),
        (false, true) => format!("{} ", normalized),
        (false, false) => normalized,
    }
}

pub fn format_embed(
    range: &std::ops::Range<usize>,
    source: &str,
    askama_nodes: &[askama::AskamaNode],
    embed: &[usize],
    transform: fn(&str) -> String,
) -> String {
    let mut result = String::new();
    let mut pos = range.start;

    for &idx in embed {
        let node = &askama_nodes[idx];
        if node.start() > pos {
            result.push_str(&transform(&source[pos..node.start()]));
        }
        result.push_str(&askama::format_askama_node(node));
        pos = node.end();
    }

    if pos < range.end {
        result.push_str(&transform(&source[pos..range.end]));
    }

    result
}

pub fn fits(start: usize, end: usize, leaves: &[Leaf], indent_map: &[usize], cfg: &Config) -> bool {
    indent_map[start] * cfg.indent_size + width(start, end, leaves) <= cfg.max_width
}

fn width(start: usize, end: usize, leaves: &[Leaf]) -> usize {
    (start..=end)
        .filter_map(|i| leaves.get(i))
        .map(|l| l.content.chars().count())
        .sum()
}

#[derive(Debug, Clone)]
pub struct Leaflet {
    pub content: String,
    pub ws_before: bool,
    pub pair: Option<usize>,
}

pub fn grow_leaflets(branch_start: usize, branch_end: usize, leaves: &[Leaf]) -> Vec<Leaflet> {
    let leaf_slice = &leaves[branch_start..=branch_end];
    let mut leaflets = Vec::new();
    let mut pairs = Vec::new();

    for leaf in leaf_slice {
        pairs.push(leaflets.len());

        if leaf.root == Root::Text {
            for (i, content) in leaf.content.split_whitespace().enumerate() {
                leaflets.push(Leaflet {
                    content: content.to_string(),
                    ws_before: i > 0 || leaf.ws_before,
                    pair: None,
                });
            }
        } else {
            leaflets.push(Leaflet {
                content: leaf.content.clone(),
                ws_before: leaf.ws_before,
                pair: None,
            });
        }
    }

    for (i, leaf) in leaf_slice.iter().enumerate() {
        if let Some(leaf_pair) = leaf.pair.and_then(|p| p.checked_sub(branch_start))
            && let (Some(start), Some(end)) = (pairs.get(i), pairs.get(leaf_pair))
            && let Some(leaflet) = leaflets.get_mut(*start)
        {
            leaflet.pair = Some(*end);
        }

        if leaf.ws_after
            && let Some(next) = pairs.get(i + 1)
            && let Some(leaflet) = leaflets.get_mut(*next)
        {
            leaflet.ws_before = true;
        }
    }

    leaflets
}
