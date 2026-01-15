use std::{
    cmp::Ordering,
    collections::{BTreeSet, HashMap, HashSet},
};
use tree_sitter::{Parser, Range};
use tree_sitter_askama::LANGUAGE as ASKAMA_LANGUAGE;
use tree_sitter_css::LANGUAGE as CSS_LANGUAGE;
use tree_sitter_html::LANGUAGE as HTML_LANGUAGE;

use crate::{
    ErrorKind,
    askama::{self, AskamaNode, ControlTag},
    check,
    css::{self, CssNode},
    html::{self, HtmlNode},
    session::Session,
};

pub struct SakuraParser {
    askama: Parser,
    html: Parser,
    css: Parser,
}

impl Default for SakuraParser {
    fn default() -> Self {
        let mut askama = Parser::new();
        askama
            .set_language(&ASKAMA_LANGUAGE.into())
            .expect("failed to set Askama language");

        let mut html = Parser::new();
        html.set_language(&HTML_LANGUAGE.into())
            .expect("failed to set HTML language");

        let mut css = Parser::new();
        css.set_language(&CSS_LANGUAGE.into())
            .expect("failed to set CSS language");

        Self { askama, html, css }
    }
}

impl SakuraParser {
    pub fn parse(&mut self, sess: &mut Session, src: &str, path: &str) -> Option<Seed> {
        let askama_tree = parse_tree(&mut self.askama, src, "Askama", &[], sess, path)?;
        let (askama, content) = askama::extract_askama(&askama_tree.root_node(), src);
        if content.is_empty() {
            return Some(Self::new_seed(
                askama,
                Vec::new(),
                Vec::new(),
                src.to_string(),
            ));
        }

        let html_tree = parse_tree(&mut self.html, src, "HTML", &content, sess, path)?;
        let (mut html, raw) = html::extract_html(sess, &html_tree.root_node(), src, &content, path);
        let crossing_indices = check::element_across_control(sess, &html, &askama, src, path);
        html::unpair_crossing_tags(&mut html, &crossing_indices);
        if raw.is_empty() {
            return Some(Self::new_seed(askama, html, Vec::new(), src.to_string()));
        }

        let css_ranges = check::exclude_askama_from_ranges(&raw, &askama);
        let css_tree = parse_tree(&mut self.css, src, "CSS", &css_ranges, sess, path)?;
        let css = css::extract_css(sess, &css_tree.root_node(), src, &css_ranges, path);
        Some(Self::new_seed(askama, html, css, src.to_string()))
    }

    fn new_seed(
        askama: Vec<AskamaNode>,
        html: Vec<HtmlNode>,
        css: Vec<CssNode>,
        src: String,
    ) -> Seed {
        Seed {
            askama,
            html,
            css,
            src,
        }
    }
}

fn parse_tree(
    parser: &mut Parser,
    src: &str,
    lang: &str,
    ranges: &[Range],
    sess: &mut Session,
    path: &str,
) -> Option<tree_sitter::Tree> {
    if parser.set_included_ranges(ranges).is_err() {
        sess.emit_error(&ErrorKind::ParserFailed { lang: lang.into() });
        return None;
    }

    let tree = parser.parse(src, None)?;

    if tree.root_node().has_error() {
        if let Some(err) = check::syntax_error(&tree.root_node(), lang.into(), src, path) {
            sess.emit_error(&err);
        }
        return None;
    }

    Some(tree)
}

pub struct Seed {
    askama: Vec<AskamaNode>,
    html: Vec<HtmlNode>,
    css: Vec<CssNode>,
    src: String,
}

impl Seed {
    pub fn grow_leaves(&self) -> Vec<Leaf> {
        let (askama_nodes, html_nodes, css_nodes) = (&self.askama, &self.html, &self.css);

        let (mut leaves, mut pruned) = Self::from_html(html_nodes, askama_nodes, &self.src);

        let (css_leaves, css_pruned) = Self::from_css(css_nodes, askama_nodes, &self.src);

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
            .map(|(_, node)| Leaf::from_askama(node))
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
                        None => leaves.insert(Leaf::from_html(node)),
                        Some(embed) => {
                            pruned.extend(embed.iter().copied());
                            let root = Root::Tag {
                                indent: node.indent(),
                                is_phrasing: node.is_phrasing(),
                                is_ws_sensitive: node.is_ws_sensitive(),
                            };
                            let content = format_embed(range, src, askama, &embed, normalize_embed);
                            leaves.insert(Leaf::grow(root, content, range.start, range.end))
                        }
                    };
                }
                HtmlNode::Text { text, .. } => {
                    let Some(range) = node.range() else { continue };
                    match Self::find_embedded(range, askama) {
                        None => {
                            leaves.insert(Leaf::from_text(text, range.start, range.end));
                        }
                        Some(embed) => {
                            let mut curr_pos = range.start;
                            for &idx in &embed {
                                let node = &askama[idx];
                                if node.start() > curr_pos {
                                    leaves.insert(Leaf::from_text(
                                        &src[curr_pos..node.start()],
                                        curr_pos,
                                        node.start(),
                                    ));
                                }
                                leaves.insert(Leaf::from_askama(node));
                                curr_pos = node.end();
                            }
                            if curr_pos < range.end {
                                leaves.insert(Leaf::from_text(
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
                            leaves.insert(Leaf::from_html(node));
                        }
                        Some(embed) => {
                            pruned.extend(embed.iter().copied());
                            let root = match node {
                                HtmlNode::Raw { .. } => Root::Script,
                                HtmlNode::Comment { .. } => Root::Comment,
                                _ => unreachable!(),
                            };
                            let content = format_embed(range, src, askama, &embed, str::to_string);
                            leaves.insert(Leaf::grow(root, content, range.start, range.end));
                        }
                    }
                }
                _ => {
                    leaves.insert(Leaf::from_html(node));
                }
            }
        }

        (leaves, pruned)
    }

    fn from_css(
        css_nodes: &[CssNode],
        askama_nodes: &[AskamaNode],
        source: &str,
    ) -> (BTreeSet<Leaf>, HashSet<usize>) {
        let mut leaves = BTreeSet::new();
        let mut pruned = HashSet::new();

        for node in css_nodes {
            match node {
                CssNode::End(_) => {
                    leaves.insert(Leaf::from_css(node));
                }
                CssNode::RuleSet { range, .. }
                | CssNode::AtRule { range, .. }
                | CssNode::Comment { range, .. }
                | CssNode::Declaration { range, .. } => {
                    if let Some(embed) = Self::find_embedded(range, askama_nodes) {
                        let (ctrls, exprs): (Vec<usize>, Vec<usize>) = embed
                            .iter()
                            .partition(|&i| matches!(askama_nodes[*i], AskamaNode::Control { .. }));

                        pruned.extend(exprs);

                        let mut ctrl_leaves = Vec::new();
                        let mut curr_pos = range.start;

                        for idx in ctrls {
                            let ctrl = &askama_nodes[idx];
                            if ctrl.start() > curr_pos {
                                let content = source[curr_pos..ctrl.start()].trim().to_string();
                                ctrl_leaves.push(Leaf::grow(
                                    Root::Opaque,
                                    content,
                                    curr_pos,
                                    ctrl.start(),
                                ));
                            }
                            curr_pos = ctrl.end();
                        }

                        if curr_pos < range.end {
                            let content = source[curr_pos..range.end].trim().to_string();
                            ctrl_leaves.push(Leaf::grow(
                                Root::Opaque,
                                content,
                                curr_pos,
                                range.end,
                            ));
                        }

                        leaves.extend(ctrl_leaves);
                    } else {
                        leaves.insert(Leaf::from_css(node));
                    }
                }
            }
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

    Opaque,
}

impl Leaf {
    fn grow(root: Root, content: String, start: usize, end: usize) -> Self {
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

    fn from_askama(askama_node: &AskamaNode) -> Self {
        let content = askama::format_askama_node(askama_node);

        let root = match askama_node {
            AskamaNode::Control { tag, .. } => Root::Control { tag: *tag },
            AskamaNode::Expression { .. } => Root::Expr,
            AskamaNode::Comment { .. } => Root::Comment,
        };

        Self::grow(root, content, askama_node.start(), askama_node.end())
    }

    fn from_html(html_node: &HtmlNode) -> Self {
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

    fn from_css(css_node: &CssNode) -> Self {
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

    fn from_text(text: &str, start: usize, end: usize) -> Self {
        Self::grow(Root::Text, crate::normalize_ws(text), start, end)
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

fn normalize_embed(fragment: &str) -> String {
    if let Some(rest) = fragment.strip_suffix('>') {
        format!("{}>", normalize_preserving_ends(rest).trim_end())
    } else {
        normalize_preserving_ends(fragment)
    }
}

fn normalize_preserving_ends(text: &str) -> String {
    let normalized = crate::normalize_ws(text);
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

fn format_embed(
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
