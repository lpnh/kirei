use miette::NamedSource;
use std::{
    borrow::Cow,
    cmp::Ordering,
    collections::{BTreeSet, HashMap, HashSet},
};
use tree_sitter::{Node, Parser, Point, Range};
use tree_sitter_askama::LANGUAGE as ASKAMA_LANGUAGE;
use tree_sitter_html::LANGUAGE as HTML_LANGUAGE;

use crate::{
    ErrorKind, KireiWarning, askama,
    askama::{AskamaNode, ControlTag},
    html::{self, HtmlNode},
    range_to_span,
    session::Notes,
};

pub struct SakuraParser {
    askama: Parser,
    html: Parser,
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

        Self { askama, html }
    }
}

impl SakuraParser {
    pub fn parse<'a>(
        &mut self,
        notes: &mut Notes,
        source: &'a str,
        filepath: &str,
    ) -> Option<SakuraSeed<'a>> {
        let ast_tree = self.askama.parse(source, None).or_else(|| {
            notes.errors.push(ErrorKind::ParserFailed {
                lang: "Askama".to_string(),
            });
            None
        })?;

        if ast_tree.root_node().has_error() {
            if let Some(err) = syntax_error(
                &ast_tree.root_node(),
                "Askama".to_string(),
                source,
                filepath,
            ) {
                notes.errors.push(err);
            }
            return None;
        }

        let (askama, content_node_ranges) =
            askama::extract_askama_nodes(&ast_tree.root_node(), source);

        if !content_node_ranges.is_empty()
            && self.html.set_included_ranges(&content_node_ranges).is_err()
        {
            notes.errors.push(ErrorKind::ParserFailed {
                lang: "HTML".to_string(),
            });
            return None;
        }

        let html_tree = self.html.parse(source, None).or_else(|| {
            notes.errors.push(ErrorKind::ParserFailed {
                lang: "HTML".to_string(),
            });
            None
        })?;

        if html_tree.root_node().has_error() {
            if let Some(err) =
                syntax_error(&html_tree.root_node(), "HTML".to_string(), source, filepath)
            {
                notes.errors.push(err);
            }
            return None;
        }

        html::extract_html_nodes(
            notes,
            &html_tree.root_node(),
            source,
            &content_node_ranges,
            filepath,
        )
        .map(|mut html| {
            let crossing_indices = element_across_control(notes, &html, &askama, source, filepath);
            html::unpair_crossing_tags(&mut html, &crossing_indices);

            SakuraSeed {
                askama,
                html,
                source,
            }
        })
    }
}

pub struct SakuraSeed<'a> {
    askama: Vec<AskamaNode<'a>>,
    html: Vec<HtmlNode<'a>>,
    source: &'a str,
}

impl<'a> SakuraSeed<'a> {
    pub fn grow_leaves(&self) -> Vec<Leaf<'a>> {
        let (askama_nodes, html_nodes) = (&self.askama, &self.html);

        let (mut leaves, pruned) = Self::from_html(html_nodes, askama_nodes, self.source);
        leaves.extend(Self::from_askama(askama_nodes, &pruned));

        let mut leaves: Vec<Leaf> = leaves.into_iter().collect();

        for leaf in &mut leaves {
            leaf.ws_before = Self::source_has_ws(self.source, leaf.start.wrapping_sub(1));
            leaf.ws_after = Self::source_has_ws(self.source, leaf.end);
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
            }));

        for (start, end) in pairs {
            if let Some(&idx) = start_idx.get(&start) {
                leaves[idx].pair = end.and_then(|end| start_idx.get(&end)).copied();
            }
        }

        leaves
    }

    fn from_askama(askama_nodes: &[AskamaNode<'a>], pruned: &HashSet<usize>) -> BTreeSet<Leaf<'a>> {
        askama_nodes
            .iter()
            .enumerate()
            .filter(|(idx, _)| !pruned.contains(idx))
            .map(|(_, node)| Leaf::from_askama(node))
            .collect()
    }

    fn from_html(
        html_nodes: &[HtmlNode<'a>],
        askama_nodes: &[AskamaNode<'a>],
        source: &'a str,
    ) -> (BTreeSet<Leaf<'a>>, HashSet<usize>) {
        let mut leaves = BTreeSet::new();
        let mut pruned = HashSet::new();

        for node in html_nodes {
            match node {
                HtmlNode::Start { .. } | HtmlNode::Void { .. } | HtmlNode::SelfClosing { .. } => {
                    let Some(range) = node.range() else { continue };
                    let embed = Self::find_embedded(range, askama_nodes);
                    if embed.is_empty() {
                        leaves.insert(Leaf::from_html(node));
                    } else {
                        pruned.extend(embed.iter().copied());
                        let root = Root::Tag {
                            indent: node.indent(),
                            is_phrasing: node.is_phrasing(),
                            is_ws_sensitive: node.is_ws_sensitive(),
                        };
                        let content =
                            Cow::Owned(html::format_tag(range, source, askama_nodes, &embed));
                        leaves.insert(Leaf::grow(root, content, range.start, range.end));
                    }
                }
                HtmlNode::Text { text, .. } => {
                    let Some(range) = node.range() else { continue };
                    let embed = Self::find_embedded(range, askama_nodes);
                    if embed.is_empty() {
                        leaves.insert(Leaf::from_text(text, range.start, range.end));
                    } else {
                        leaves.extend(Leaf::from_mixed_text(range, askama_nodes, &embed, source));
                    }
                }
                HtmlNode::Raw { .. } | HtmlNode::Comment { .. } => {
                    let Some(range) = node.range() else { continue };
                    let embed = Self::find_embedded(range, askama_nodes);
                    if embed.is_empty() {
                        leaves.insert(Leaf::from_html(node));
                    } else {
                        pruned.extend(embed.iter().copied());
                        let root = match node {
                            HtmlNode::Raw { .. } => Root::Raw,
                            HtmlNode::Comment { .. } => Root::Comment,
                            _ => unreachable!(),
                        };
                        let content =
                            Cow::Owned(html::format_opaque(range, source, askama_nodes, &embed));
                        leaves.insert(Leaf::grow(root, content, range.start, range.end));
                    }
                }
                _ => {
                    leaves.insert(Leaf::from_html(node));
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
        askama_nodes: &[AskamaNode<'a>],
    ) -> Vec<usize> {
        askama_nodes
            .iter()
            .enumerate()
            .filter_map(|(i, n)| (n.start() >= range.start && n.end() <= range.end).then_some(i))
            .collect()
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Leaf<'a> {
    pub root: Root,
    pub content: Cow<'a, str>,
    pub ws_before: bool,
    pub ws_after: bool,
    pub start: usize,
    pub end: usize,
    pub pair: Option<usize>,
}

impl Ord for Leaf<'_> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.start.cmp(&other.start)
    }
}

impl PartialOrd for Leaf<'_> {
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
    Raw,
}

impl<'a> Leaf<'a> {
    fn grow(root: Root, content: Cow<'a, str>, start: usize, end: usize) -> Self {
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

    fn from_askama(askama_node: &AskamaNode<'a>) -> Self {
        let content = Cow::Owned(askama::format_askama_node(askama_node));

        let root = match askama_node {
            AskamaNode::Control { tag, .. } => Root::Control { tag: *tag },
            AskamaNode::Expression { .. } => Root::Expr,
            AskamaNode::Comment { .. } => Root::Comment,
        };

        Self::grow(root, content, askama_node.start(), askama_node.end())
    }

    fn from_html(html_node: &HtmlNode<'a>) -> Self {
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
            HtmlNode::Raw { .. } => Root::Raw,
            HtmlNode::Comment { .. } => Root::Comment,
        };

        Self::grow(root, content, start, end)
    }

    fn from_text(text: &str, start: usize, end: usize) -> Self {
        Self::grow(
            Root::Text,
            Cow::Owned(crate::normalize_ws(text)),
            start,
            end,
        )
    }

    fn from_mixed_text(
        range: &std::ops::Range<usize>,
        askama_nodes: &[AskamaNode<'a>],
        embed_askama: &[usize],
        source: &str,
    ) -> Vec<Self> {
        let mut segments = Vec::new();
        let mut curr_pos = range.start;

        for &idx in embed_askama {
            let node = &askama_nodes[idx];
            if node.start() > curr_pos {
                segments.push((curr_pos, node.start()));
            }
            curr_pos = node.end();
        }

        if curr_pos < range.end {
            segments.push((curr_pos, range.end));
        }

        segments
            .into_iter()
            .map(|(start, end)| Self::from_text(&source[start..end], start, end))
            .collect()
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

fn syntax_error(root_node: &Node, kind: String, source: &str, filepath: &str) -> Option<ErrorKind> {
    find_error_node(root_node).map(|node| {
        if node.is_missing() {
            let node_kind = node.kind();
            let parent = node.parent();

            let message = if let ("identifier", Some("block_statement")) =
                (node_kind, parent.map(|p| p.kind()))
            {
                "block tag requires a name".to_string()
            } else {
                format!("expected {} here", node_kind)
            };

            return ErrorKind::SyntaxError(Box::new(crate::BoxedSyntaxError {
                lang: kind,
                src: NamedSource::new(filepath, source.to_string()),
                span: range_to_span(&node.range()),
                message,
            }));
        }

        let range = refine_error_range(&node);
        let span = range_to_span(&range);

        ErrorKind::SyntaxError(Box::new(crate::BoxedSyntaxError {
            lang: kind,
            src: NamedSource::new(filepath, source.to_string()),
            span,
            message: "due to this".to_string(),
        }))
    })
}

fn find_error_node<'a>(node: &Node<'a>) -> Option<Node<'a>> {
    if node.is_error() || node.is_missing() {
        return Some(*node);
    }
    for child in node.children(&mut node.walk()) {
        if let Some(err) = find_error_node(&child) {
            return Some(err);
        }
    }
    None
}

fn refine_error_range(error_node: &Node) -> Range {
    let mut range = error_node.range();
    let children: Vec<_> = error_node.children(&mut error_node.walk()).collect();
    let non_error_children: Vec<_> = children.iter().filter(|c| !c.is_error()).collect();

    if non_error_children.is_empty() {
        if range.start_point.row != range.end_point.row {
            range.end_point = range.start_point;
            range.end_point.column = range.start_point.column + 1;
            range.end_byte = range.start_byte + 1;
        }
        return range;
    }

    let opening_pos = non_error_children
        .iter()
        .position(|c| matches!(c.kind(), "{{" | "{%" | "{#"));

    if let Some(open_idx) = opening_pos {
        let closing_kind = match non_error_children[open_idx].kind() {
            "{{" => "}}",
            "{%" => "%}",
            "{#" => "#}",
            _ => "",
        };

        if let Some(close_idx) = non_error_children[open_idx..]
            .iter()
            .position(|c| c.kind() == closing_kind)
        {
            let first = non_error_children[open_idx];
            let last = non_error_children[open_idx + close_idx];
            return range_between(first, last);
        }
    }

    let first = non_error_children.first().unwrap();
    let last = non_error_children.last().unwrap();
    range_between(first, last)
}

fn range_between(first: &Node, last: &Node) -> Range {
    Range {
        start_byte: first.range().start_byte,
        start_point: first.range().start_point,
        end_byte: last.range().end_byte,
        end_point: last.range().end_point,
    }
}

fn element_across_control<'a>(
    notes: &mut Notes,
    html_nodes: &[HtmlNode<'a>],
    askama_nodes: &[AskamaNode<'a>],
    source: &str,
    filepath: &str,
) -> Vec<(usize, usize)> {
    html_nodes
        .iter()
        .enumerate()
        .filter_map(|(i, node)| {
            if let HtmlNode::Start {
                range, end, name, ..
            } = node
                && let Some(end_byte) = *end
                && let Some(end_idx) = html_nodes.iter().position(|n| Some(n.start()) == *end)
            {
                let start_byte = range.start;

                has_crossing_boundary(start_byte, end_byte, askama_nodes).then(|| {
                    let close_tag_end = end_byte + 2 + name.len() + 1;
                    let span_range = range_from_bytes(source, start_byte, close_tag_end);

                    notes.warnings.push(KireiWarning::UnbalancedHtml {
                        src: NamedSource::new(filepath, source.to_string()),
                        span: range_to_span(&span_range),
                    });

                    (i, end_idx)
                })
            } else {
                None
            }
        })
        .collect()
}

fn has_crossing_boundary(start: usize, end: usize, askama_nodes: &[AskamaNode<'_>]) -> bool {
    if start == end {
        return false;
    }

    let (min_pos, max_pos) = (start.min(end), start.max(end));

    for node in askama_nodes {
        if let AskamaNode::Control {
            range,
            end: close_pos,
            tag,
            ..
        } = node
        {
            if matches!(
                tag,
                ControlTag::Else | ControlTag::ElseIf | ControlTag::When | ControlTag::MatchElse
            ) && range.start > min_pos
                && range.start < max_pos
                && let Some((parent_start, parent_end)) = find_closest_parent(range, askama_nodes)
            {
                let min_inside = min_pos >= parent_start && min_pos < parent_end;
                let max_inside = max_pos >= parent_start && max_pos < parent_end;

                if min_inside && max_inside {
                    return true;
                }
            }

            if let Some(close) = close_pos {
                let start_inside = min_pos >= range.end && min_pos < *close;
                let end_inside = max_pos >= range.end && max_pos < *close;

                if start_inside != end_inside {
                    return true;
                }
            }
        }
    }

    false
}

fn find_closest_parent(
    range: &std::ops::Range<usize>,
    askama_nodes: &[AskamaNode<'_>],
) -> Option<(usize, usize)> {
    let mut closest_parent = None;
    let mut smallest_size = usize::MAX;

    for parent in askama_nodes {
        if let AskamaNode::Control {
            range: parent_range,
            end: Some(parent_end),
            tag: parent_tag,
            ..
        } = parent
            && parent_tag.is_opening()
            && parent_range.end <= range.start
            && *parent_end >= range.end
        {
            let block_size = *parent_end - parent_range.end;
            if block_size < smallest_size {
                closest_parent = Some((parent_range.end, *parent_end));
                smallest_size = block_size;
            }
        }
    }

    closest_parent
}

fn range_from_bytes(source: &str, start: usize, end: usize) -> Range {
    Range {
        start_byte: start,
        end_byte: end,
        start_point: byte_to_point(source, start),
        end_point: byte_to_point(source, end),
    }
}

fn byte_to_point(source: &str, byte_pos: usize) -> Point {
    let (mut row, mut col) = (0, 0);
    for (i, c) in source.char_indices() {
        if i >= byte_pos {
            break;
        }
        if c == '\n' {
            row += 1;
            col = 0;
        } else {
            col += 1;
        }
    }
    Point { row, column: col }
}
