use miette::NamedSource;
use tree_sitter::{Node, Point, Range};

use crate::{
    ErrorKind,
    askama::{AskamaNode, ControlTag},
    html::HtmlNode,
    range_to_span,
    session::Session,
};

pub fn syntax_error(
    root_node: &Node,
    kind: String,
    source: &str,
    filepath: &str,
) -> Option<ErrorKind> {
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

pub fn element_across_control(
    session: &mut Session,
    html_nodes: &[HtmlNode],
    askama_nodes: &[AskamaNode],
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

                    session.emit_warning(&ErrorKind::UnbalancedHtml {
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

fn has_crossing_boundary(start: usize, end: usize, askama_nodes: &[AskamaNode]) -> bool {
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
    askama_nodes: &[AskamaNode],
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
