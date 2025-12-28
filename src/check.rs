use tree_sitter::{Point, Range};

use crate::{
    askama::{AskamaNode, ControlTag},
    draw::{Annotation, Diagnostic},
    html::HtmlNode,
};

pub fn crossing_control_boundary(
    html_nodes: &[HtmlNode],
    askama_nodes: &[AskamaNode],
    source: &str,
) -> (Vec<Diagnostic>, Vec<(usize, usize)>) {
    let mut diagnostics = Vec::new();
    let mut crossing_indices = Vec::new();

    for i in 0..html_nodes.len() {
        let Some((start_byte, end_byte, end_idx, name)) = (match &html_nodes[i] {
            HtmlNode::Start {
                range,
                end: Some(end_idx),
                name,
                ..
            } => html_nodes.get(*end_idx).and_then(|n| match n {
                HtmlNode::End { start, .. } => Some((range.start, *start, *end_idx, name.clone())),
                _ => None,
            }),
            _ => None,
        }) else {
            continue;
        };

        if !has_crossing_boundary(start_byte, end_byte, askama_nodes) {
            continue;
        }

        let close_tag_end = end_byte + 2 + name.len() + 1;
        let span_range = range_from_bytes(source, start_byte, close_tag_end);

        diagnostics.push(
            Diagnostic::warning("unbalanced HTML across control blocks").with_label(
                span_range,
                "",
                Annotation::Primary,
            ),
        );

        crossing_indices.push((i, end_idx));
    }

    (diagnostics, crossing_indices)
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
