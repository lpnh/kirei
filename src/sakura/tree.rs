use super::{
    branch::{self, Style},
    leaf::{self, Leaf},
    ring,
};

use crate::{config::Config, parse::seed::Seed};

pub fn grow(seed: &Seed, cfg: &Config) -> String {
    let leaves = seed.grow_leaves();
    let indent_map = ring::generate_indent_map(&leaves);
    let rings = ring::grow_rings_recursive(&leaves, &indent_map, cfg, 0, leaves.len(), false);

    let mut branches = Vec::new();
    for ring in rings {
        branch::grow_branches_recursive(&ring, &leaves, &indent_map, cfg, &mut branches);
    }

    let mut output = String::new();

    for branch in branches {
        let indent = indent_map[branch.start];
        let base_indent = " ".repeat(indent * cfg.indent_size);

        let lines = match branch.style {
            Style::Inline => vec![branch_content(branch.start, branch.end, &leaves)],
            Style::Wrapped => render_wrapped(branch.start, branch.end, &leaves, &indent_map, cfg),
            Style::Comment => render_comment(branch.start, branch.end, &leaves, cfg),
            Style::Opaque => render_opaque(branch.start, branch.end, &leaves),
        };

        for line in lines {
            if line.is_empty() {
                output.push('\n');
            } else {
                output.push_str(&base_indent);
                output.push_str(&line);
                output.push('\n');
            }
        }
    }

    output
}

fn render_wrapped(
    start: usize,
    end: usize,
    leaves: &[Leaf],
    indent_map: &[usize],
    cfg: &Config,
) -> Vec<String> {
    let leaflets = leaf::grow_leaflets(start, end, leaves);
    let mut lines = Vec::new();
    let mut curr_line = String::new();
    let mut pair = None;

    for (i, leaflet) in leaflets.iter().enumerate() {
        let mut end = leaflets[i].pair.unwrap_or(i);

        while let Some(next) = leaflets.get(end + 1).filter(|n| !n.ws_before) {
            end = next.pair.unwrap_or(end + 1);
        }

        let curr_width = leaflets[i..=end]
            .iter()
            .enumerate()
            .map(|(i, ll)| ll.content.chars().count() + usize::from(i > 0 && ll.ws_before))
            .sum();

        if leaflet.ws_before
            && pair.is_none_or(|p| i > p)
            && !curr_line.is_empty()
            && !fits_2(start, &curr_line, curr_width, indent_map, cfg)
        {
            lines.push(std::mem::take(&mut curr_line));
        }

        if leaflet.ws_before && !curr_line.is_empty() {
            curr_line.push(' ');
        }

        curr_line.push_str(&leaflet.content);
        pair = pair.max(leaflet.pair);
    }

    lines.push(curr_line);
    lines
}

fn render_comment(start: usize, end: usize, leaves: &[Leaf], cfg: &Config) -> Vec<String> {
    let content = branch_content(start, end, leaves);
    let lines: Vec<&str> = content.lines().collect();
    let indent = " ".repeat(cfg.indent_size);

    lines
        .iter()
        .enumerate()
        .map(|(i, line)| {
            let line = line.trim();
            if line.is_empty() || i == 0 || i == lines.len() - 1 {
                line.to_string()
            } else {
                format!("{}{}", indent, line)
            }
        })
        .collect()
}

fn render_opaque(start: usize, end: usize, leaves: &[Leaf]) -> Vec<String> {
    let content = branch_content(start, end, leaves);
    let lines: Vec<&str> = content.lines().collect();

    let indent = lines
        .iter()
        .filter_map(|l| l.chars().position(|c| !c.is_whitespace()))
        .min()
        .unwrap_or(0);

    lines
        .iter()
        .enumerate()
        .filter_map(|(i, line)| {
            if !line.trim().is_empty() {
                Some(line.get(indent..).unwrap_or(line).to_string())
            } else if i != 0 && i != lines.len() - 1 {
                Some(String::new())
            } else {
                None
            }
        })
        .collect()
}

fn branch_content(start: usize, end: usize, leaves: &[Leaf]) -> String {
    let mut content = String::new();
    let mut prev_idx = None;

    for leaf_idx in start..=end {
        if let Some(leaf) = leaves.get(leaf_idx) {
            if let Some(prev) = prev_idx {
                let has_ws = leaves.get(prev).is_some_and(|l: &Leaf| {
                    l.preserves_ws() && leaf.preserves_ws() && (l.ws_after || leaf.ws_before)
                });
                if has_ws {
                    content.push(' ');
                }
            }
            content.push_str(&leaf.content);
            prev_idx = Some(leaf_idx);
        }
    }

    content
}

fn fits_2(start: usize, line: &str, extra: usize, indent_map: &[usize], cfg: &Config) -> bool {
    indent_map[start] * cfg.indent_size + line.chars().count() + extra < cfg.max_width
}
