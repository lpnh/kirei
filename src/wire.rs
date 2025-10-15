use crate::{
    askama::{self, AskamaNode},
    html,
    sakura_tree::{Branch, BranchStyle, Layer, Ring, Root, SakuraTree},
};

pub fn wire(tree: &mut SakuraTree) {
    let indent_map = analyze_indentation_structure(tree);

    wire_branches(tree, &indent_map);
}

// Build indentation map for proper formatting
fn analyze_indentation_structure(tree: &SakuraTree) -> Vec<i32> {
    let mut indent_map = vec![0; tree.leaves.len()];
    let mut curr_indent = 0;

    for (i, leaf) in tree.leaves.iter().enumerate() {
        // Check for HTML EndTags to decrease indent before assigning it
        if let Root::Html(html_node) = &leaf.root
            && html_node.is_closing_tag()
        {
            curr_indent = (curr_indent - 1).max(0);
        }

        // Handle Askama indentation changes
        if let Some(askama_node) = leaf.maybe_askama_node() {
            let (pre_delta, post_delta) = askama_node.indent_delta();
            curr_indent = (curr_indent + pre_delta).max(0);
            indent_map[i] = curr_indent;
            curr_indent = (curr_indent + post_delta).max(0);
        } else {
            // Not an Askama node, so just assign the current indent
            indent_map[i] = curr_indent;
        }

        // Check for HTML StartTags to increase indent for subsequent nodes
        // Only increase indent for StartTag (not Void, SelfClosingTag, etc.)
        if let Root::Html(html_node) = &leaf.root
            && matches!(html_node, html::HtmlNode::StartTag { .. })
        {
            curr_indent += 1;
        }
    }
    indent_map
}

fn wire_branches(tree: &mut SakuraTree, indent_map: &[i32]) {
    let rings: Vec<Ring> = tree.rings.clone();

    for ring in &rings {
        wire_branch_recursive(tree, ring, indent_map);
    }
}

fn wire_branch_recursive(tree: &mut SakuraTree, ring: &Ring, indent_map: &[i32]) {
    let style = decide_branch_style(tree, ring);

    if style == BranchStyle::MultiLine {
        match &ring.layer {
            Layer::CompleteElement {
                start_leaf,
                inner_rings,
                end_leaf,
                ..
            } => {
                wire_multiple_branches(tree, *start_leaf, inner_rings, *end_leaf, indent_map);
            }
            Layer::ControlBlock {
                open_leaf,
                inner_rings,
                close_leaf,
                ..
            } => {
                wire_multiple_branches(tree, *open_leaf, inner_rings, *close_leaf, indent_map);
            }
            Layer::EmptyControlBlock {
                open_leaf,
                close_leaf,
            } => {
                wire_multiple_branches(tree, *open_leaf, &[], *close_leaf, indent_map);
            }
            _ => {
                tree.branches
                    .push(wire_single_branch(tree, ring, indent_map));
            }
        }
    } else {
        tree.branches
            .push(wire_single_branch(tree, ring, indent_map));
    }
}

fn wire_multiple_branches(
    tree: &mut SakuraTree,
    open_leaf: usize,
    inner_rings: &[Ring],
    close_leaf: usize,
    indent_map: &[i32],
) {
    // Wire first branch (opening element/control tag)
    let open_indent = indent_map.get(open_leaf).copied().unwrap_or(0);
    tree.branches.push(Branch::grow(
        vec![open_leaf],
        BranchStyle::MultiLine,
        open_indent,
    ));

    // Recursively process inner rings
    for ring in inner_rings {
        wire_branch_recursive(tree, ring, indent_map);
    }

    // Wire last branch (closing element/control tag)
    let close_indent = indent_map.get(close_leaf).copied().unwrap_or(0);
    tree.branches.push(Branch::grow(
        vec![close_leaf],
        BranchStyle::MultiLine,
        close_indent,
    ));
}

fn wire_single_branch(tree: &SakuraTree, ring: &Ring, indent_map: &[i32]) -> Branch {
    let leaves = ring.all_leaf_indices();
    let indent = leaves
        .first()
        .and_then(|&idx| indent_map.get(idx))
        .copied()
        .unwrap_or(0);

    let style = decide_branch_style(tree, ring);

    Branch::grow(leaves, style, indent)
}

fn decide_branch_style(tree: &SakuraTree, ring: &Ring) -> BranchStyle {
    let fits_in_line = ring.total_chars <= tree.config.max_width;

    match &ring.layer {
        Layer::EmptyControlBlock { .. } | Layer::Standalone { .. } => {
            if fits_in_line {
                BranchStyle::Inline
            } else {
                BranchStyle::MultiLine
            }
        }

        Layer::ScriptStyle { .. } => BranchStyle::Raw,

        Layer::TextSequence { leaves } => {
            // Check if this sequence starts with a when clause
            let starts_with_when = leaves
                .first()
                .and_then(|&idx| tree.leaves.get(idx))
                .and_then(|leaf| leaf.maybe_askama_node())
                .and_then(AskamaNode::get_ctrl_tag)
                .is_some_and(|tag| tag.boundary() == askama::Boundary::Inner);

            if starts_with_when || fits_in_line {
                BranchStyle::Inline
            } else {
                BranchStyle::Wrapped
            }
        }

        Layer::CompleteElement { .. } => {
            // Check structure constraints
            if fits_in_line && !ring.has_block {
                BranchStyle::Inline
            } else {
                BranchStyle::MultiLine
            }
        }

        Layer::ControlBlock { .. } => BranchStyle::MultiLine,
    }
}
