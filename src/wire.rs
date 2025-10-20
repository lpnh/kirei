use crate::{
    html,
    sakura_tree::{Branch, BranchStyle, Ring, Root, SakuraTree, Twig},
};

pub fn wire(tree: &mut SakuraTree) {
    let indent_map = analyze_indentation_structure(tree);
    wire_branches(tree, &indent_map);
}

fn analyze_indentation_structure(tree: &SakuraTree) -> Vec<i32> {
    let mut indent_map = vec![0; tree.leaves.len()];
    let mut curr_indent = 0;

    for (i, leaf) in tree.leaves.iter().enumerate() {
        if let Root::Html(html_node) = &leaf.root
            && html_node.is_closing_tag()
        {
            curr_indent = (curr_indent - 1).max(0);
        }

        if let Some(askama_node) = leaf.maybe_askama_node() {
            let (pre_delta, post_delta) = askama_node.indent_delta();
            curr_indent = (curr_indent + pre_delta).max(0);
            indent_map[i] = curr_indent;
            curr_indent = (curr_indent + post_delta).max(0);
        } else {
            indent_map[i] = curr_indent;
        }

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
        wire_branch(tree, ring, indent_map);
    }
}

fn wire_branch(tree: &mut SakuraTree, ring: &Ring, indent_map: &[i32]) {
    let fits = ring.total_chars(tree) <= tree.config.max_width;
    let has_block = ring.has_block(tree);

    match ring {
        // Compound
        Ring::Element(leaves, inner) => {
            if fits && !has_block {
                push_branch(tree, *leaves, BranchStyle::Inline, indent_map);
            } else {
                let (start_leaf, end_leaf) = leaves.as_range();
                wire_open_close_branches(tree, start_leaf, inner, end_leaf, indent_map);
            }
        }
        Ring::ControlBlock(leaves, inner) => {
            let (start_leaf, end_leaf) = leaves.as_range();
            wire_open_close_branches(tree, start_leaf, inner, end_leaf, indent_map);
        }
        Ring::EmptyBlock(leaves) => {
            if fits {
                push_branch(tree, *leaves, BranchStyle::Inline, indent_map);
            } else {
                let (start_leaf, end_leaf) = leaves.as_range();
                wire_open_close_branches(tree, start_leaf, &[], end_leaf, indent_map);
            }
        }
        Ring::TextSequence(leaves) => {
            let style = if fits {
                BranchStyle::Inline
            } else {
                BranchStyle::Multiple
            };
            push_branch(tree, *leaves, style, indent_map);
        }
        Ring::MatchArm(_, _) => {
            let leaves = ring.all_leaf_indices();
            push_branch(tree, leaves, BranchStyle::Inline, indent_map);
        }

        // Atomic
        Ring::RawText(leaves) => {
            push_branch(tree, *leaves, BranchStyle::Raw, indent_map);
        }
        Ring::Comment(leaves) => {
            let style = if fits {
                BranchStyle::Inline
            } else {
                BranchStyle::AskamaComment
            };
            push_branch(tree, *leaves, style, indent_map);
        }
        Ring::InlineText(leaves) => {
            let style = if fits {
                BranchStyle::Inline
            } else {
                BranchStyle::SingleHtmlText
            };
            push_branch(tree, *leaves, style, indent_map);
        }
        Ring::Other(leaves) => {
            let style = if fits {
                BranchStyle::Inline
            } else {
                BranchStyle::OpenClose
            };
            push_branch(tree, *leaves, style, indent_map);
        }
    }
}

fn wire_open_close_branches(
    tree: &mut SakuraTree,
    start_leaf: usize,
    inner: &[Ring],
    end_leaf: usize,
    indent_map: &[i32],
) {
    let open_indent = indent_map.get(start_leaf).copied().unwrap_or(0);
    tree.branches.push(Branch::grow(
        Twig::Single(start_leaf),
        BranchStyle::OpenClose,
        open_indent,
    ));

    for ring in inner {
        wire_branch(tree, ring, indent_map);
    }

    let close_indent = indent_map.get(end_leaf).copied().unwrap_or(0);
    tree.branches.push(Branch::grow(
        Twig::Single(end_leaf),
        BranchStyle::OpenClose,
        close_indent,
    ));
}

fn get_indent(indent_map: &[i32], twig: Twig) -> i32 {
    indent_map.get(twig.first()).copied().unwrap_or(0)
}

fn push_branch(tree: &mut SakuraTree, twig: Twig, style: BranchStyle, indent_map: &[i32]) {
    let indent = get_indent(indent_map, twig);
    tree.branches.push(Branch::grow(twig, style, indent));
}
