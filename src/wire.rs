use crate::{
    askama::{self, AskamaNode},
    html,
    sakura_tree::{BranchStyle, NodeSource, SakuraBranch, SakuraTree, TrunkLayer, TrunkRing},
};

pub(crate) fn wire(tree: &mut SakuraTree) {
    let indent_map = analyze_indentation_structure(tree);

    wire_branches(tree, &indent_map);
}

// Build indentation map for proper formatting
fn analyze_indentation_structure(tree: &SakuraTree) -> Vec<i32> {
    let mut indent_map = vec![0; tree.leaf_count()];
    let mut current_indent = 0;

    for (i, leaf) in tree.iter_leaves().enumerate() {
        // Check for HTML EndTags to decrease indent before assigning it
        if let NodeSource::Html(html_node) = &leaf.source
            && html_node.is_closing_tag()
        {
            current_indent = (current_indent - 1).max(0);
        }

        // Handle Askama indentation changes
        if let Some(askama_node) = leaf.maybe_askama_node() {
            let (pre_delta, post_delta) = askama_node.indent_delta();
            current_indent = (current_indent + pre_delta).max(0);
            indent_map[i] = current_indent;
            current_indent = (current_indent + post_delta).max(0);
        } else {
            // Not an Askama node, so just assign the current indent
            indent_map[i] = current_indent;
        }

        // Check for HTML StartTags to increase indent for subsequent nodes
        // Only increase indent for StartTag (not Void, SelfClosingTag, etc.)
        if let NodeSource::Html(html_node) = &leaf.source
            && matches!(html_node, html::HtmlNode::StartTag { .. })
        {
            current_indent += 1;
        }
    }
    indent_map
}

fn wire_branches(tree: &mut SakuraTree, indent_map: &[i32]) {
    let trunk_rings: Vec<TrunkRing> = tree.iter_rings().cloned().collect();

    for ring in &trunk_rings {
        wire_branch_recursive(tree, ring, indent_map);
    }
}

fn wire_branch_recursive(tree: &mut SakuraTree, trunk_ring: &TrunkRing, indent_map: &[i32]) {
    let style = decide_branch_style(tree, trunk_ring);

    if style == BranchStyle::MultiLine {
        match &trunk_ring.layer {
            TrunkLayer::CompleteElement {
                start_leaf,
                inner_rings,
                end_leaf,
                ..
            } => {
                // Wire branch for opening tag
                let open_indent = indent_map.get(*start_leaf).copied().unwrap_or(0);
                let open_branch =
                    SakuraBranch::grow(vec![*start_leaf], BranchStyle::MultiLine, open_indent);
                tree.grow_branch(open_branch);

                // Recursively process inner rings
                for ring in inner_rings {
                    wire_branch_recursive(tree, ring, indent_map);
                }

                // Wire branch for closing tag
                let close_indent = indent_map.get(*end_leaf).copied().unwrap_or(0);
                let close_branch =
                    SakuraBranch::grow(vec![*end_leaf], BranchStyle::MultiLine, close_indent);
                tree.grow_branch(close_branch);
            }
            TrunkLayer::ControlBlock {
                open_leaf,
                inner_rings,
                close_leaf,
                ..
            } => {
                // Wire branch for opening control
                let open_indent = indent_map.get(*open_leaf).copied().unwrap_or(0);
                let open_branch =
                    SakuraBranch::grow(vec![*open_leaf], BranchStyle::MultiLine, open_indent);
                tree.grow_branch(open_branch);

                // Recursively process inner rings
                for trunk_ring in inner_rings {
                    wire_branch_recursive(tree, trunk_ring, indent_map);
                }

                // Wire branch for closing control
                let close_indent = indent_map.get(*close_leaf).copied().unwrap_or(0);
                let close_branch =
                    SakuraBranch::grow(vec![*close_leaf], BranchStyle::MultiLine, close_indent);
                tree.grow_branch(close_branch);
            }
            TrunkLayer::EmptyControlBlock {
                open_leaf,
                close_leaf,
            } => {
                // Wire branch for the opening block
                let open_indent = indent_map.get(*open_leaf).copied().unwrap_or(0);
                let open_branch =
                    SakuraBranch::grow(vec![*open_leaf], BranchStyle::MultiLine, open_indent);
                tree.grow_branch(open_branch);

                // Wire branch for the closing block
                let close_indent = indent_map.get(*close_leaf).copied().unwrap_or(0);
                let close_branch =
                    SakuraBranch::grow(vec![*close_leaf], BranchStyle::MultiLine, close_indent);
                tree.grow_branch(close_branch);
            }
            _ => {
                // For non-container, wire single branch
                let branch = wire_single_branch(tree, trunk_ring, indent_map);
                tree.grow_branch(branch);
            }
        }
    } else {
        // Inline: wire single branch
        let branch = wire_single_branch(tree, trunk_ring, indent_map);
        tree.grow_branch(branch);
    }
}

fn wire_single_branch(
    tree: &SakuraTree,
    trunk_ring: &TrunkRing,
    indent_map: &[i32],
) -> SakuraBranch {
    let leaf_indices = trunk_ring.all_leaf_indices();
    let indent_level = leaf_indices
        .first()
        .and_then(|&idx| indent_map.get(idx))
        .copied()
        .unwrap_or(0);

    let style = decide_branch_style(tree, trunk_ring);

    SakuraBranch::grow(leaf_indices, style, indent_level)
}

fn decide_branch_style(tree: &SakuraTree, trunk_ring: &TrunkRing) -> BranchStyle {
    match &trunk_ring.layer {
        // Inline if fits, otherwise multiline
        TrunkLayer::EmptyControlBlock { .. } | TrunkLayer::Standalone { .. } => {
            if trunk_ring.total_chars <= tree.config.max_line_length {
                BranchStyle::Inline
            } else {
                BranchStyle::MultiLine
            }
        }

        // ScriptStyle  always use RawText style
        TrunkLayer::ScriptStyle { .. } => BranchStyle::RawText,

        // Text sequences are inline if they fit
        TrunkLayer::TextSequence { leaves } => {
            // Check if this sequence starts with a when clause
            let starts_with_when = leaves
                .first()
                .and_then(|&idx| tree.get_leaf(idx))
                .and_then(|leaf| leaf.maybe_askama_node())
                .and_then(AskamaNode::get_ctrl_tag)
                .is_some_and(|tag| tag.boundary() == askama::Boundary::Inner);

            if starts_with_when {
                // When clauses with their content should stay inline
                BranchStyle::Inline
            } else if trunk_ring.total_chars <= tree.config.max_line_length {
                BranchStyle::Inline
            } else {
                BranchStyle::WrappedText
            }
        }

        TrunkLayer::CompleteElement {
            is_semantic_inline,
            start_leaf,
            ..
        } => {
            // Check if this is style/script element
            if let Some(leaf) = tree.get_leaf(*start_leaf)
                && let NodeSource::Html(html_node) = &leaf.source
                && html_node.is_style_or_script_element()
            {
                return BranchStyle::MultiLine;
            }

            // Check structure constraints
            let fits_in_line = trunk_ring.total_chars <= tree.config.max_line_length;
            let has_control_block = trunk_ring.has_control_block_ring();
            let has_non_inline_ring = trunk_ring.has_non_semantic_inline_ring();

            if *is_semantic_inline {
                // Inline elements: inline if they fit
                if fits_in_line {
                    BranchStyle::Inline
                } else {
                    BranchStyle::MultiLine
                }
            } else {
                // Block elements: inline only if simple and fits
                if fits_in_line && !has_control_block && !has_non_inline_ring {
                    BranchStyle::Inline
                } else {
                    BranchStyle::MultiLine
                }
            }
        }

        // Askama control blocks: always block format
        TrunkLayer::ControlBlock { .. } => BranchStyle::MultiLine,
    }
}
