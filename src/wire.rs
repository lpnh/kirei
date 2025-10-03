use crate::{
    askama::{self, AskamaNode},
    html,
    sakura_tree::{FormatStyle, NodeSource, SakuraBranch, SakuraTree, TrunkRing, TrunkShape},
};

pub(crate) fn wire(tree: &mut SakuraTree) {
    // Build indentation map for proper formatting
    let indent_map = analyze_indentation_structure(tree);

    wire_branches(tree, &indent_map);
}

// This builds a map of leaf_index -> indent_level based on control blocks
fn analyze_indentation_structure(tree: &SakuraTree) -> Vec<i32> {
    let mut indent_map = vec![0; tree.leaf_count()];
    let mut current_indent = 0;

    for (i, leaf) in tree.iter_leaves().enumerate() {
        // Check for HTML EndTags to decrease indent *before* assigning it
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

        // Check for HTML StartTags to increase indent for *subsequent* nodes
        // Only increase indent for StartTag (not Void or SelfClosingTag)
        if let NodeSource::Html(html_node) = &leaf.source
            && matches!(html_node, html::HtmlNode::StartTag { .. })
        {
            current_indent += 1;
        }
    }
    indent_map
}

fn wire_branches(tree: &mut SakuraTree, indent_map: &[i32]) {
    // Clone rings to avoid borrow checker issues
    let trunk: Vec<TrunkRing> = tree.trunk().cloned().collect();

    for ring in &trunk {
        wire_branches_from_trunk_rings(tree, ring, indent_map);
    }
}

// Recursively create branches
fn wire_branches_from_trunk_rings(tree: &mut SakuraTree, trunk: &TrunkRing, indent_map: &[i32]) {
    let style = decide_format_style(tree, trunk);

    if style == FormatStyle::MultiLine {
        match &trunk.shape {
            TrunkShape::CompleteElement {
                start_leaf,
                children,
                end_leaf,
                ..
            } => {
                // Create branch for opening tag
                let open_indent = indent_map.get(*start_leaf).copied().unwrap_or(0);
                let open_branch =
                    SakuraBranch::new(vec![*start_leaf], FormatStyle::MultiLine, open_indent);
                tree.push_branch(open_branch);

                // Recursively process children
                for child_trunk in children {
                    wire_branches_from_trunk_rings(tree, child_trunk, indent_map);
                }

                // Create branch for closing tag
                let close_indent = indent_map.get(*end_leaf).copied().unwrap_or(0);
                let close_branch =
                    SakuraBranch::new(vec![*end_leaf], FormatStyle::MultiLine, close_indent);
                tree.push_branch(close_branch);
            }
            TrunkShape::ControlBlock {
                open_leaf,
                children,
                close_leaf,
                ..
            } => {
                // Create branch for opening control
                let open_indent = indent_map.get(*open_leaf).copied().unwrap_or(0);
                let open_branch =
                    SakuraBranch::new(vec![*open_leaf], FormatStyle::MultiLine, open_indent);
                tree.push_branch(open_branch);

                // Recursively process children
                for child_trunk in children {
                    wire_branches_from_trunk_rings(tree, child_trunk, indent_map);
                }

                // Create branch for closing control
                let close_indent = indent_map.get(*close_leaf).copied().unwrap_or(0);
                let close_branch =
                    SakuraBranch::new(vec![*close_leaf], FormatStyle::MultiLine, close_indent);
                tree.push_branch(close_branch);
            }
            TrunkShape::EmptyAskamaBlock {
                open_leaf,
                close_leaf,
            } => {
                // Create branch for the opening block
                let open_indent = indent_map.get(*open_leaf).copied().unwrap_or(0);
                let open_branch =
                    SakuraBranch::new(vec![*open_leaf], FormatStyle::MultiLine, open_indent);
                tree.push_branch(open_branch);

                // Create branch for the closing block
                let close_indent = indent_map.get(*close_leaf).copied().unwrap_or(0);
                let close_branch =
                    SakuraBranch::new(vec![*close_leaf], FormatStyle::MultiLine, close_indent);
                tree.push_branch(close_branch);
            }
            _ => {
                // For non-container, create single branch
                let branch = create_branch_from_trunk(tree, trunk, indent_map);
                tree.push_branch(branch);
            }
        }
    } else {
        // Inline: create single branch
        let branch = create_branch_from_trunk(tree, trunk, indent_map);
        tree.push_branch(branch);
    }
}

fn create_branch_from_trunk(
    tree: &SakuraTree,
    trunk: &TrunkRing,
    indent_map: &[i32],
) -> SakuraBranch {
    let leaf_indices = trunk.all_leaf_indices();
    let base_indent = leaf_indices
        .first()
        .and_then(|&idx| indent_map.get(idx))
        .copied()
        .unwrap_or(0);

    let style = decide_format_style(tree, trunk);

    SakuraBranch::new(leaf_indices, style, base_indent)
}

fn decide_format_style(tree: &SakuraTree, trunk: &TrunkRing) -> FormatStyle {
    match &trunk.shape {
        // Inline if fits, otherwise multiline
        TrunkShape::EmptyAskamaBlock { .. } | TrunkShape::Standalone { .. } => {
            if trunk.total_chars <= tree.config.max_line_length {
                FormatStyle::Inline
            } else {
                FormatStyle::MultiLine
            }
        }

        // ScriptStyle  always use RawText style
        TrunkShape::ScriptStyle { .. } => FormatStyle::RawText,

        // Text sequences are inline if they fit
        TrunkShape::TextSequence { leaves } => {
            // Check if this sequence starts with a when clause
            let starts_with_when = leaves
                .first()
                .and_then(|&idx| tree.get_leaf(idx))
                .and_then(|leaf| leaf.maybe_askama_node())
                .and_then(AskamaNode::get_ctrl_tag)
                .is_some_and(|tag| tag.boundary() == askama::Boundary::Inner);

            if starts_with_when {
                // When clauses with their content should stay inline
                FormatStyle::Inline
            } else if trunk.total_chars <= tree.config.max_line_length {
                FormatStyle::Inline
            } else {
                FormatStyle::WrappedText
            }
        }

        TrunkShape::CompleteElement {
            is_semantic_inline,
            start_leaf,
            ..
        } => {
            // Check if this is style/script element
            if let Some(leaf) = tree.get_leaf(*start_leaf)
                && let NodeSource::Html(html_node) = &leaf.source
                && html_node.is_style_or_script_element()
            {
                return FormatStyle::MultiLine;
            }

            // Check structure constraints
            let fits_in_line = trunk.total_chars <= tree.config.max_line_length;
            let has_control_blocks = trunk.has_control_blocks();
            let has_block_children = trunk.contains_block_children();

            if *is_semantic_inline {
                // Inline elements: inline if they fit
                if fits_in_line {
                    FormatStyle::Inline
                } else {
                    FormatStyle::MultiLine
                }
            } else {
                // Block elements: inline only if simple and fits
                if fits_in_line && !has_control_blocks && !has_block_children {
                    FormatStyle::Inline
                } else {
                    FormatStyle::MultiLine
                }
            }
        }

        // Askama control blocks: always block format
        TrunkShape::ControlBlock { .. } => FormatStyle::MultiLine,
    }
}
