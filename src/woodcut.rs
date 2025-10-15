use std::borrow::Cow;
use textwrap::{Options, wrap};

use crate::{
    askama::AskamaNode,
    config::Config,
    html::HtmlNode,
    sakura_tree::{BranchStyle, Leaf, Root, SakuraTree},
};

pub fn print(tree: &SakuraTree) -> String {
    let estimated_inked_tree_size = tree.leaves.iter().map(|l| l.content.len()).sum::<usize>()
        + tree.leaves.len() * (tree.config.indent_size * 4);
    let mut inked_tree = String::with_capacity(estimated_inked_tree_size);

    for branch in &tree.branches {
        match branch.style {
            BranchStyle::Inline => {
                ink_inline_branch(&mut inked_tree, tree, branch.indent, &branch.leaves);
            }
            BranchStyle::MultiLine => {
                ink_multiline_branch(&mut inked_tree, tree, branch.indent, &branch.leaves);
            }
            BranchStyle::Wrapped => {
                ink_wrapped_branch(&mut inked_tree, tree, branch.indent, &branch.leaves);
            }
            BranchStyle::Raw => {
                ink_raw_branch(&mut inked_tree, tree, branch.indent, &branch.leaves);
            }
        }
    }

    inked_tree
}

fn ink_inline_branch(inked_tree: &mut String, tree: &SakuraTree, indent: i32, leaves: &[usize]) {
    let indent_str = indent_for(&tree.config, indent);
    let mut line_content = String::new();

    // Check if this branch contains only a comment
    let is_comment_only = leaves.len() == 1
        && leaves
            .first()
            .and_then(|&idx| tree.leaves.get(idx))
            .and_then(|leaf| leaf.maybe_askama_node())
            .is_some_and(AskamaNode::is_comment);

    for (i, &leaf_idx) in leaves.iter().enumerate() {
        if let Some(leaf) = tree.leaves.get(leaf_idx) {
            let content = content_normalized(leaf);

            // Add space between leaves when needed for proper formatting
            if i > 0 && should_add_space_before_leaf(tree, leaf_idx, leaves, i) {
                line_content.push(' ');
            }
            line_content.push_str(&content);
        }
    }

    if !line_content.trim().is_empty() {
        // Handle multi-line content ONLY for comments - indent each line
        if is_comment_only && line_content.contains('\n') {
            for line in line_content.lines() {
                inked_tree.push_str(&indent_str);
                inked_tree.push_str(line.trim_end());
                inked_tree.push('\n');
            }
        } else {
            inked_tree.push_str(&indent_str);
            inked_tree.push_str(line_content.trim_end());
            inked_tree.push('\n');
        }
    }
}

fn ink_multiline_branch(inked_tree: &mut String, tree: &SakuraTree, indent: i32, leaves: &[usize]) {
    let indent_str = indent_for(&tree.config, indent);

    for &leaf_idx in leaves {
        if let Some(leaf) = tree.leaves.get(leaf_idx) {
            let content = content_normalized(leaf);

            // Only wrap text content
            if leaf.is_html_text() || leaf.is_html_entity() {
                let wrapped_content = wrap_inline_content(&tree.config, &content, indent);
                inked_tree.push_str(&wrapped_content);
            } else {
                // Handle multiline content
                if content.contains('\n') {
                    let lines: Vec<&str> = content.lines().collect();
                    for (i, line) in lines.iter().enumerate() {
                        if i > 0 {
                            inked_tree.push('\n');
                        }
                        inked_tree.push_str(&indent_str);
                        inked_tree.push_str(line.trim_end());
                    }
                } else {
                    // Single line content
                    inked_tree.push_str(&indent_str);
                    inked_tree.push_str(content.trim_end());
                }
            }
            inked_tree.push('\n');
        }
    }
}

fn ink_wrapped_branch(inked_tree: &mut String, tree: &SakuraTree, indent: i32, leaves: &[usize]) {
    let indent_str = indent_for(&tree.config, indent);
    let available_width = tree.config.max_width - indent_str.len();

    let mut curr_line = String::new();
    let mut lines = Vec::new();

    for (i, &leaf_idx) in leaves.iter().enumerate() {
        if let Some(leaf) = tree.leaves.get(leaf_idx) {
            let content = content_normalized(leaf);

            // Check if we need space before this leaf
            let needs_space = i > 0 && should_add_space_before_leaf(tree, leaf_idx, leaves, i);

            // Lookahead: check if adding this leaf would exceed line length
            let space_len = usize::from(needs_space);
            let would_exceed = curr_line.len() + space_len + content.len() > available_width;

            // If would exceed AND current leaf is a StartTag, break before it
            if would_exceed && !curr_line.is_empty() {
                let is_start_tag = matches!(&leaf.root, Root::Html(HtmlNode::StartTag { .. }));

                if is_start_tag {
                    // Start new line before this StartTag
                    lines.push(curr_line.trim_end().to_string());
                    curr_line = String::new();
                    // Don't add space after breaking - we're on a new line
                    curr_line.push_str(&content);
                    continue;
                } else if leaf.is_html_text() {
                    // Current leaf is text and would exceed - try to wrap it
                    if !curr_line.is_empty() {
                        lines.push(curr_line.trim_end().to_string());
                        curr_line = String::new();
                    }
                    // Use textwrap for the text content
                    let wrapped = wrap_inline_content(&tree.config, &content, indent);
                    for wrapped_line in wrapped.lines() {
                        lines.push(wrapped_line.trim_start().to_string());
                    }
                    continue;
                }
            }

            // Add space if needed (but not at the start of a new line)
            if needs_space && !curr_line.is_empty() {
                curr_line.push(' ');
            }

            curr_line.push_str(&content);
        }
    }

    // Don't forget the last line
    if !curr_line.trim().is_empty() {
        lines.push(curr_line.trim_end().to_string());
    }

    // Output all lines with proper indentation
    for line in lines {
        if !line.trim().is_empty() {
            inked_tree.push_str(&indent_str);
            inked_tree.push_str(&line);
            inked_tree.push('\n');
        }
    }
}

fn ink_raw_branch(inked_tree: &mut String, tree: &SakuraTree, indent: i32, leaves: &[usize]) {
    let mut curr_indent = indent;

    for &leaf_idx in leaves {
        if let Some(leaf) = tree.leaves.get(leaf_idx) {
            let content = &leaf.content;

            // Process raw text content with preserved line structure
            if !content.trim().is_empty() {
                // Content now preserves original line breaks
                // Just add proper indentation to each line
                for line in content.lines() {
                    if line.trim().is_empty() {
                        // Preserve empty lines
                        inked_tree.push('\n');
                    } else {
                        let trimmed = line.trim();

                        // Check if line starts with '}' - if so, temporarily decrease indent
                        let temp_indent_decrease = trimmed.starts_with('}');
                        if temp_indent_decrease {
                            curr_indent = curr_indent.saturating_sub(1);
                        }

                        // Write the line with current indent level
                        let indent_str = indent_for(&tree.config, curr_indent);
                        inked_tree.push_str(&indent_str);
                        inked_tree.push_str(trimmed);
                        inked_tree.push('\n');

                        // After writing, if line ends with '{', increase indent for next lines
                        if trimmed.ends_with('{') {
                            curr_indent += 1;
                        }

                        // Handle lines with both braces (like "} else {")
                        // Count net brace difference for complex cases
                        let open_braces = trimmed.matches('{').count();
                        let close_braces = trimmed.matches('}').count();
                        let net_change = open_braces as i32 - close_braces as i32;

                        // If we already handled the opening brace above, subtract 1 from net_change
                        let net_change = if trimmed.ends_with('{') && net_change > 0 {
                            net_change - 1
                        } else {
                            net_change
                        };

                        // Apply any additional net change (for cases like multiple braces on one line)
                        if net_change > 0 {
                            curr_indent += net_change;
                        } else if net_change < 0 {
                            // For closing braces that weren't handled by the temp decrease
                            let decrease = (-net_change) as usize;
                            // Only decrease if we didn't already handle it with temp_indent_decrease
                            if !temp_indent_decrease || decrease > 1 {
                                let actual_decrease = if temp_indent_decrease {
                                    decrease - 1
                                } else {
                                    decrease
                                };
                                curr_indent = curr_indent.saturating_sub(actual_decrease as i32);
                            }
                        }
                    }
                }
            } else if content.is_empty() {
                // Preserve empty lines
                inked_tree.push('\n');
            }
        }
    }
}

fn content_normalized(leaf: &Leaf) -> Cow<'_, str> {
    if leaf.is_html_text() {
        Cow::Owned(normalize_text_content(&leaf.content))
    } else {
        Cow::Borrowed(&leaf.content)
    }
}

fn normalize_text_content(text: &str) -> String {
    // If only whitespace, return empty (space between expressions gets removed)
    if text.trim().is_empty() {
        return String::new();
    }

    // Remove leading whitespace (template indentation), preserve trailing
    let has_trailing = text.ends_with(char::is_whitespace);
    let trailing = if has_trailing { " " } else { "" };

    // Normalize internal content (removes leading whitespace)
    let internal = text.split_whitespace().collect::<Vec<_>>().join(" ");

    format!("{}{}", internal, trailing)
}

fn should_add_space_before_leaf(
    tree: &SakuraTree,
    curr_leaf_idx: usize,
    branch_indices: &[usize],
    position_in_branch: usize,
) -> bool {
    // Get the current leaf and the previous leaf in the branch
    let curr_leaf = tree.leaves.get(curr_leaf_idx);
    let prev_branch_index = if position_in_branch > 0 {
        branch_indices.get(position_in_branch - 1)
    } else {
        None
    };

    let prev_leaf = prev_branch_index.and_then(|&idx| tree.leaves.get(idx));

    let (Some(current), Some(prev)) = (curr_leaf, prev_leaf) else {
        return false;
    };

    let curr_content = content_normalized(current);
    let prev_content = content_normalized(prev);

    // Don't add space if either content is empty (nothing to separate)
    if curr_content.is_empty() || prev_content.is_empty() {
        return false;
    }

    // The whitespace is already present
    if curr_content.starts_with(' ') || prev_content.ends_with(' ') {
        return false;
    }

    // Add space between specific node type combinations
    match (&prev.root, &current.root) {
        // Text followed by HTML start tag or entity, entity followed by text
        (
            Root::Html(HtmlNode::Text(_)),
            Root::Html(HtmlNode::Entity(_) | HtmlNode::StartTag { .. }),
        )
        | (Root::Html(HtmlNode::Entity(_)), Root::Html(HtmlNode::Text(_))) => true,

        // Askama expression followed by text (!punctuation)
        (Root::Askama(prev_askama), Root::Html(HtmlNode::Text(_))) => {
            !prev_askama.is_expr()
                || !current
                    .content
                    .starts_with(|c: char| c.is_ascii_punctuation())
        }

        // Text (!punctuation) followed by Askama expression
        (Root::Html(HtmlNode::Text(_)), Root::Askama(curr_askama)) => {
            curr_askama.is_expr() && !prev.content.ends_with(|c: char| c.is_ascii_punctuation())
        }

        // HTML end tag followed by text that starts with a letter
        (Root::Html(HtmlNode::EndTag { .. }), Root::Html(HtmlNode::Text(_))) => {
            curr_content.starts_with(char::is_alphabetic)
        }

        // When clause followed by HTML start tag
        (Root::Askama(prev_askama), Root::Html(HtmlNode::StartTag { .. })) => {
            prev_askama.is_when_clause()
        }

        _ => false,
    }
}

fn wrap_inline_content(config: &Config, content: &str, indent: i32) -> String {
    let prefix = indent_for(config, indent);
    let available_width = config.max_width;

    let options = Options::new(available_width)
        .initial_indent(&prefix)
        .subsequent_indent(&prefix);

    wrap(content, &options).join("\n")
}

fn indent_for(config: &Config, indent: i32) -> String {
    " ".repeat(indent as usize * config.indent_size)
}
