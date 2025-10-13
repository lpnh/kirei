use textwrap::{Options, wrap};

use crate::{
    askama::AskamaNode,
    config::Config,
    html::HtmlNode,
    sakura_tree::{BranchStyle, NodeSource, SakuraLeaf, SakuraTree},
};

pub(crate) fn print(tree: &SakuraTree) -> String {
    let mut inked_tree = String::new();

    for branch in tree.iter_branches() {
        let inked_branch = match branch.style {
            BranchStyle::Inline => {
                ink_inline_branch(tree, branch.indent_level, &branch.leaf_indices)
            }
            BranchStyle::MultiLine => {
                ink_multiline_branch(tree, branch.indent_level, &branch.leaf_indices)
            }
            BranchStyle::Wrapped => {
                ink_wrapped_branch(tree, branch.indent_level, &branch.leaf_indices)
            }
            BranchStyle::Raw => ink_raw_branch(tree, branch.indent_level, &branch.leaf_indices),
        };

        inked_tree.push_str(&inked_branch);
    }

    inked_tree
}

fn ink_inline_branch(tree: &SakuraTree, indent_level: i32, leaf_indices: &[usize]) -> String {
    let indent_str = indent_for(&tree.config, indent_level);
    let mut line_content = String::new();

    // Check if this branch contains only a comment
    let is_comment_only = leaf_indices.len() == 1
        && leaf_indices
            .first()
            .and_then(|&idx| tree.get_leaf(idx))
            .and_then(|leaf| leaf.maybe_askama_node())
            .is_some_and(AskamaNode::is_comment);

    for (i, &leaf_index) in leaf_indices.iter().enumerate() {
        if let Some(leaf) = tree.get_leaf(leaf_index) {
            let content = content_normalized(leaf);

            // Add space between leaves when needed for proper formatting
            if i > 0 && should_add_space_before_leaf(tree, leaf_index, leaf_indices, i) {
                line_content.push(' ');
            }
            line_content.push_str(&content);
        }
    }

    let mut output = String::new();
    if !line_content.trim().is_empty() {
        // Handle multi-line content ONLY for comments - indent each line
        if is_comment_only && line_content.contains('\n') {
            for line in line_content.lines() {
                output.push_str(&indent_str);
                output.push_str(line.trim_end());
                output.push('\n');
            }
        } else {
            output.push_str(&indent_str);
            output.push_str(line_content.trim_end());
            output.push('\n');
        }
    }
    output
}

fn ink_multiline_branch(tree: &SakuraTree, indent_level: i32, leaf_indices: &[usize]) -> String {
    let mut output = String::new();

    for &leaf_index in leaf_indices {
        if let Some(leaf) = tree.get_leaf(leaf_index) {
            let content = content_normalized(leaf);

            if !content.trim().is_empty() {
                let indent_str = indent_for(&tree.config, indent_level);

                if leaf.is_html_text() || leaf.is_html_entity() {
                    // Only wrap pure text content
                    let wrapped_content = wrap_inline_content(&tree.config, &content, indent_level);
                    output.push_str(&wrapped_content);
                } else {
                    // HTML tags and Askama expressions - handle multiline content properly
                    if content.contains('\n') {
                        // Multiline content: indent each line properly
                        let lines: Vec<&str> = content.lines().collect();
                        for (i, line) in lines.iter().enumerate() {
                            if i > 0 {
                                output.push('\n');
                            }
                            output.push_str(&indent_str);
                            output.push_str(line.trim_end());
                        }
                    } else {
                        // Single line content
                        output.push_str(&indent_str);
                        output.push_str(content.trim_end());
                    }
                }
                output.push('\n');
            }
        }
    }

    output
}

fn ink_wrapped_branch(tree: &SakuraTree, indent_level: i32, leaf_indices: &[usize]) -> String {
    let indent_str = indent_for(&tree.config, indent_level);
    let available_width = tree.config.max_line_length - indent_str.len();

    let mut current_line = String::new();
    let mut lines = Vec::new();

    for (i, &leaf_index) in leaf_indices.iter().enumerate() {
        if let Some(leaf) = tree.get_leaf(leaf_index) {
            let content = content_normalized(leaf);

            // Check if we need space before this leaf
            let needs_space =
                i > 0 && should_add_space_before_leaf(tree, leaf_index, leaf_indices, i);

            // Lookahead: check if adding this leaf would exceed line length
            let space_len = usize::from(needs_space);
            let would_exceed = current_line.len() + space_len + content.len() > available_width;

            // If would exceed AND current leaf is a StartTag, break before it
            if would_exceed && !current_line.is_empty() {
                let is_start_tag =
                    matches!(&leaf.source, NodeSource::Html(HtmlNode::StartTag { .. }));

                if is_start_tag {
                    // Start new line before this StartTag
                    lines.push(current_line.trim_end().to_string());
                    current_line = String::new();
                    // Don't add space after breaking - we're on a new line
                    current_line.push_str(&content);
                    continue;
                } else if leaf.is_html_text() {
                    // Current leaf is text and would exceed - try to wrap it
                    if !current_line.is_empty() {
                        lines.push(current_line.trim_end().to_string());
                        current_line = String::new();
                    }
                    // Use textwrap for the text content
                    let wrapped = wrap_inline_content(&tree.config, &content, indent_level);
                    for wrapped_line in wrapped.lines() {
                        lines.push(wrapped_line.trim_start().to_string());
                    }
                    continue;
                }
            }

            // Add space if needed (but not at the start of a new line)
            if needs_space && !current_line.is_empty() {
                current_line.push(' ');
            }

            current_line.push_str(&content);
        }
    }

    // Don't forget the last line
    if !current_line.trim().is_empty() {
        lines.push(current_line.trim_end().to_string());
    }

    // Output all lines with proper indentation
    let mut output = String::new();
    for line in lines {
        if !line.trim().is_empty() {
            output.push_str(&indent_str);
            output.push_str(&line);
            output.push('\n');
        }
    }
    output
}

fn ink_raw_branch(tree: &SakuraTree, indent_level: i32, leaf_indices: &[usize]) -> String {
    let mut output = String::new();
    let mut current_indent = indent_level;

    for &leaf_index in leaf_indices {
        if let Some(leaf) = tree.get_leaf(leaf_index) {
            let content = &leaf.content;

            // Process raw text content with preserved line structure
            if !content.trim().is_empty() {
                // Content now preserves original line breaks
                // Just add proper indentation to each line
                for line in content.lines() {
                    if line.trim().is_empty() {
                        // Preserve empty lines
                        output.push('\n');
                    } else {
                        let trimmed = line.trim();

                        // Check if line starts with '}' - if so, temporarily decrease indent
                        let temp_indent_decrease = trimmed.starts_with('}');
                        if temp_indent_decrease {
                            current_indent = current_indent.saturating_sub(1);
                        }

                        // Write the line with current indent level
                        let indent_str = indent_for(&tree.config, current_indent);
                        output.push_str(&indent_str);
                        output.push_str(trimmed);
                        output.push('\n');

                        // After writing, if line ends with '{', increase indent for next lines
                        if trimmed.ends_with('{') {
                            current_indent += 1;
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
                            current_indent += net_change;
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
                                current_indent =
                                    current_indent.saturating_sub(actual_decrease as i32);
                            }
                        }
                    }
                }
            } else if content.is_empty() {
                // Preserve empty lines
                output.push('\n');
            }
        }
    }

    output
}

fn content_normalized(leaf: &SakuraLeaf) -> String {
    if leaf.is_html_text() {
        normalize_text_content(&leaf.content)
    } else {
        leaf.content.clone()
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
    current_leaf_index: usize,
    branch_indices: &[usize],
    position_in_branch: usize,
) -> bool {
    // Get the current leaf and the previous leaf in the branch
    let current_leaf = tree.get_leaf(current_leaf_index);
    let prev_branch_index = if position_in_branch > 0 {
        branch_indices.get(position_in_branch - 1)
    } else {
        None
    };

    let prev_leaf = prev_branch_index.and_then(|&idx| tree.get_leaf(idx));

    if let (Some(current), Some(prev)) = (current_leaf, prev_leaf) {
        let current_content = content_normalized(current);
        let prev_content = content_normalized(prev);

        // Don't add space if the current leaf already starts with space
        if current_content.starts_with(' ') {
            return false;
        }

        // Don't add space if previous leaf ends with space
        if prev_content.ends_with(' ') {
            return false;
        }

        // Add space between specific node type combinations
        match (&prev.source, &current.source) {
            // Entity followed by Text OR Text followed by Entity - add space
            (NodeSource::Html(HtmlNode::Entity(_)), NodeSource::Html(HtmlNode::Text(_)))
            | (NodeSource::Html(HtmlNode::Text(_)), NodeSource::Html(HtmlNode::Entity(_))) => true,
            // Askama expression followed by text - add space (preserve leading)
            (NodeSource::Askama(prev_askama), NodeSource::Html(HtmlNode::Text(_))) => {
                if prev_askama.is_expr() {
                    // Don't add space if text starts with punctuation
                    let starts_with_punct = current
                        .content
                        .chars()
                        .next()
                        .is_some_and(|c| c.is_ascii_punctuation());
                    !starts_with_punct
                } else {
                    // Control block
                    prev_askama.get_ctrl_tag().is_some()
                }
            }
            // Text followed by Askama expression - add space (preserve trailing)
            (NodeSource::Html(HtmlNode::Text(_)), NodeSource::Askama(current_askama)) => {
                if current_askama.is_expr() {
                    // Don't add space if text ends with punctuation
                    let ends_with_punct = prev
                        .content
                        .chars()
                        .last()
                        .is_some_and(|c| c.is_ascii_punctuation());
                    !ends_with_punct
                } else {
                    false
                }
            }
            // Askama control block followed by HTML element - add space
            (
                NodeSource::Askama(prev_askama),
                NodeSource::Html(HtmlNode::StartTag { .. } | HtmlNode::Void { .. }),
            ) => prev_askama.get_ctrl_tag().is_some(),
            // HTML end tag or Void element followed by text that starts with a letter - add space
            (
                NodeSource::Html(HtmlNode::EndTag { .. } | HtmlNode::Void { .. }),
                NodeSource::Html(HtmlNode::Text(_)),
            ) => current_content
                .chars()
                .next()
                .is_some_and(char::is_alphabetic),
            // Text followed by HTML start tag or void element - add space (like "a<a>" -> "a <a>")
            (
                NodeSource::Html(HtmlNode::Text(_)),
                NodeSource::Html(HtmlNode::StartTag { .. } | HtmlNode::Void { .. }),
            ) => !prev_content.ends_with(' '),
            _ => false,
        }
    } else {
        false
    }
}

fn wrap_inline_content(config: &Config, content: &str, indent_level: i32) -> String {
    let prefix = indent_for(config, indent_level);
    let available_width = config.max_line_length;

    let options = Options::new(available_width)
        .initial_indent(&prefix)
        .subsequent_indent(&prefix);

    wrap(content, &options).join("\n")
}

fn indent_for(config: &Config, indent_level: i32) -> String {
    " ".repeat(indent_level as usize * config.indent_size)
}
