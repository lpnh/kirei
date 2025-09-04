use crate::types::*;

pub(crate) fn is_askama_token(text: &str) -> bool {
    text.starts_with(ASKAMA_CTRL_TOKEN)
        || text.starts_with(ASKAMA_EXPR_TOKEN)
        || text.starts_with(ASKAMA_COMMENT_TOKEN)
}

pub(crate) fn collect_placeholder_indices(s: &str) -> Vec<usize> {
    let mut indices = Vec::new();
    let mut pos = 0;
    let text_len = s.len();

    while pos < text_len {
        // Find the earliest token occurrence
        let mut earliest_match: Option<(usize, &str)> = None;

        for &token_prefix in TOKEN_PREFIXES {
            if let Some(found_pos) = s[pos..].find(token_prefix) {
                let absolute_pos = pos + found_pos;
                if earliest_match.is_none_or(|(best_pos, _)| absolute_pos < best_pos) {
                    earliest_match = Some((absolute_pos, token_prefix));
                }
            }
        }

        let Some((start_pos, token_prefix)) = earliest_match else {
            break;
        };

        // Find the matching end token
        let search_start = start_pos + token_prefix.len();
        if let Some(relative_end) = s[search_start..].find(ASKAMA_END_TOKEN) {
            let content_start = search_start;
            let content_end = search_start + relative_end;

            // Parse the index from the content between markers
            if content_end > content_start {
                let content = &s[content_start..content_end];
                if let Ok(index) = content.parse::<usize>() {
                    indices.push(index);
                }
            }

            pos = content_end + ASKAMA_END_TOKEN.len();
        } else {
            // No matching end token found
            pos = search_start;
        }
    }

    // Remove duplicates and sort for consistent ordering
    indices.sort_unstable();
    indices.dedup();
    indices
}

// Calculate indentation adjustments based on block semantics
pub(crate) fn block_indent_adjustments(token: &str, placeholders: &[AskamaNode]) -> (i32, i32) {
    if let Some(placeholder) = get_placeholder_for_indent(token, placeholders) {
        let indentation = match placeholder {
            AskamaNode::Control { style, .. }
            | AskamaNode::Expression { style, .. }
            | AskamaNode::Comment { style, .. } => style.indentation(),
        };
        (indentation.0 as i32, indentation.1 as i32)
    } else {
        (0, 0) // No indentation change for non-placeholder tokens
    }
}

// Format template element with semantic indentation
pub(crate) fn format_template_block(
    token: &str,
    indent_level: &mut i32,
    indent_size: usize,
    placeholders: &[AskamaNode],
    mut output_handler: impl FnMut(&str),
) {
    let token_trimmed = token.trim();
    if token_trimmed.is_empty() {
        return;
    }

    // Calculate how the block indentation should change
    let (pre_adjust, post_adjust) = block_indent_adjustments(token_trimmed, placeholders);

    // Adjust indent before rendering this element
    *indent_level = (*indent_level + pre_adjust).max(0);

    let indent = " ".repeat(*indent_level as usize * indent_size);

    // Render the template element with current indentation
    if is_askama_token(token_trimmed) {
        output_handler(&format!("{}{}", indent, token_trimmed));
    } else {
        // Handle multi-line plain text content
        for line in token_trimmed.lines() {
            let trimmed = line.trim();
            if !trimmed.is_empty() {
                output_handler(&format!("{}{}", indent, trimmed));
            }
        }
    }

    // Adjust indent after rendering this element
    *indent_level = (*indent_level + post_adjust).max(0);
}

// Extract placeholder metadata for indentation calculation
fn get_placeholder_for_indent<'a>(
    token: &str,
    placeholders: &'a [AskamaNode],
) -> Option<&'a AskamaNode> {
    let token = token.trim();

    // Check each token type prefix to find the placeholder index
    for &prefix in &[ASKAMA_EXPR_TOKEN, ASKAMA_CTRL_TOKEN, ASKAMA_COMMENT_TOKEN] {
        if let Some(rest) = token.strip_prefix(prefix)
            && let Some(idx_str) = rest.strip_suffix(ASKAMA_END_TOKEN)
            && let Ok(idx) = idx_str.parse::<usize>()
        {
            return placeholders.get(idx);
        }
    }
    None
}

// Extract content between delimiters, handling trim markers
pub(crate) fn extract_inner_content(content: &str, open_delim: &str, close_delim: &str) -> String {
    let mut inner = content.trim();

    // Strip the opening delimiter
    if let Some(after_open) = inner.strip_prefix(open_delim) {
        inner = after_open;
        // Check for trim characters (-, +, ~) right after the opening delimiter
        if let Some(first_char) = inner.chars().next()
            && matches!(first_char, '-' | '+' | '~')
        {
            inner = &inner[first_char.len_utf8()..];
        }
    }

    // Strip the closing delimiter
    if let Some(before_close) = inner.strip_suffix(close_delim) {
        inner = before_close;
        // Check for trim characters right before the closing delimiter
        if let Some(last_char) = inner.chars().next_back()
            && matches!(last_char, '-' | '+' | '~')
        {
            inner = &inner[..inner.len() - last_char.len_utf8()];
        }
    }

    inner.to_string()
}

// Helper to determine if a placeholder should be formatted inline
pub(crate) fn should_format_inline(placeholder: &AskamaNode) -> bool {
    match placeholder {
        AskamaNode::Control { style, .. }
        | AskamaNode::Expression { style, .. }
        | AskamaNode::Comment { style, .. } => matches!(style, Style::Inline),
    }
}

// Enhanced placeholder validation
pub(crate) fn validate_placeholder_indices(
    text: &str,
    placeholders: &[AskamaNode],
) -> Result<(), String> {
    let indices = collect_placeholder_indices(text);

    for &idx in &indices {
        if idx >= placeholders.len() {
            return Err(format!(
                "Placeholder index {} out of bounds (max: {})",
                idx,
                placeholders.len().saturating_sub(1)
            ));
        }
    }

    Ok(())
}
