use crate::types::*;

pub(crate) fn is_askama_token(text: &str) -> bool {
    text.starts_with(ASKAMA_EXPR_TOKEN)
        || text.starts_with(ASKAMA_CTRL_TOKEN)
        || text.starts_with(ASKAMA_COMMENT_TOKEN)
}

pub(crate) fn collect_placeholder_indices(s: &str) -> Vec<usize> {
    let mut indices = Vec::new();
    let mut pos = 0;
    let text_len = s.len();

    // Scan through the text and find all our placeholders
    while pos < text_len {
        // Look ahead for any of our three token types and pick the closest
        let expr_match = s[pos..]
            .find(ASKAMA_EXPR_TOKEN)
            .map(|p| (pos + p, ASKAMA_EXPR_TOKEN));
        let ctrl_match = s[pos..]
            .find(ASKAMA_CTRL_TOKEN)
            .map(|p| (pos + p, ASKAMA_CTRL_TOKEN));
        let comm_match = s[pos..]
            .find(ASKAMA_COMMENT_TOKEN)
            .map(|p| (pos + p, ASKAMA_COMMENT_TOKEN));

        // Gather all the candidates we found
        let candidates: Vec<_> = [expr_match, ctrl_match, comm_match]
            .into_iter()
            .flatten()
            .collect();

        if candidates.is_empty() {
            break;
        }

        // Pick the earliest one
        let (start_pos, token_prefix) = candidates.into_iter().min_by_key(|(pos, _)| *pos).unwrap();

        // Find where this placeholder ends
        if let Some(relative_end) = s[start_pos..].find(ASKAMA_END_TOKEN) {
            let end_pos = start_pos + relative_end + ASKAMA_END_TOKEN.len();

            // Extract what's between the opening and closing markers
            let content_start = start_pos + token_prefix.len();
            let content_end = start_pos + relative_end;

            if content_end > content_start {
                let content = &s[content_start..content_end];

                // Try to find a valid placeholder
                if let Ok(index) = content.parse::<usize>() {
                    indices.push(index);
                }
            }

            // Move past this placeholder and keep looking
            pos = end_pos;
        } else {
            break; // Probably an opening but no closing marker
        }
    }

    indices
}

pub(crate) fn is_block_control(tag: &crate::types::ControlTag) -> bool {
    use crate::types::ControlTag::*;
    match tag {
        // Control tags that define blocks, conditions, loops, etc.
        Open | Middle | Close => true,

        // 'let', 'include', etc. are more like inline statements
        _ => false,
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

// Indentation adjustments based on template control tag
fn block_indent_adjustments(token: &str, placeholders: &[AskamaNode]) -> (i32, i32) {
    let placeholder = get_placeholder_for_indent(token, placeholders);

    match placeholder {
        Some(AskamaNode::Control { tag_type, .. }) => match tag_type {
            ControlTag::Open => (0, 1),    // indent after opening block
            ControlTag::Middle => (-1, 0), // outdent before middle block
            ControlTag::Close => (-1, 0),  // outdent before closing block
            ControlTag::Other => (0, 0),   // no indentation change
        },
        _ => (0, 0), // expressions, comments, and plain text
    }
}

// Extract placeholder metadata for indentation calculation
fn get_placeholder_for_indent<'a>(
    token: &str,
    placeholders: &'a [AskamaNode],
) -> Option<&'a AskamaNode> {
    let token = token.trim();

    // Check each token type prefix to find the placeholder index
    for prefix in &[ASKAMA_EXPR_TOKEN, ASKAMA_CTRL_TOKEN, ASKAMA_COMMENT_TOKEN] {
        if let Some(rest) = token.strip_prefix(prefix)
            && let Some(idx_str) = rest.strip_suffix(ASKAMA_END_TOKEN)
            && let Ok(idx) = idx_str.parse::<usize>()
        {
            return placeholders.get(idx);
        }
    }
    None
}

// Return the tag type
pub(crate) fn get_tag_type(child: tree_sitter::Node) -> ControlTag {
    if let Some(grand_child) = child.child(1) {
        // Control tag type based on its node type
        return match grand_child.kind() {
            // Opening tags - start a new block
            "if_statement" | "for_statement" | "block_statement" | "filter_statement"
            | "match_statement" | "macro_statement" | "call_statement" => ControlTag::Open,
            // Middle tags - continue within a block
            "else_statement" | "else_if_statement" | "when_statement" => ControlTag::Middle,
            // Closing tags - end a block
            "endif_statement"
            | "endfor_statement"
            | "endblock_statement"
            | "endfilter_statement"
            | "endmatch_statement"
            | "endmacro_statement"
            | "endcall_statement" => ControlTag::Close,
            // Other tags like 'let', 'include', etc
            _ => ControlTag::Other,
        };
    }

    // unreachable?
    ControlTag::Other
}

// Extract content between delimiters, handling trim markers
pub(crate) fn extract_inner_content(content: &str, open_delim: &str, close_delim: &str) -> String {
    // Clean up the content by removing the delimiters and any trim markers
    let mut inner = content.trim();

    // Strip the opening delimiter
    if inner.starts_with(open_delim) {
        inner = &inner[open_delim.len()..];
        // Check for trim characters (-, +, ~) right after the opening delimiter
        if let Some(first_char) = inner.chars().next()
            && (first_char == '-' || first_char == '+' || first_char == '~')
        {
            let ch_len = first_char.len_utf8();
            inner = &inner[ch_len..];
        }
    }

    // Strip the closing delimiter
    if inner.ends_with(close_delim) {
        inner = &inner[..inner.len() - close_delim.len()];
        // Check for trim characters right before the closing delimiter
        if let Some(last_char) = inner.chars().next_back()
            && (last_char == '-' || last_char == '+' || last_char == '~')
        {
            let ch_len = last_char.len_utf8();
            inner = &inner[..inner.len() - ch_len];
        }
    }

    inner.to_string()
}
