use crate::types::*;

pub(crate) fn is_askama_token(text: &str) -> bool {
    text.starts_with(ASKAMA_EXPR_TOKEN)
        || text.starts_with(ASKAMA_CTRL_TOKEN)
        || text.starts_with(ASKAMA_COMMENT_TOKEN)
}

// Calculate indentation adjustments for a line based on control tags
pub(crate) fn calculate_indent_adjustments(line: &str, placeholders: &[AskamaNode]) -> (i32, i32) {
    let mut pre_adjust = 0;
    let mut post_adjust = 0;

    // First, check if we have control placeholders with metadata we can use
    for (i, placeholder) in placeholders.iter().enumerate() {
        if let AskamaNode::Control { tag_type, .. } = placeholder {
            let marker = format!("{}{}{}", ASKAMA_CTRL_TOKEN, i, ASKAMA_EXPR_TOKEN);
            if line.contains(&marker) {
                match tag_type {
                    ControlTag::Open => post_adjust += 1,
                    ControlTag::Close => pre_adjust -= 1,
                    ControlTag::Middle => {
                        // Middle tags like 'else' - outdent before printing,
                        // but don't increase indent after
                        pre_adjust -= 1;
                    }
                    ControlTag::Other => {}
                }
                // Keep scanning - there might be multiple control tags on one line
            }
        }
    }

    // If we didn't find any placeholder metadata, parse the line content directly
    if pre_adjust == 0 && post_adjust == 0 {
        let t = line.trim();

        if t.starts_with(CTRL_OPEN) || t.starts_with(CTRL_OPEN_TRIM) {
            let tag_type = get_tag_type(t);

            match tag_type {
                ControlTag::Open => post_adjust += 1,
                ControlTag::Close => pre_adjust -= 1,
                ControlTag::Middle => pre_adjust -= 1,
                ControlTag::Other => {}
            }
        }
    }

    (pre_adjust, post_adjust)
}

// Returns the tag type
pub(crate) fn get_tag_type(tag_content: &str) -> ControlTag {
    let inner = extract_inner_content(tag_content, CTRL_OPEN, CTRL_CLOSE);
    let first_word = inner.split_whitespace().next().unwrap_or("");

    // Control tag type based on the first keyword
    match first_word {
        // Opening tags - start a new block
        "if" | "for" | "block" | "filter" | "match" | "macro" | "call" => ControlTag::Open,
        // Middle tags - continue within a block
        "else" | "elif" | "else if" | "when" => ControlTag::Middle,
        // Closing tags - end a block
        "endif" | "endfor" | "endblock" | "endfilter" | "endmatch" | "endmacro" | "endcall" => {
            ControlTag::Close
        }
        // Other tags like 'let', 'include', etc
        _ => ControlTag::Other,
    }
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
