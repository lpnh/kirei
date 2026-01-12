#[test]
fn match_else_inline() {
    format_and_snapshot!("match_blocks/match_else_inline.html");
}

#[test]
fn match_else_multiline() {
    format_and_snapshot!("match_blocks/match_else_multiline.html");
}

#[test]
fn match_text_inline() {
    format_and_snapshot!("match_blocks/match_text_inline.html");
}

#[test]
fn match_text_multiline() {
    format_and_snapshot!("match_blocks/match_text_multiline.html");
}
