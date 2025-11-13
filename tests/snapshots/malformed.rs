#[test]
fn unclosed_div() {
    format_and_snapshot!("malformed/unclosed_div.html");
}

#[test]
fn unclosed_block() {
    format_and_snapshot!("malformed/unclosed_block.html");
}

#[test]
fn unclosed_tags_in_conditional() {
    format_and_snapshot!("malformed/unclosed_tags_in_conditional.html");
}
