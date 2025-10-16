#[test]
fn unclosed_paragraph() {
    format_and_snapshot!("malformed/unclosed_paragraph.html");
}

#[test]
fn unclosed_div() {
    format_and_snapshot!("malformed/unclosed_div.html");
}

#[test]
fn unclosed_block() {
    format_and_snapshot!("malformed/unclosed_block.html");
}
