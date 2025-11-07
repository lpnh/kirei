#[test]
fn unclosed_paragraph() {
    format_and_snapshot!("malformed/unclosed_paragraph.html");
}

#[test]
fn unclosed_div() {
    format_and_snapshot!("malformed/unclosed_div.html");
}

#[test]
fn unclosed_attr_quote() {
    format_and_snapshot!("malformed/unclosed_attr_quote.html");
}

#[test]
fn mixed_unclosed_attr_quote() {
    format_and_snapshot!("malformed/mixed_unclosed_attr_quote.html");
}

#[test]
fn unclosed_block() {
    format_and_snapshot!("malformed/unclosed_block.html");
}

#[test]
fn invalid_block() {
    format_and_snapshot!("malformed/invalid_block.html");
}

#[test]
fn invalid_nested_block() {
    format_and_snapshot!("malformed/invalid_nested_block.html");
}
