// === Whitespace

#[test]
fn html_trailing_space() {
    format_and_snapshot!("rules/whitespace/html_trailing_space.html");
}

#[test]
fn html_blank_line() {
    format_and_snapshot!("rules/whitespace/html_blank_line.html");
}

#[test]
fn html_entity() {
    format_and_snapshot!("rules/whitespace/html_entity.html");
}

#[test]
fn tmpl_trailing_space() {
    format_and_snapshot!("rules/whitespace/tmpl_trailing_space.html");
}

#[test]
fn tmpl_blank_line() {
    format_and_snapshot!("rules/whitespace/tmpl_blank_line.html");
}

#[test]
fn tmpl_empty_comment() {
    format_and_snapshot!("rules/whitespace/tmpl_empty_comment.html");
}

#[test]
fn tmpl_nested_comment() {
    format_and_snapshot!("rules/whitespace/tmpl_nested_comment.html");
}

#[test]
fn indented_multiline_comment() {
    format_and_snapshot!("rules/whitespace/indented_multiline_comment.html");
}

#[test]
fn mixed_text_sequence() {
    format_and_snapshot!("rules/whitespace/mixed_text_sequence.html");
}

#[test]
fn mixed_no_whitespace_between() {
    format_and_snapshot!("rules/whitespace/mixed_no_whitespace_between.html");
}

// === Delimiter Padding

#[test]
fn no_padding_expression() {
    format_and_snapshot!("rules/delimiter_padding/no_padding_expression.html");
}

// === Wrapping

#[test]
fn lorem_ipsum() {
    format_and_snapshot!("rules/wrapping/lorem_ipsum.html");
}

#[test]
fn html_lorem_ipsum() {
    format_and_snapshot!("rules/wrapping/html_lorem_ipsum.html");
}

#[test]
fn tmpl_lorem_ipsum() {
    format_and_snapshot!("rules/wrapping/tmpl_lorem_ipsum.html");
}

#[test]
fn lorem_ipsum_newline() {
    format_and_snapshot!("rules/wrapping/lorem_ipsum_newline.html");
}

#[test]
fn html_lorem_ipsum_newline() {
    format_and_snapshot!("rules/wrapping/html_lorem_ipsum_newline.html");
}

#[test]
fn tmpl_lorem_ipsum_newline() {
    format_and_snapshot!("rules/wrapping/tmpl_lorem_ipsum_newline.html");
}

// === Collapsing

#[test]
fn empty_block() {
    format_and_snapshot!("rules/collapsing/empty_block.html");
}

#[test]
fn empty_if() {
    format_and_snapshot!("rules/collapsing/empty_if.html");
}

#[test]
fn block_with_whitespace() {
    format_and_snapshot!("rules/collapsing/block_with_whitespace.html");
}

#[test]
fn block_with_newline() {
    format_and_snapshot!("rules/collapsing/block_with_newline.html");
}

// === Quote

#[test]
fn attr_single_quote() {
    format_and_snapshot!("rules/quote/attr_single_quote.html");
}
