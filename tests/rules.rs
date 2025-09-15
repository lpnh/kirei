mod helper;

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
fn tmpl_empty_comment() {
    format_and_snapshot!("rules/whitespace/tmpl_empty_comment.html");
}

// === Delimiter Padding

#[test]
fn no_padding_expression() {
    format_and_snapshot!("rules/delimiter_padding/no_padding_expression.html");
}
