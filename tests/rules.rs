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
