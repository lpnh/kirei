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
fn tmpl_trailing_space() {
    format_and_snapshot!("rules/whitespace/tmpl_trailing_space.html");
}

#[test]
fn entity() {
    format_and_snapshot!("rules/whitespace/entity.html");
}
