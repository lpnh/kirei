#[test]
fn for_list() {
    format_and_snapshot!("mixed/for_list.html");
}

#[test]
fn nested_blocks() {
    format_and_snapshot!("mixed/nested_blocks.html");
}

#[test]
fn block_surrounded_by_html() {
    format_and_snapshot!("mixed/block_surrounded_by_html.html");
}

#[test]
fn nesting_macros() {
    format_and_snapshot!("mixed/nesting_macros.html");
}

#[test]
fn base_template() {
    format_and_snapshot!("mixed/base_template.html");
}

#[test]
fn head() {
    format_and_snapshot!("mixed/head.html");
}

#[test]
fn full_blown() {
    format_and_snapshot!("mixed/full_blown.html");
}

#[test]
fn simple_template() {
    format_and_snapshot!("mixed/simple_template.html");
}
