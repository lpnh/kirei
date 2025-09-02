use kirei::AskamaFormatter;

fn create_formatter() -> AskamaFormatter {
    AskamaFormatter::new().expect("Failed to create formatter")
}

#[test]
fn empty() {
    let mut formatter = create_formatter();
    let input = "";

    let result = formatter.format(input).expect("Formatting failed");

    insta::assert_snapshot!(result);
}

#[test]
fn hello_world() {
    let mut formatter = create_formatter();
    let input = r#"<p>Hello, world!</p>"#;

    let result = formatter.format(input).expect("Formatting failed");

    insta::assert_snapshot!(result);
}

#[test]
fn doctype() {
    let mut formatter = create_formatter();
    let input = r#"<!DOCTYPE html><title>Page Title</title>"#;

    let result = formatter.format(input).expect("Formatting failed");

    insta::assert_snapshot!(result);
}

#[test]
fn link() {
    let mut formatter = create_formatter();
    let input = r#"<a href="https://example.com">Visit us</a>"#;

    let result = formatter.format(input).expect("Formatting failed");

    insta::assert_snapshot!(result);
}

#[test]
fn image() {
    let mut formatter = create_formatter();
    let input = r#"<img src="image.jpg" alt="A test image">"#;

    let result = formatter.format(input).expect("Formatting failed");

    insta::assert_snapshot!(result);
}

#[test]
fn list() {
    let mut formatter = create_formatter();
    let input = r#"<ul><li>Item 1</li><li>Item 2</li></ul>"#;

    let result = formatter.format(input).expect("Formatting failed");

    insta::assert_snapshot!(result);
}

#[test]
fn form() {
    let mut formatter = create_formatter();
    let input = r#"<form action="/submit" method="post"><label for="name">Name:</label><input type="text" id="name"></form>"#;

    let result = formatter.format(input).expect("Formatting failed");

    insta::assert_snapshot!(result);
}

#[test]
fn bold() {
    let mut formatter = create_formatter();
    let input =
        r#"<div><h1>Main Heading</h1><p>Some content with <strong>bold text</strong>.</p></div>"#;

    let result = formatter.format(input).expect("Formatting failed");

    insta::assert_snapshot!(result);
}

#[test]
fn table() {
    let mut formatter = create_formatter();
    let input = r#"<table border="1"><thead><tr><th>Header 1</th></tr></thead><tbody><tr><td>Data 1</td></tr></tbody></table>"#;

    let result = formatter.format(input).expect("Formatting failed");

    insta::assert_snapshot!(result);
}

#[test]
fn container() {
    let mut formatter = create_formatter();
    let input = r#"<div class="container"><header><nav><ul><li><a href="/">Home</a></li></ul></nav></header><main><section><h2>Section Heading</h2><p>This is a paragraph.</p></section></main><footer><p>&copy; 2025</p></footer></div>"#;

    let result = formatter.format(input).expect("Formatting failed");

    insta::assert_snapshot!(result);
}
