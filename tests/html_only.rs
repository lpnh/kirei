use kirei::formatter::AskamaFormatter;

#[test]
fn empty() {
    let mut formatter = AskamaFormatter::default();
    let input = "";

    let formatted_output = formatter.format(input).expect("Formatting failed");

    insta::assert_snapshot!(input);
    insta::assert_snapshot!(formatted_output);
}

#[test]
fn hello_world() {
    let mut formatter = AskamaFormatter::default();
    let input = r"<p>Hello, world!</p>";

    let formatted_output = formatter.format(input).expect("Formatting failed");

    insta::assert_snapshot!(input);
    insta::assert_snapshot!(formatted_output);
}

#[test]
fn hello_world_with_newline() {
    let mut formatter = AskamaFormatter::default();
    let input = r"
        <p>
        Hello,     
    world!
  </p>
";

    let formatted_output = formatter.format(input).expect("Formatting failed");

    insta::assert_snapshot!(input);
    insta::assert_snapshot!(formatted_output);
}

#[test]
fn hello_with_div() {
    let mut formatter = AskamaFormatter::default();
    let input = r"<div><p>Hello, world!</p></div>";

    let formatted_output = formatter.format(input).expect("Formatting failed");

    insta::assert_snapshot!(input);
    insta::assert_snapshot!(formatted_output);
}

#[test]
fn entity() {
    let mut formatter = AskamaFormatter::default();
    let input = r"
        <p>&copy;   
    2025</p>
";

    let formatted_output = formatter.format(input).expect("Formatting failed");

    insta::assert_snapshot!(input);
    insta::assert_snapshot!(formatted_output);
}

#[test]
fn doctype() {
    let mut formatter = AskamaFormatter::default();
    let input = r"<!DOCTYPE html><title>Page Title</title>";

    let formatted_output = formatter.format(input).expect("Formatting failed");

    insta::assert_snapshot!(input);
    insta::assert_snapshot!(formatted_output);
}

#[test]
fn link() {
    let mut formatter = AskamaFormatter::default();
    let input = r#"<a href="https://example.com">Visit us</a>"#;

    let formatted_output = formatter.format(input).expect("Formatting failed");

    insta::assert_snapshot!(input);
    insta::assert_snapshot!(formatted_output);
}

#[test]
fn image() {
    let mut formatter = AskamaFormatter::default();
    let input = r#"<img src="image.jpg" alt="A test image">"#;

    let formatted_output = formatter.format(input).expect("Formatting failed");

    insta::assert_snapshot!(input);
    insta::assert_snapshot!(formatted_output);
}

#[test]
fn list() {
    let mut formatter = AskamaFormatter::default();
    let input = r"<ul><li>Item 1</li><li>Item 2</li></ul>";

    let formatted_output = formatter.format(input).expect("Formatting failed");

    insta::assert_snapshot!(input);
    insta::assert_snapshot!(formatted_output);
}

#[test]
fn form() {
    let mut formatter = AskamaFormatter::default();
    let input = r#"<form action="/submit" method="post"><label for="name">Name:</label><input type="text" id="name"></form>"#;

    let formatted_output = formatter.format(input).expect("Formatting failed");

    insta::assert_snapshot!(input);
    insta::assert_snapshot!(formatted_output);
}

#[test]
fn bold() {
    let mut formatter = AskamaFormatter::default();
    let input =
        r"<div><h1>Main Heading</h1><p>Some content with <strong>bold text</strong>.</p></div>";

    let formatted_output = formatter.format(input).expect("Formatting failed");

    insta::assert_snapshot!(input);
    insta::assert_snapshot!(formatted_output);
}

#[test]
fn table() {
    let mut formatter = AskamaFormatter::default();
    let input = r#"<table border="1"><thead><tr><th>Header 1</th></tr></thead><tbody><tr><td>Data 1</td></tr></tbody></table>"#;

    let formatted_output = formatter.format(input).expect("Formatting failed");

    insta::assert_snapshot!(input);
    insta::assert_snapshot!(formatted_output);
}

#[test]
fn container() {
    let mut formatter = AskamaFormatter::default();
    let input = r#"<div class="container"><header><nav><ul><li><a href="/">Home</a></li></ul></nav></header><main><section><h2>Section Heading</h2><p>This is a paragraph.</p></section></main><footer><p>&copy; 2025</p></footer></div>"#;

    let formatted_output = formatter.format(input).expect("Formatting failed");

    insta::assert_snapshot!(input);
    insta::assert_snapshot!(formatted_output);
}

#[test]
fn void_one() {
    let mut formatter = AskamaFormatter::default();
    let input = r#"<div>Text<img src="image.jpg" ><br />Some text<div> More text</div></div>"#;

    let formatted_output = formatter.format(input).expect("Formatting failed");

    insta::assert_snapshot!(input);
    insta::assert_snapshot!(formatted_output);
}

#[test]
fn void_two() {
    let mut formatter = AskamaFormatter::default();
    let input = r#"<div>Text<div>More text</div><img src="image.jpg" >Some text<br /></div>"#;

    let formatted_output = formatter.format(input).expect("Formatting failed");

    insta::assert_snapshot!(input);
    insta::assert_snapshot!(formatted_output);
}
