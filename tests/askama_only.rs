use kirei::AskamaFormatter;

fn create_formatter() -> AskamaFormatter {
    AskamaFormatter::new().expect("Failed to create formatter")
}

#[test]
fn comment() {
    let mut formatter = create_formatter();
    let input = r#"{# A Comment #}"#;

    let formatted_output = formatter.format(input).expect("Formatting failed");

    insta::assert_snapshot!(input);
    insta::assert_snapshot!(formatted_output);
}

#[test]
fn empty_if() {
    let mut formatter = create_formatter();
    let input = r#"{% if foo %}{% endif %}"#;

    let formatted_output = formatter.format(input).expect("Formatting failed");

    insta::assert_snapshot!(input);
    insta::assert_snapshot!(formatted_output);
}

#[test]
fn if_with_text() {
    let mut formatter = create_formatter();
    let input = r#"{% if foo %} this is some text {% endif %}"#;

    let formatted_output = formatter.format(input).expect("Formatting failed");

    insta::assert_snapshot!(input);
    insta::assert_snapshot!(formatted_output);
}

#[test]
fn simple() {
    let mut formatter = create_formatter();
    let input = r#"{{ user.name }}{% for item in items %}{{ item }}{% endfor %}{# comment here #}"#;

    let formatted_output = formatter.format(input).expect("Formatting failed");

    insta::assert_snapshot!(input);
    insta::assert_snapshot!(formatted_output);
}
