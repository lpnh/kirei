use kirei::AskamaFormatter;

#[test]
fn comment() {
    let mut formatter = AskamaFormatter::default();
    let input = r#"{# A Comment #}"#;

    let formatted_output = formatter.format(input).expect("Formatting failed");

    insta::assert_snapshot!(input);
    insta::assert_snapshot!(formatted_output);
}

#[test]
fn empty_if() {
    let mut formatter = AskamaFormatter::default();
    let input = r#"{% if foo %}{% endif %}"#;

    let formatted_output = formatter.format(input).expect("Formatting failed");

    insta::assert_snapshot!(input);
    insta::assert_snapshot!(formatted_output);
}

#[test]
fn if_with_text() {
    let mut formatter = AskamaFormatter::default();
    let input = r#"{% if foo %} this is some text {% endif %}"#;

    let formatted_output = formatter.format(input).expect("Formatting failed");

    insta::assert_snapshot!(input);
    insta::assert_snapshot!(formatted_output);
}

#[test]
fn simple() {
    let mut formatter = AskamaFormatter::default();
    let input = r#"{{ user.name }}{% for item in items %}{{ item }}{% endfor %}{# comment here #}"#;

    let formatted_output = formatter.format(input).expect("Formatting failed");

    insta::assert_snapshot!(input);
    insta::assert_snapshot!(formatted_output);
}
