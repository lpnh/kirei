use kirei::AskamaFormatter;

fn create_formatter() -> AskamaFormatter {
    AskamaFormatter::new().expect("Failed to create formatter")
}

#[test]
fn if_with_whitespace() {
    let mut formatter = create_formatter();
    let input = r#"{% if foo %}         {% endif %}"#;

    let formatted_output = formatter.format(input).expect("Formatting failed");

    insta::assert_snapshot!(input);
    insta::assert_snapshot!(formatted_output);
}

#[test]
fn simple_control() {
    let mut formatter = create_formatter();

    let input = r#"{%   if condition %}<p>Hello</p>{% endif    %}"#;

    let formatted_output = formatter.format(input).expect("Formatting failed");

    insta::assert_snapshot!(input);
    insta::assert_snapshot!(formatted_output);
}

#[test]
fn simple_expression() {
    let mut formatter = create_formatter();
    let input = r#"<div>{{user.name}}</div>"#;

    let formatted_output = formatter.format(input).expect("Formatting failed");

    insta::assert_snapshot!(input);
    insta::assert_snapshot!(formatted_output);
}

#[test]
fn whitespace_control() {
    let mut formatter = create_formatter();

    let input = r#"{% if foo ~%}{{- bar -}}{% else if another -%} nothing {%- endif %}"#;

    let formatted_output = formatter.format(input).expect("Formatting failed");

    insta::assert_snapshot!(input);
    insta::assert_snapshot!(formatted_output);
}

#[test]
fn complex_whitespace_control() {
    let mut formatter = create_formatter();

    let input =
        r#"{%     if foo ~%}{{-    bar -}}{% else if  another   -%} nothing {%-   endif %}"#;

    let formatted_output = formatter.format(input).expect("Formatting failed");

    insta::assert_snapshot!(input);
    insta::assert_snapshot!(formatted_output);
}

#[test]
fn with_html() {
    let mut formatter = create_formatter();

    let input = r#"{% if foo %} {{ bar }} {% else if another -%} <p>nothing   </p>{%- endif %}"#;

    let formatted_output = formatter.format(input).expect("Formatting failed");

    insta::assert_snapshot!(input);
    insta::assert_snapshot!(formatted_output);
}
