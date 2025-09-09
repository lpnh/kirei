use kirei::formatter::AskamaFormatter;

#[test]
fn empty_comment() {
    let mut formatter = AskamaFormatter::default();
    let input = r"{#-   ~#}";

    let formatted_output = formatter.format(input).expect("Formatting failed");

    insta::assert_snapshot!(input);
    insta::assert_snapshot!(formatted_output);
}

#[test]
fn nested_comments() {
    let mut formatter = AskamaFormatter::default();
    let input = r"{#-   Some    {# Nested {#~   Comments #}   +#}    ~#}";

    let formatted_output = formatter.format(input).expect("Formatting failed");

    insta::assert_snapshot!(input);
    insta::assert_snapshot!(formatted_output);
}

#[test]
fn if_with_whitespace() {
    let mut formatter = AskamaFormatter::default();
    let input = r"{% if foo %}         {% endif %}";

    let formatted_output = formatter.format(input).expect("Formatting failed");

    insta::assert_snapshot!(input);
    insta::assert_snapshot!(formatted_output);
}

#[test]
fn simple_control() {
    let mut formatter = AskamaFormatter::default();

    let input = r"{%   if condition %}<p>Hello</p>{% endif    %}";

    let formatted_output = formatter.format(input).expect("Formatting failed");

    insta::assert_snapshot!(input);
    insta::assert_snapshot!(formatted_output);
}

#[test]
fn simple_expression() {
    let mut formatter = AskamaFormatter::default();
    let input = r"<div>{{user.name}}</div>";

    let formatted_output = formatter.format(input).expect("Formatting failed");

    insta::assert_snapshot!(input);
    insta::assert_snapshot!(formatted_output);
}

#[test]
fn whitespace_control() {
    let mut formatter = AskamaFormatter::default();

    let input = r"{% if foo ~%}{{- bar -}}{% else if another -%} nothing {%- endif %}";

    let formatted_output = formatter.format(input).expect("Formatting failed");

    insta::assert_snapshot!(input);
    insta::assert_snapshot!(formatted_output);
}

#[test]
fn complex_whitespace_control() {
    let mut formatter = AskamaFormatter::default();

    let input = r"{%     if foo ~%}{{-    bar -}}{% else if  another   -%} nothing {%-   endif %}";

    let formatted_output = formatter.format(input).expect("Formatting failed");

    insta::assert_snapshot!(input);
    insta::assert_snapshot!(formatted_output);
}

#[test]
fn with_html() {
    let mut formatter = AskamaFormatter::default();

    let input = r"{% if foo %} {{ bar }} {% else if another -%} <p>nothing   </p>{%- endif %}";

    let formatted_output = formatter.format(input).expect("Formatting failed");

    insta::assert_snapshot!(input);
    insta::assert_snapshot!(formatted_output);
}
