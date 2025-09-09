use kirei::formatter::AskamaFormatter;

#[test]
fn empty_block() {
    let mut formatter = AskamaFormatter::default();
    let input = r"{% block content %}{% endblock %}";

    let formatted_output = formatter.format(input).expect("Formatting failed");

    insta::assert_snapshot!(input);
    insta::assert_snapshot!(formatted_output);
}

#[test]
fn empty_block_with_whitespace() {
    let mut formatter = AskamaFormatter::default();
    let input = r"
{% block content %}      {% endblock %}
";

    let formatted_output = formatter.format(input).expect("Formatting failed");

    insta::assert_snapshot!(input);
    insta::assert_snapshot!(formatted_output);
}

#[test]
fn empty_block_with_whitespace_and_html() {
    let mut formatter = AskamaFormatter::default();
    let input = r"
<div>
{%~ block content ~%}     {%~ endblock ~%}
</div>
";

    let formatted_output = formatter.format(input).expect("Formatting failed");

    insta::assert_snapshot!(input);
    insta::assert_snapshot!(formatted_output);
}
