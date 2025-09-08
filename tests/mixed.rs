use kirei::formatter::AskamaFormatter;

#[test]
fn for_item() {
    let mut formatter = AskamaFormatter::default();
    let input =
        r#"<h1>Users</h1><ul>{% for user in users %}<li>{{ user.name }}</li>{% endfor %}</ul>"#;

    let formatted_output = formatter.format(input).expect("Formatting failed");

    insta::assert_snapshot!(input);
    insta::assert_snapshot!(formatted_output);
}

#[test]
fn nesting_blocks() {
    let mut formatter = AskamaFormatter::default();

    let input =
        r#"{% block first %}{% block inner %}<p>A cool paragraph</p>{% endblock %}{% endblock %}"#;

    let formatted_output = formatter.format(input).expect("Formatting failed");

    insta::assert_snapshot!(input);
    insta::assert_snapshot!(formatted_output);
}

#[test]
fn mixed() {
    let mut formatter = AskamaFormatter::default();

    let input = r#"<div id="content"> {% block content %} <p>Oh, this is a paragraph</p> {% endblock %} </div>"#;

    let formatted_output = formatter.format(input).expect("Formatting failed");

    insta::assert_snapshot!(input);
    insta::assert_snapshot!(formatted_output);
}

#[test]
fn nesting_macros() {
    let mut formatter = AskamaFormatter::default();
    let input = r#"{% macro container() %}<div class="container">{{ caller() }}</div>{% endmacro %}{% macro outer_container() %}{# Create an alias to our `caller`, so we can access it within container: #}{% set outer_caller = caller %}<div class="outer-container">{# nested macro invocation - will overwrite the `caller` variable: #}{% call container() %}{{ outer_caller() }}{% endcall %}</div>{% endmacro %}"#;

    let formatted_output = formatter.format(input).expect("Formatting failed");

    insta::assert_snapshot!(input);
    insta::assert_snapshot!(formatted_output);
}

#[test]
fn base_template() {
    let mut formatter = AskamaFormatter::default();
    let input = r#"<!DOCTYPE html><html lang="en"><head><title>{% block title %}{{ title }} - My Site{% endblock %}</title>{% block head %}{% endblock %}</head><body><div id="content">{% block content %}<p>Placeholder content</p>{% endblock %}</div></body></html>"#;

    let formatted_output = formatter.format(input).expect("Formatting failed");

    insta::assert_snapshot!(input);
    insta::assert_snapshot!(formatted_output);
}
