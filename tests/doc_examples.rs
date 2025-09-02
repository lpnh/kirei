use kirei::AskamaFormatter;

fn create_formatter() -> AskamaFormatter {
    AskamaFormatter::new().expect("Failed to create formatter")
}

#[test]
fn base_template_1() {
    let mut formatter = create_formatter();
    let input = r#"<!DOCTYPE html><html lang="en"><head><title>{% block title %}{{ title }} - My Site{% endblock %}</title>{% block head %}{% endblock %}</head><body><div id="content">{% block content %}<p>Placeholder content</p>{% endblock %}</div></body></html>"#;

    let result = formatter.format(input).expect("Formatting failed");

    insta::assert_snapshot!(result);
}

#[test]
fn for_1() {
    let mut formatter = create_formatter();
    let input =
        r#"<h1>Users</h1><ul>{% for user in users %}<li>{{ user.name }}</li>{% endfor %}</ul>"#;

    let result = formatter.format(input).expect("Formatting failed");

    insta::assert_snapshot!(result);
}

#[test]
fn comment_1() {
    let mut formatter = create_formatter();
    let input = r#"{# A Comment #}"#;

    let result = formatter.format(input).expect("Formatting failed");

    insta::assert_snapshot!(result);
}
