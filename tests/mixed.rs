use kirei::formatter::AskamaFormatter;

#[test]
fn for_item() {
    let mut formatter = AskamaFormatter::default();
    let input =
        r"<h1>Users</h1><ul>{% for user in users %}<li>{{ user.name }}</li>{% endfor %}</ul>";

    let formatted_output = formatter.format(input).expect("Formatting failed");

    insta::assert_snapshot!(input);
    insta::assert_snapshot!(formatted_output);
}

#[test]
fn nesting_blocks() {
    let mut formatter = AskamaFormatter::default();

    let input =
        r"{% block first %}{% block inner %}<p>A cool paragraph</p>{% endblock %}{% endblock %}";

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

#[test]
fn head() {
    let mut formatter = AskamaFormatter::default();
    let input = r#"
{% block head %}
<style>
.user {
  color: blue;
  margin-bottom: 5px;
}
</style>
<script>
function showCount(count) {
  console.log("We have " + count + " users.");
}
document.addEventListener("DOMContentLoaded", () => {
  showCount({{ users | length }});
});
</script>
{% endblock %}
"#;

    let formatted_output = formatter.format(input).expect("Formatting failed");

    insta::assert_snapshot!(input);
    insta::assert_snapshot!(formatted_output);
}

#[test]
fn full_blown() {
    let mut formatter = AskamaFormatter::default();
    let input = r#"
{% extends "base.html" %}
{% block title %}Profile{% endblock %}
{% block head %}
{# Template comment: Styles specific to the profile page #}
<style>
/* CSS comment: highlight user name */
.username {
font-weight: bold;
color: {{ name_color }};
}
</style>
<script>
// JS comment: greet user on page load
document.addEventListener("DOMContentLoaded", () => {
alert("Welcome, {{ user.name }}!");
});
</script>
{% endblock %}
{% block content %}
<!-- HTML comment: Profile header -->
<h1 class="username">{{ user.name }}</h1>
{# Loop through user posts #}
<ul>
{% for post in user.posts %}
<li>{{ post.title }}</li>
{% endfor %}
</ul>
{% endblock %}
"#;

    let formatted_output = formatter.format(input).expect("Formatting failed");

    insta::assert_snapshot!(input);
    insta::assert_snapshot!(formatted_output);
}
