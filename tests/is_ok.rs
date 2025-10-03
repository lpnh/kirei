use kirei::AskamaFormatter;

#[test]
fn unusual_utf8_characters() {
    let mut formatter = AskamaFormatter::default();
    let input = "\u{FFFD}\u{0000}";
    let result = formatter.format(input);

    assert!(result.is_ok());
}

#[test]
fn binary_and_control_characters() {
    let mut formatter = AskamaFormatter::default();
    let input = "\0\0\0\x01\x02\x03";
    let result = formatter.format(input);

    assert!(result.is_ok());
}

#[test]
fn empty_and_whitespace_input() {
    let mut formatter = AskamaFormatter::default();
    let inputs = ["", " ", "\n\n", "\t\t\t", " \t \n \t \n"];

    for input in inputs {
        let result = formatter.format(input);
        assert!(result.is_ok());
    }
}

#[test]
fn malformed_syntax() {
    let mut formatter = AskamaFormatter::default();
    let cases = [
        "<div",
        "<div<div>",
        "</div>",
        "<>",
        "< div >",
        "<div class=>",
        "{{% %}}",
        "{{% %}",
        "{% %}}",
        "{{ }}",
        "{ }}",
        "{{ }",
        "{% block foo %} no endblock",
        "no_block {% endblock %}",
        "{% if true %} no endif",
        "no_if {% endif %}",
        "{% for x in y %} no endfor",
        "no_for {% endfor %}",
        "{% block unclosed_block",
        "block unopened_block %}",
        "{{ unclosed_exp",
        "unopened_exp }}",
        "{# unclosed comment",
        "{% if true %}<div endif %}",
        "<div>{% for",
        "{{ exp >",
        "<div }",
        "<{}>",
        "{<>}",
        "{% block >",
        "<div %}",
    ];

    for input in cases {
        let result = formatter.format(input);
        assert!(result.is_ok());
    }
}
