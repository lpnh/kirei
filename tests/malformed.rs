use kirei::formatter::AskamaFormatter;

#[test]
fn hello() {
    let mut formatter = AskamaFormatter::default();
    let input = r"<p>Hello!";

    let formatted_output = formatter.format(input).expect("Formatting failed");

    insta::assert_snapshot!(input);
    insta::assert_snapshot!(formatted_output);
}

#[test]
fn missing_div() {
    let mut formatter = AskamaFormatter::default();
    let input = r" <main> <div> <h2>Test</h2> <p> Where did that div go </p> </main>";

    let formatted_output = formatter.format(input).expect("Formatting failed");

    insta::assert_snapshot!(input);
    insta::assert_snapshot!(formatted_output);
}

#[test]
fn missing_endblock() {
    let mut formatter = AskamaFormatter::default();
    let input = r"{% block paragraph %}<p>A cool paragraph</p>";

    let formatted_output = formatter.format(input).expect("Formatting failed");

    insta::assert_snapshot!(input);
    insta::assert_snapshot!(formatted_output);
}
