#[macro_export]
macro_rules! format_and_snapshot {
    ($fixture_path:expr) => {
        let path = std::path::Path::new("tests/fixtures").join($fixture_path);
        let input = std::fs::read_to_string(&path).unwrap_or_else(|_| panic!("Could not read fixture: {}", path.display()));
        let mut formatter = kirei::formatter::AskamaFormatter::default();
        let formatted = formatter.format(&input).expect("Formatting failed");

        insta::with_settings!({
            omit_expression => true,
        }, {
            insta::assert_snapshot!(formatted);
        });
    };
}
