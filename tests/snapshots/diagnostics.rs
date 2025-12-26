use std::path::Path;

#[macro_export]
macro_rules! diagnostic_snapshot {
    ($fixture_path:expr) => {
        use colored::control;

        control::set_override(false);

        let path = Path::new("tests/fixtures/diagnostics").join($fixture_path);
        let input = std::fs::read_to_string(&path)
            .unwrap_or_else(|_| panic!("Could not read fixture: {}", path.display()));

        let mut kirei = kirei::write::Kirei::default();
        let result = kirei.write(&input);

        match result {
            Ok(_) => panic!("Expected error but formatting succeeded for {}", $fixture_path),
            Err(err) => {
                let diagnostic = kirei::draw::draw(&err, &input, Some($fixture_path));

                insta::with_settings!({
                    description => input,
                    omit_expression => true,
                    prepend_module_to_snapshot => false,
                    snapshot_path => "output/diagnostics",
                }, {
                    insta::assert_snapshot!(diagnostic);
                });
            }
        }
    };
}

#[test]
fn erroneous_end_tag_inline() {
    diagnostic_snapshot!("erroneous_end_tag_inline.html");
}

#[test]
fn erroneous_end_tag_multiline() {
    diagnostic_snapshot!("erroneous_end_tag_multiline.html");
}

#[test]
fn invalid_ctrl_statement() {
    diagnostic_snapshot!("invalid_ctrl_statement.html");
}

#[test]
fn unclosed_exp_inside_attr() {
    diagnostic_snapshot!("unclosed_exp_inside_attr.html");
}
