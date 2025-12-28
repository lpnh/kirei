#[macro_export]
macro_rules! snapshot_test {
    ($fixture_path:expr) => {
        use colored::control;

        control::set_override(false);

        let path = std::path::Path::new("tests/fixtures").join($fixture_path);
        let input = std::fs::read_to_string(&path)
            .unwrap_or_else(|_| panic!("Could not read fixture: {}", path.display()));

        let mut kirei = kirei::write::Kirei::default();
        let noted = kirei.write(&input);

        if noted.diagnostics.iter().all(|d| d.level != kirei::Severity::Error) {
            insta::with_settings!({
                description => input.clone(),
                omit_expression => true,
                prepend_module_to_snapshot => false,
                snapshot_path => "output",
            }, {
                insta::assert_snapshot!(noted.value);
            });
        }

        if !noted.diagnostics.is_empty() {
            let mut diagnostic_output = String::new();
            for diagnostic in &noted.diagnostics {
                diagnostic_output.push_str(&kirei::draw::Diagnostic::draw(diagnostic, &input, Some($fixture_path)));
                diagnostic_output.push_str("\n");
            }

            insta::with_settings!({
                description => input,
                omit_expression => true,
                prepend_module_to_snapshot => false,
                snapshot_path => "output/diagnostics",
            }, {
                insta::assert_snapshot!(diagnostic_output);
            });
        }
    };
}

#[macro_export]
macro_rules! format_and_snapshot {
    ($fixture_path:expr) => {
        snapshot_test!($fixture_path)
    };
}

#[macro_export]
macro_rules! diagnostic_snapshot {
    ($fixture_path:expr) => {
        snapshot_test!($fixture_path)
    };
}

mod askama;
mod axum_app;
mod diagnostics;
mod html;
mod ice_cream_club;
mod malformed;
mod mixed;
mod phrasing;
mod rules;
mod teapot;
