#[macro_export]
macro_rules! snapshot_test {
    ($fixture_path:expr) => {
        use kirei::session::Session;

        let path = std::path::Path::new("tests/fixtures").join($fixture_path);
        let input = std::fs::read_to_string(&path)
            .unwrap_or_else(|_| panic!("Could not read fixture: {}", path.display()));

        let mut session = Session::default();
        let output = session.format(&input, $fixture_path);

        if let Some(formatted) = &output {
            insta::with_settings!({
                description => input.clone(),
                omit_expression => true,
                prepend_module_to_snapshot => false,
                snapshot_path => "output",
            }, {
                insta::assert_snapshot!(formatted);
            });
        }

        let diagnostic_output = {
            let mut output = String::new();
            let notes = session.notes;
            for error in &notes.errors {
                output.push_str(&Session::render_diagnostic(error, false));
            }
            for warning in &notes.warnings {
                output.push_str(&Session::render_diagnostic(warning, false));
            }
            output
        };

        if !diagnostic_output.is_empty() {
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
