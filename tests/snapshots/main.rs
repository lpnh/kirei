#[macro_export]
macro_rules! format_and_snapshot {
    ($fixture_path:expr) => {
        let path = std::path::Path::new("tests/fixtures").join($fixture_path);
        let input = std::fs::read_to_string(&path).unwrap_or_else(|_| panic!("Could not read fixture: {}", path.display()));
        let mut kirei = kirei::write::Kirei::default();
        let formatted = kirei.write(&input).unwrap();

        insta::with_settings!({
            description => input,
            omit_expression => true,
            prepend_module_to_snapshot => false,
            snapshot_path => "output",
        }, {
            insta::assert_snapshot!(formatted);
        });
    };
}

mod askama;
mod axum_app;
mod html;
mod ice_cream_club;
mod malformed;
mod mixed;
mod phrasing;
mod rules;
mod teapot;
