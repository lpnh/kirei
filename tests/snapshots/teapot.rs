#[test]
fn teapot_base() {
    format_and_snapshot!("teapot/base.html");
}

#[test]
fn teapot_brewing_practices() {
    format_and_snapshot!("teapot/brewing-practices.html");
}

#[test]
fn teapot_http_protocol() {
    format_and_snapshot!("teapot/http-protocol.html");
}

#[test]
fn teapot_index() {
    format_and_snapshot!("teapot/index.html");
}

#[test]
fn teapot_intro() {
    format_and_snapshot!("teapot/intro.html");
}

#[test]
fn teapot_status_code_details() {
    format_and_snapshot!("teapot/status-code-details.html");
}

#[test]
fn teapot_story() {
    format_and_snapshot!("teapot/story.html");
}
