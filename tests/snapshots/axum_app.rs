#[test]
fn layout() {
    format_and_snapshot!("axum-app/_layout.html");
}

#[test]
fn error() {
    format_and_snapshot!("axum-app/error.html");
}

#[test]
fn greet() {
    format_and_snapshot!("axum-app/greet.html");
}

#[test]
fn index() {
    format_and_snapshot!("axum-app/index.html");
}
