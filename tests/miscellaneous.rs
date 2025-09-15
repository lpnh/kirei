mod helper;

#[test]
fn empty_file() {
    format_and_snapshot!("empty_file.html");
}
