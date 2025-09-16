mod helper;

#[test]
fn comment() {
    format_and_snapshot!("askama/comment.html");
}

#[test]
fn nested_comment() {
    format_and_snapshot!("askama/nested_comment.html");
}

#[test]
fn if_block() {
    format_and_snapshot!("askama/if_block.html");
}

#[test]
fn let_block() {
    format_and_snapshot!("askama/let_block.html");
}

#[test]
fn filter() {
    format_and_snapshot!("askama/filter.html");
}
