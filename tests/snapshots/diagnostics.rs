#[test]
fn erroneous_end_tag_inline() {
    snapshot_test!("diagnostics/erroneous_end_tag_inline.html");
}

#[test]
fn erroneous_end_tag_multiline() {
    snapshot_test!("diagnostics/erroneous_end_tag_multiline.html");
}

#[test]
fn invalid_ctrl_statement() {
    snapshot_test!("diagnostics/invalid_ctrl_statement.html");
}

#[test]
fn unclosed_exp_inside_attr() {
    snapshot_test!("diagnostics/unclosed_exp_inside_attr.html");
}

#[test]
fn diagnostics_crossing_for() {
    snapshot_test!("diagnostics/crossing_for.html");
}

#[test]
fn diagnostics_crossing_if() {
    snapshot_test!("diagnostics/crossing_if.html");
}

#[test]
fn diagnostics_crossing_match() {
    snapshot_test!("diagnostics/crossing_match.html");
}

#[test]
fn diagnostics_missing_block_name() {
    snapshot_test!("diagnostics/missing_block_name.html");
}

#[test]
fn diagnostics_invalid_css() {
    snapshot_test!("diagnostics/invalid_css.html");
}
