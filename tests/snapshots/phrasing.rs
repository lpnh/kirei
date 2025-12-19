#[test]
fn phrasing_checkbox() {
    format_and_snapshot!("phrasing/checkbox.html");
}

#[test]
fn phrasing_impossible_things() {
    format_and_snapshot!("phrasing/impossible_things.html");
}

// source: https://developer.mozilla.org/en-US/docs/Web/MathML/Reference/Element/math#examples
#[test]
fn phrasing_infinite_sum() {
    format_and_snapshot!("phrasing/infinite_sum.html");
}

#[test]
fn phrasing_looking_glass() {
    format_and_snapshot!("phrasing/looking_glass.html");
}

#[test]
fn phrasing_mad_tea_party() {
    format_and_snapshot!("phrasing/mad_tea_party.html");
}

#[test]
fn phrasing_quotes() {
    format_and_snapshot!("phrasing/quotes.html");
}

#[test]
fn phrasing_small_cake() {
    format_and_snapshot!("phrasing/small_cake.html");
}

#[test]
fn phrasing_mixed_ws_1() {
    format_and_snapshot!("phrasing/mixed_ws_1.html");
}

#[test]
fn phrasing_mixed_ws_2() {
    format_and_snapshot!("phrasing/mixed_ws_2.html");
}

#[test]
fn phrasing_nested_lorem() {
    format_and_snapshot!("phrasing/nested_lorem.html");
}

#[test]
fn phrasing_mixed_roots() {
    format_and_snapshot!("phrasing/mixed_roots.html");
}
