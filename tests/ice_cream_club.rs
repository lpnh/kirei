mod helper;

#[test]
fn base() {
    format_and_snapshot!("ice-cream-club/base.html");
}

#[test]
fn club() {
    format_and_snapshot!("ice-cream-club/club.html");
}

#[test]
fn flavors() {
    format_and_snapshot!("ice-cream-club/flavors.html");
}

#[test]
fn home() {
    format_and_snapshot!("ice-cream-club/home.html");
}

#[test]
fn ice_cream_card() {
    format_and_snapshot!("ice-cream-club/ice_cream_card.html");
}
