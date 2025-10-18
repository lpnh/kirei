use criterion::{Criterion, criterion_group, criterion_main};
use std::hint::black_box;

use kirei::AskamaFormatter;

const ICE_CREAM_CLUB_CARD: &str =
    include_str!("../tests/fixtures/ice-cream-club/ice_cream_card.html");
const ICE_CREAM_CLUB_HOME: &str = include_str!("../tests/fixtures/ice-cream-club/home.html");
const ICE_CREAM_CLUB_FLAVORS: &str = include_str!("../tests/fixtures/ice-cream-club/flavors.html");
const ICE_CREAM_CLUB_CLUB: &str = include_str!("../tests/fixtures/ice-cream-club/club.html");
const ICE_CREAM_CLUB_BASE: &str = include_str!("../tests/fixtures/ice-cream-club/base.html");

fn format_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("ice_cream_club");
    group.sample_size(50);

    group.bench_function("card", |b| {
        let mut formatter = AskamaFormatter::new().unwrap();
        b.iter(|| {
            formatter.format(black_box(ICE_CREAM_CLUB_CARD)).unwrap()
        });
    });

    group.bench_function("home", |b| {
        let mut formatter = AskamaFormatter::new().unwrap();
        b.iter(|| {
            formatter.format(black_box(ICE_CREAM_CLUB_HOME)).unwrap()
        });
    });

    group.bench_function("flavors", |b| {
        let mut formatter = AskamaFormatter::new().unwrap();
        b.iter(|| {
            formatter.format(black_box(ICE_CREAM_CLUB_FLAVORS)).unwrap()
        });
    });

    group.bench_function("club", |b| {
        let mut formatter = AskamaFormatter::new().unwrap();
        b.iter(|| {
            formatter.format(black_box(ICE_CREAM_CLUB_CLUB)).unwrap()
        });
    });

    group.bench_function("base", |b| {
        let mut formatter = AskamaFormatter::new().unwrap();
        b.iter(|| {
            formatter.format(black_box(ICE_CREAM_CLUB_BASE)).unwrap()
        });
    });

    group.finish();
}

criterion_group!(ice_cream_club_benches, format_benchmark);
criterion_main!(ice_cream_club_benches);
