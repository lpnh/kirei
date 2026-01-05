use criterion::{Criterion, Throughput, criterion_group, criterion_main};
use kirei::Session;
use std::hint::black_box;

const ICE_CREAM_CLUB_CARD: &str =
    include_str!("../tests/fixtures/ice-cream-club/ice_cream_card.html");
const ICE_CREAM_CLUB_HOME: &str = include_str!("../tests/fixtures/ice-cream-club/home.html");
const ICE_CREAM_CLUB_FLAVORS: &str = include_str!("../tests/fixtures/ice-cream-club/flavors.html");
const ICE_CREAM_CLUB_CLUB: &str = include_str!("../tests/fixtures/ice-cream-club/club.html");
const ICE_CREAM_CLUB_BASE: &str = include_str!("../tests/fixtures/ice-cream-club/base.html");

fn combined_templates() -> Vec<&'static str> {
    vec![
        ICE_CREAM_CLUB_CARD,
        ICE_CREAM_CLUB_HOME,
        ICE_CREAM_CLUB_FLAVORS,
        ICE_CREAM_CLUB_CLUB,
        ICE_CREAM_CLUB_BASE,
    ]
}

fn format_benchmark(c: &mut Criterion) {
    let templates = combined_templates();
    let total_bytes: usize = templates.iter().map(|t| t.len()).sum();

    let mut group = c.benchmark_group("ice_cream_club");
    group.throughput(Throughput::Bytes(total_bytes as u64));

    group.bench_function("all_files", |b| {
        let mut session = Session::default();
        b.iter(|| {
            for template in &templates {
                black_box(session.format(black_box(template), "bench.html").value);
            }
        });
    });

    group.finish();
}

criterion_group!(ice_cream_club_benches, format_benchmark);
criterion_main!(ice_cream_club_benches);
