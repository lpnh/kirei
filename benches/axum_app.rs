use criterion::{Criterion, criterion_group, criterion_main};
use std::hint::black_box;

use kirei::AskamaFormatter;

const AXUM_APP_INDEX: &str = include_str!("../tests/fixtures/axum-app/index.html");
const AXUM_APP_GREET: &str = include_str!("../tests/fixtures/axum-app/greet.html");
const AXUM_APP_ERROR: &str = include_str!("../tests/fixtures/axum-app/error.html");
const AXUM_APP_LAYOUT: &str = include_str!("../tests/fixtures/axum-app/_layout.html");

fn format_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("axum_app");
    group.sample_size(50);

    group.bench_function("index", |b| {
        let mut formatter = AskamaFormatter::new().unwrap();
        b.iter(|| {
            formatter.format(black_box(AXUM_APP_INDEX)).unwrap()
        });
    });

    group.bench_function("greet", |b| {
        let mut formatter = AskamaFormatter::new().unwrap();
        b.iter(|| {
            formatter.format(black_box(AXUM_APP_GREET)).unwrap()
        });
    });

    group.bench_function("error", |b| {
        let mut formatter = AskamaFormatter::new().unwrap();
        b.iter(|| {
            formatter.format(black_box(AXUM_APP_ERROR)).unwrap()
        });
    });

    group.bench_function("layout", |b| {
        let mut formatter = AskamaFormatter::new().unwrap();
        b.iter(|| {
            formatter.format(black_box(AXUM_APP_LAYOUT)).unwrap()
        });
    });

    group.finish();
}

criterion_group!(axum_app_benches, format_benchmark);
criterion_main!(axum_app_benches);
