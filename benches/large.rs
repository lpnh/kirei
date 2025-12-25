use criterion::{Criterion, criterion_group, criterion_main};
use std::hint::black_box;

use kirei::Kirei;

const AXUM_APP_INDEX: &str = include_str!("../tests/fixtures/axum-app/index.html");
const AXUM_APP_GREET: &str = include_str!("../tests/fixtures/axum-app/greet.html");
const AXUM_APP_ERROR: &str = include_str!("../tests/fixtures/axum-app/error.html");
const AXUM_APP_LAYOUT: &str = include_str!("../tests/fixtures/axum-app/_layout.html");

fn combined_axum_app() -> String {
    let mut combined = String::new();
    combined.push_str(AXUM_APP_INDEX);
    combined.push('\n');
    combined.push_str(AXUM_APP_GREET);
    combined.push('\n');
    combined.push_str(AXUM_APP_ERROR);
    combined.push('\n');
    combined.push_str(AXUM_APP_LAYOUT);
    combined
}

fn generate_file(base: &str, repetitions: usize) -> String {
    base.repeat(repetitions)
}

fn format_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("large_files");
    group.sample_size(10);

    let base = combined_axum_app(); // ~6KB

    // 100KB
    let file_100kb = generate_file(&base, 16);
    group.bench_function("100kb", |b| {
        let mut formatter = Kirei::default();
        b.iter(|| formatter.write(black_box(&file_100kb)).unwrap());
    });

    // 500KB
    let file_500kb = generate_file(&base, 80);
    group.bench_function("500kb", |b| {
        let mut formatter = Kirei::default();
        b.iter(|| formatter.write(black_box(&file_500kb)).unwrap());
    });

    group.finish();
}

criterion_group!(large_files_benches, format_benchmark);
criterion_main!(large_files_benches);
