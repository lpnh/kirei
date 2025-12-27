use criterion::{Criterion, Throughput, criterion_group, criterion_main};
use kirei::Kirei;
use std::hint::black_box;

const AXUM_APP_INDEX: &str = include_str!("../tests/fixtures/axum-app/index.html");
const AXUM_APP_GREET: &str = include_str!("../tests/fixtures/axum-app/greet.html");
const AXUM_APP_ERROR: &str = include_str!("../tests/fixtures/axum-app/error.html");
const AXUM_APP_LAYOUT: &str = include_str!("../tests/fixtures/axum-app/_layout.html");

fn combined_templates() -> Vec<&'static str> {
    vec![
        AXUM_APP_INDEX,
        AXUM_APP_GREET,
        AXUM_APP_ERROR,
        AXUM_APP_LAYOUT,
    ]
}

fn format_benchmark(c: &mut Criterion) {
    let templates = combined_templates();
    let total_bytes: usize = templates.iter().map(|t| t.len()).sum();

    let mut group = c.benchmark_group("axum_app");
    group.throughput(Throughput::Bytes(total_bytes as u64));

    group.bench_function("all_files", |b| {
        let mut formatter = Kirei::default();
        b.iter(|| {
            for template in &templates {
                black_box(formatter.write(black_box(template)).0);
            }
        });
    });

    group.finish();
}

criterion_group!(axum_app_benches, format_benchmark);
criterion_main!(axum_app_benches);
