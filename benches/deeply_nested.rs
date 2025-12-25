use criterion::{Criterion, criterion_group, criterion_main};
use std::hint::black_box;

use kirei::Kirei;

fn generate_deeply_nested_html() -> String {
    let mut template = String::new();
    let depth = 50;

    for _ in 0..depth {
        template.push_str(r#"<div id="i">\n"#);
        template.push_str("Some text\n");
    }
    template.push_str("<p>Deep content</p>\n");
    template.push_str("outside html tags\n");
    for _ in 0..depth {
        template.push_str("More text\n");
        template.push_str("</div>\n");
    }

    template
}

fn generate_deeply_nested_tmpl() -> String {
    let mut template = String::new();
    let depth = 50;

    for _ in 0..depth {
        template.push_str("{% block i %}\n");
        template.push_str("Some text\n");
        template.push_str("{% if i != depth %}\n");
        template.push_str("{{ i }}\n");
    }
    template.push_str("{{ deep_content }}\n");
    template.push_str("outside expression\n");
    for _ in 0..depth {
        template.push_str("{% endif %}\n");
        template.push_str("More text\n");
        template.push_str("{% endblock %}\n");
    }

    template
}

fn generate_deeply_nested_mixed() -> String {
    let mut template = String::new();
    let depth = 50;

    for _ in 0..depth {
        template.push_str("{% block i %}\n");
        template.push_str("Some text\n");
        template.push_str("{% if i != depth %}\n");
        template.push_str(r#"<div id="i">\n"#);
        template.push_str("<p>Oh, hi {{ username }} !</p>\n");
        template.push_str("Ah... I said that already?\n");
    }
    template.push_str("Wait! What is <em>this</em>?\n");
    template.push_str("<span>A deeply nested paragraph...</span>\n");
    template.push_str("...and some content outside tags and blocks?!\n");
    template.push_str("Omg! And <strong>that</strong>?\n");
    template.push_str("{{ even_more_content }}\n");
    template.push_str("outside expression!!\n");
    for _ in 0..depth {
        template.push_str("</div>\n");
        template.push_str("{% endif %}\n");
        template.push_str("More text\n");
        template.push_str("{% endblock %}\n");
    }

    template
}

fn format_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("deeply_nested");
    group.sample_size(50);

    group.bench_function("html", |b| {
        let template = generate_deeply_nested_html();
        let mut formatter = Kirei::default();
        b.iter(|| formatter.write(black_box(&template)).unwrap());
    });

    group.bench_function("askama", |b| {
        let template = generate_deeply_nested_tmpl();
        let mut formatter = Kirei::default();
        b.iter(|| formatter.write(black_box(&template)).unwrap());
    });

    group.bench_function("mixed", |b| {
        let template = generate_deeply_nested_mixed();
        let mut formatter = Kirei::default();
        b.iter(|| formatter.write(black_box(&template)).unwrap());
    });

    group.finish();
}

criterion_group!(deeply_nested_benches, format_benchmark);
criterion_main!(deeply_nested_benches);
