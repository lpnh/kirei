use criterion::{Criterion, Throughput, criterion_group, criterion_main};
use kirei::Kirei;
use std::hint::black_box;

fn generate_nested_html() -> String {
    let mut template = String::new();
    let depth = 50;

    for _ in 0..depth {
        template.push_str("<div id=\"i\">\n");
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

fn generate_nested_askama() -> String {
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

fn generate_nested_mixed() -> String {
    let mut template = String::new();
    let depth = 50;

    for _ in 0..depth {
        template.push_str("{% block i %}\n");
        template.push_str("Some text\n");
        template.push_str("{% if i != depth %}\n");
        template.push_str("<div id=\"i\">\n");
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
    // save_bench_files();

    let html = generate_nested_html();
    let askama = generate_nested_askama();
    let mixed = generate_nested_mixed();

    let templates = vec![html, askama, mixed];
    let total_bytes: usize = templates.iter().map(|t| t.len()).sum();

    let mut group = c.benchmark_group("deeply_nested");
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

criterion_group!(deeply_nested_benches, format_benchmark);
criterion_main!(deeply_nested_benches);

fn save_bench_files() {
    use std::fs;
    use std::path::Path;

    let output_dir = Path::new("target/bench_templates");
    fs::create_dir_all(output_dir).unwrap();

    let templates = [
        ("nested_html.html", generate_nested_html()),
        ("nested_askama.html", generate_nested_askama()),
        ("nested_mixed.html", generate_nested_mixed()),
    ];

    for (filename, content) in &templates {
        let path = output_dir.join(filename);
        fs::write(&path, content).unwrap();
        println!("Generated: {}", path.display());
    }
}
