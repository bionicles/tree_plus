//! Criterion benchmarks: traversal+counting, extraction, and full renders.
//!
//! Run: cargo bench -p tree_plus_core
//! Benchmark any repository: cargo bench -p tree_plus_core -- --quick
//! (set TREE_PLUS_BENCH_PATH=/path/to/repo to point at a different tree).

use std::path::{Path, PathBuf};

use criterion::{criterion_group, criterion_main, Criterion};
use std::hint::black_box;

use tree_plus_core::{extract_components, from_seeds, render_to_string, TreePlusConfig};

fn repo_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap()
}

fn bench_target() -> PathBuf {
    std::env::var("TREE_PLUS_BENCH_PATH")
        .map(PathBuf::from)
        .unwrap_or_else(|_| repo_root())
}

fn full_tree(c: &mut Criterion) {
    let target = bench_target();
    let seeds = vec![target.to_string_lossy().into_owned()];
    let config = TreePlusConfig::default();
    c.bench_function("full_tree_with_components", |b| {
        b.iter(|| {
            let tree = from_seeds(black_box(&seeds), &config);
            black_box(render_to_string(&tree, 80))
        })
    });
}

fn concise_tree(c: &mut Criterion) {
    let target = bench_target();
    let seeds = vec![target.to_string_lossy().into_owned()];
    let config = TreePlusConfig {
        concise: true,
        ..Default::default()
    };
    c.bench_function("concise_tree_traversal_counting", |b| {
        b.iter(|| {
            let tree = from_seeds(black_box(&seeds), &config);
            black_box(render_to_string(&tree, 80))
        })
    });
}

fn single_file_extraction(c: &mut Criterion) {
    let root = repo_root();
    let cases = [
        ("rust", "tests/more_languages/group4/rust_test.rs"),
        ("python", "tests/path_to_test/class_method_type.py"),
        ("typescript", "tests/more_languages/group1/test.ts"),
        (
            "c_pathological",
            "tests/more_languages/group6/catastrophic.c",
        ),
        ("markdown", "tests/more_languages/group_todo/todo.md"),
    ];
    for (name, rel) in cases {
        let path = root.join(rel);
        if !path.exists() {
            continue;
        }
        c.bench_function(&format!("extract_{name}"), |b| {
            b.iter(|| black_box(extract_components(black_box(&path), false)))
        });
    }
}

criterion_group!(benches, full_tree, concise_tree, single_file_extraction);
criterion_main!(benches);
