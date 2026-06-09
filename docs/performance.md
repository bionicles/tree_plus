# Performance Report (Rust Port)

Measured 2026-06-09 on Linux (WSL2), release build, default features.
Comparisons via hyperfine (warmup 1) against the legacy Python CLI
(Python 3.10, tree_plus 1.0.79). "Full" = components extracted; "concise"
= traversal + counting only.

## End-to-end CLI, this repository (473 files, ~50.7k lines, ~540k tokens)

| Invocation | Rust | Python | Speedup |
|---|---|---|---|
| `tree_plus -c .` (traversal+counting) | 11.8 ms ± 1.3 | 782.8 ms ± 3.5 | **66×** |
| `tree_plus .` (full extraction) | 28.0 ms ± 1.9 | 2.385 s ± 0.022 | **85×** |

Throughput at the full-extraction figure: ~17k files/s; ~75 MB/s of
repository text (2.1 MB of counted text; 7.5 MB on disk incl. binaries).

## Medium repositories (full extraction)

| Repository | Kind | Stats (Rust footer) | Rust wall | Speedup vs Python |
|---|---|---|---|---|
| Gymnasium | Python | 684 files, 63k lines, 704k tokens | ~20 ms | **84×** |
| diamond-types | Rust | 290 files, 75k lines, 1.27M tokens | ~40 ms | **70×** |
| ramda | JavaScript | 700 files, 46k lines, 389k tokens | ~30 ms | **55×** |

Reproduce with any local tree:

```
cargo build --release
hyperfine './target/release/tree_plus /path/to/repo > /dev/null' \
          'python tree_plus_cli.py /path/to/repo > /dev/null'
```

## Linux kernel (torvalds/linux @ ~6.15, 101,136 files, 44.77M lines)

Single runs (`/usr/bin/time -v`), page cache warm:

| Invocation | Rust wall | Peak RSS | Notes |
|---|---|---|---|
| `tprs -c linux` | 0.50 s | 178 MB | 6,285 folders, 101,136 files, 44.77M lines, 409.2M tokens |
| `tprs linux` | 12.4 s | 1.5 GB | full extraction; 8.42M-line render (598 MB of output) |
| `tprs linux/kernel` | 0.23 s | — | 711 files, 553,855 lines; Python: 12.6 s → **~56×** |

Counts (lines, tokens) agree byte-for-byte with the legacy Python CLI on
`linux/kernel`. Peak RSS on the full run is dominated by the final render
string (~600 MB) plus the assembled tree, not by extraction.

The kernel run also surfaced two stack-overflow bugs, both fixed:

1. Extractor visitors recursed on AST depth.
   `arch/x86/kernel/cpu/microcode/intel-ucode-defs.h` is a headerless
   initializer-list fragment that tree-sitter parses as a deeply nested
   ERROR tree; the recursive walk blew rayon's 2 MiB worker stacks. All
   tree-sitter extractors now traverse with explicit heap stacks
   (regression test: `deep_nesting_never_overflows_worker_stacks`).
2. Rayon work-stealing nests `from_folder` frames on a worker while it
   waits in `join`, leaving too little headroom for tree-sitter's C
   frames on big trees (`drivers/` segfaulted). Extraction now runs in a
   dedicated pool with 16 MiB worker stacks.

Also reproducible via
`TREE_PLUS_BENCH_PATH=/path/to/linux cargo bench -p tree_plus_core`.

## Single-file extraction latency (criterion, mean)

| Fixture | Extractor | Mean |
|---|---|---|
| class_method_type.py (101 lines) | tree-sitter Python | 217 µs |
| test.ts (165 lines) | tree-sitter TypeScript | 386 µs |
| rust_test.rs (~190 lines) | tree-sitter Rust | 670 µs |
| catastrophic.c (754 lines, pathological) | tree-sitter C | 1.81 ms |

Criterion reports p50≈mean here with tight bounds (±1%); the pathological
C file that motivated the rewrite extracts in under 2 ms (the legacy regex
needed a timeout guard for files like it).

Run: `cargo bench -p tree_plus_core` (whole-repo benches accept
`TREE_PLUS_BENCH_PATH`).

## Where the time goes / tuning applied

- Files are processed in parallel with rayon after a deterministic sort, so
  output order never depends on scheduling; no locks in the hot path.
- One tree-sitter parser per thread (`thread_local`), grammars shared.
- All regexes compiled once (`LazyLock`); only the linear-time `regex`
  crate is used (no backtracking engine anywhere).
- Binary sniffing is capped at 1 KiB; CSV/JSONL read only the first lines;
  files over 1M tokens skip extraction (legacy behavior).
- Peak memory (full run on this repo): 43 MB RSS (`/usr/bin/time -v`),
  dominated by per-file contents held during parallel extraction.
