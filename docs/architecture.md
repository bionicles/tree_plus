# tree_plus Rust Port — Architecture

## Layout

```
crates/tree_plus_core      # library
  src/model.rs             # TreePlus, Category, counts
  src/config.rs            # TreePlusConfig (CLI flags map here)
  src/walk.rs              # traversal + tree construction (legacy engine.py)
  src/ignore.rs            # fnmatch-compatible ignore + amortized globs
  src/sort.rs              # natural path sort (natsort.os_sorted parity)
  src/count.rs             # wc-style token/line counting
  src/render.rs            # rich-Tree-compatible deterministic renderer
  src/extract/mod.rs       # dispatch (the legacy parse_file, renamed)
  src/extract/markers.rs   # TODO/BUG/NOTE markers
  src/extract/markdown.rs  # Markdown, RST, txt checkboxes
  src/extract/simple.rs    # .env, requirements, Makefile, Angular helpers
  src/extract/data.rs      # JSON family, JSONL, YAML, TOML, CSV, SQLite
  src/extract/treesitter/  # Rust, Python, JS/TS, C/C++ extractors
  tests/golden_parity.rs   # byte-level parity vs legacy Python goldens
  benches/tree_plus_bench.rs
crates/tree_plus_cli       # clap binary named `tree_plus`
tests/golden/              # golden generator + legacy goldens
```

## Naming correction

The legacy `parse_file` name was misleading: it never parsed files
generically; it extracted displayable component labels. The Rust analog is
`extract::extract_components(path, syntax)`.

## Why Tree-sitter for the major languages

Rust, Python, JavaScript/TypeScript, C and C++ use tree-sitter grammars:

- real parsing handles strings, comments, and nesting that regexes cannot;
- invalid syntax degrades to partial components (ERROR-node salvage) instead
  of catastrophic behavior;
- parsers are reused per thread (`thread_local` parser, immutable grammars).

The formatters do **not** dump AST text. Each language has a formatter that
emits source-text signature slices matching the legacy tree_plus style, with
the legacy regex quirks reproduced deliberately where tests depend on them
(e.g. Python `async def` is skipped, Rust `pub(crate)` items are skipped,
C++ enum variants keep their trailing commas). Where the legacy regex
produced demonstrably wrong output, the difference is documented in
docs/rust-port-differences.md rather than replicated.

## Why regex stays for the simple extractors

Markers, Markdown/RST headings, .env, requirements.txt, Makefile targets and
similar line-oriented formats are matched with the `regex` crate, compiled
once via `std::sync::LazyLock`. These patterns are anchored, bounded, and
have no look-around, so the linear-time `regex` engine is sufficient and
safe (no catastrophic backtracking by construction).

## Why fancy-regex is not used

An early draft ported the legacy TypeScript/C regexes verbatim with
fancy-regex (the originals need look-around). It was removed in favor of
tree-sitter formatters: backtracking regexes in the hot path are exactly the
failure mode the rewrite exists to remove (see `catastrophic.c`, a fixture
that exists because the legacy regex approach nearly exploded on it).

## Why nom/pest are not used

No mainstream language gets a homegrown grammar. Tree-sitter covers the big
languages; everything else in scope is line-oriented (regex) or structured
data with a native parser (serde_json, toml, serde_yaml, rusqlite).

## Data and config parsers

- JSON / package.json / JSON-RPC / OpenRPC / JSON Schema: `serde_json`
  (with `preserve_order` so output order matches the file).
- TOML (Cargo.toml, pyproject.toml): `toml` (with `preserve_order`).
- YAML (k8s, Ansible, GitHub workflows, OpenAPI): `serde_yaml` multi-doc.
- SQLite (.db/.sqlite): `rusqlite` behind the default-on `sqlite` feature.
- CSV: header line split, mirroring the legacy first-3-lines behavior
  (the csv crate would change behavior; the legacy reads only the header).

## Traversal, determinism, parallelism

- `walk.rs` recursion mirrors the legacy `Path.iterdir()` walk; entries are
  sorted with `sort::os_sort_key` before any parallel work, then files are
  processed with rayon (`into_par_iter`) and collected in order, so output
  is deterministic regardless of thread scheduling.
- Ignores match any path component via fnmatch translation; glob filters are
  amortized exactly like the legacy `AmortizedGlobs` (match set + ancestors).
- Binary detection reads at most 1 KiB; counting skips the legacy extension
  list; files above 1M tokens skip extraction.

## Rendering

`render.rs` reimplements the small slice of `rich` the legacy output
depends on: tree guides, multiline label continuation, cell-width-aware
word wrapping (`divide_line`/`chop_cells` ports, fold semantics), 8-cell tab
expansion, and trailing-space removal. Width is 80 when piped (the legacy
capture width), the terminal width when interactive, and 128 under
`TREE_PLUS_UPDATE_README=YES`.

## How to add a new language

1. Add the extension to the dispatch in `extract/mod.rs` (order matters and
   is tested; mirror the legacy dispatch position).
2. Tree-sitter route: add the grammar crate, write a formatter in
   `extract/treesitter/<lang>.rs` that emits signature slices, and recurse
   into bodies for nested items.
3. Regenerate goldens: `python tests/golden/generate_legacy_goldens.py`
   (remove the language from `DEFERRED_PARSERS` in the generator so the v1
   tree goldens include its components).
4. The fixture parity test picks the extension up via `in_v1_scope` in
   `crates/tree_plus_core/tests/golden_parity.rs`; add it there.
5. Add unit tests for the formatter and at least one pathological input.
