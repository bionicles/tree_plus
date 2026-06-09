# Intentional Differences from the Python Implementation

Everything not listed here is byte-identical for the version-1 scope; the
golden parity suite (`cargo test -p tree_plus_core --test golden_parity`)
enforces it against outputs captured from the legacy implementation.

## Output differences (version-1 languages)

1. **TensorFlow flag special case removed.** Legacy `parse_file` special-cased
   `.cc`/`.h` files whose path contained "tensorflow" and whose name contained
   "flags", emitting `Flag('...')` components. Deliberately dropped (decision:
   too project-specific for a general tool). `tensorflow_flags.h` still gets
   full C/C++ extraction. The parity test pins the exact remaining components.

2. **String literals are no longer mistaken for code.** The legacy TS regex
   emitted `  return("Standalone function with parameters")` because the
   string contains the word "function". Tree-sitter does not parse string
   contents, so this artifact is gone.

3. **A few regex-noise lines are no longer emitted** for TS/JS and C/C++:
   the legacy "method" pattern matched some bare call statements. The common
   ones (`super(...)`, simple `name(args);` calls, `while((...))`, indented
   `asm(...)`, pybind `.def("...` chains) are still emitted for parity, but
   awaited calls and calls with complex callees that the legacy regex happened
   to match in other codebases may differ. Fixture-covered cases all match.

4. **Type predicates are no longer truncated mid-token in new code paths.**
   Where the legacy lazily stopped a TS return type at the first ` {`
   (e.g. `): ticket is`), the port reproduces the truncation for parity; this
   is noted as a known legacy wart to revisit in version 2.

## Binary name / migration

The Rust CLI builds two identical binaries: `tree_plus` (legacy-compatible
name) and `tprs` (alias that avoids PATH collisions with the Python
`tree_plus` entry point, e.g. from a conda env). Install one or both:

```
cargo install --path crates/tree_plus_cli --bin tprs        # alias only
cargo install --path crates/tree_plus_cli                   # both binaries
```

## Behavioral differences

5. **Tokenizers.** Only the default word-count tokenizer (`wc`: tokens =
   characters / 4) is implemented. `-t` / `-T gpt4o` produce an explicit
   error instead of importing tiktoken. (`-T wc` is accepted.)

6. **Web modes are deferred.** URL seeds, `--yc`/`--hn`, `-n`, `-m`, and
   `-l` are not implemented; the CLI does not accept the HN flags. Local
   filesystem mode is complete.

7. **Syntax highlighting is deferred.** Output is plain text (identical to
   the legacy `into_str()` capture). `-s` is accepted for compatibility.
   Note the legacy enum markup escaping (e.g. `#\[default]`) is preserved so
   plain-text output matches the legacy renderer byte-for-byte.

8. **`--timeout` is accepted and ignored.** The Rust extractors are
   linear-time; there is no regex timeout to configure. The legacy behavior
   of returning zero components when extraction fails is preserved.

9. **Sort tie-breaking.** Names that differ only by case (identical natural
   sort keys) fall back to raw byte order in Rust; Python fell back to
   filesystem enumeration order (nondeterministic). Deterministic on purpose.

10. **`COLUMNS`/TTY width.** When piped, output wraps at width 80 exactly like
    the legacy `into_str()`. Interactive terminals use the real width, like
    the legacy CLI did; `TREE_PLUS_UPDATE_README=YES` forces 128. The legacy
    interactive renderer also applied `tab_size=2` and rich markup; the Rust
    CLI always renders the deterministic plain-text form (tab size 8).

11. **`read_file` caching.** The legacy cached file contents process-wide
    (`lru_cache`); the port reads files once per run with no global cache.

12. **Deferred-language files** (see docs/language-roadmap.md) still appear
    in trees with correct counts and TODO/BUG/NOTE markers, but no language
    components. The legacy goldens for those languages are kept as the
    contract for future ports (`tests/golden/legacy/components/`).
