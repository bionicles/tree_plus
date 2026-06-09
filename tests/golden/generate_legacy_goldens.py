# tests/golden/generate_legacy_goldens.py
"""Generate golden outputs from the legacy Python tree_plus implementation.

Run from the repository root:
    python tests/golden/generate_legacy_goldens.py

Outputs land in tests/golden/legacy/:
- components/<sanitized_path>.json : parse_file() component lists per fixture
- trees/<name>.txt                 : into_str() renders for directories/files
- counts/<sanitized_path>.json     : count_tokens_lines() per fixture (wc tokenizer)
"""
from pathlib import Path
import json
import sys

sys.path.insert(0, str(Path(__file__).resolve().parents[2]))

import tree_plus_src as tree_plus  # noqa: E402
from tree_plus_src import engine  # noqa: E402

REPO = Path(__file__).resolve().parents[2]
OUT = REPO / "tests" / "golden" / "legacy"

FIXTURE_DIRS = [
    REPO / "tests" / "path_to_test",
    REPO / "tests" / "more_languages",
    REPO / "tests" / "dot_dot",
]

TREE_TARGETS = {
    "path_to_test": ("tests/path_to_test",),
    "more_languages": ("tests/more_languages",),
    "more_languages_group1": ("tests/more_languages/group1",),
    "more_languages_group2": ("tests/more_languages/group2",),
    "more_languages_group3": ("tests/more_languages/group3",),
    "more_languages_group4": ("tests/more_languages/group4",),
    "more_languages_group5": ("tests/more_languages/group5",),
    "more_languages_group6": ("tests/more_languages/group6",),
    "more_languages_group7": ("tests/more_languages/group7",),
    "more_languages_group_lisp": ("tests/more_languages/group_lisp",),
    "more_languages_group_todo": ("tests/more_languages/group_todo",),
    "dot_dot": ("tests/dot_dot",),
    "multi_seed": ("tests/path_to_test", "tests/more_languages/group1"),
}


def sanitize(p: Path) -> str:
    rel = p.relative_to(REPO)
    return str(rel).replace("/", "__")


DEFERRED_PARSERS = [
    "parse_php", "parse_bash",
    "parse_ps1", "parse_zig", "parse_rb", "parse_sql", "parse_graphql",
    "parse_cs", "parse_jl", "parse_scala", "parse_perl",
    "parse_hs", "parse_fsharp", "parse_lisp", "parse_erl", "parse_capnp",
    "parse_grpc", "parse_tex", "parse_lean", "parse_fortran", "parse_tf",
    "parse_isabelle", "parse_lua", "parse_tcl", "parse_objective_c",
    "parse_matlab", "parse_r", "parse_mathematica", "parse_ocaml",
    "parse_cbl", "parse_apl", "parse_metal", "parse_wgsl",
    "parse_tensorflow_flags",
]

TS_ARTIFACT = '  return("Standalone function with parameters")'


def _patch_for_v1_scope() -> None:
    import importlib

    # the package re-exports the function; we need the module itself
    pf = importlib.import_module("tree_plus_src.parse_file")

    for name in DEFERRED_PARSERS:
        if hasattr(pf, name):
            setattr(pf, name, lambda *a, **k: [])
    original_parse_ts = pf.parse_ts

    def parse_ts_no_artifact(*a, **k):
        return [c for c in original_parse_ts(*a, **k) if c != TS_ARTIFACT]

    pf.parse_ts = parse_ts_no_artifact
    # the engine's read_file/parse caches must not leak between runs
    pf.read_file.cache_clear()


def main() -> None:
    (OUT / "components").mkdir(parents=True, exist_ok=True)
    (OUT / "trees").mkdir(parents=True, exist_ok=True)
    (OUT / "counts").mkdir(parents=True, exist_ok=True)

    n_components = 0
    n_counts = 0
    for fixture_dir in FIXTURE_DIRS:
        for path in sorted(fixture_dir.rglob("*")):
            if not path.is_file():
                continue
            if "__pycache__" in path.parts or path.suffix == ".pyc":
                continue
            # skip anything the default ignore set would hide (test artifacts
            # like .hypothesis, caches, compiled files)
            if tree_plus.should_ignore(path, tree_plus.DEFAULT_IGNORE):
                continue
            rel = str(path.relative_to(REPO))
            try:
                components = tree_plus.parse_file(rel)
            except Exception as e:
                components = [f"__EXCEPTION__: {type(e).__name__}: {e}"]
            (OUT / "components" / f"{sanitize(path)}.json").write_text(
                json.dumps(
                    {"path": rel, "components": components},
                    indent=1,
                    ensure_ascii=False,
                )
            )
            n_components += 1
            try:
                count = tree_plus.count_tokens_lines(rel)
            except Exception:
                count = None
            payload = (
                None
                if count is None
                else {"n_tokens": count.n_tokens, "n_lines": count.n_lines}
            )
            (OUT / "counts" / f"{sanitize(path)}.json").write_text(
                json.dumps({"path": rel, "count": payload}, ensure_ascii=False)
            )
            n_counts += 1

    for name, seeds in TREE_TARGETS.items():
        root = engine.from_seeds(seeds, tokenizer_name=tree_plus.TokenizerName.WC)
        (OUT / "trees" / f"{name}.txt").write_text(root.into_str())

    # v1-scope tree goldens: legacy output with extractors outside the Rust
    # port's version-1 scope stubbed out (deferred languages keep markers but
    # lose components), plus documented intentional differences.
    _patch_for_v1_scope()
    (OUT / "trees_v1").mkdir(parents=True, exist_ok=True)
    for name, seeds in TREE_TARGETS.items():
        root = engine.from_seeds(seeds, tokenizer_name=tree_plus.TokenizerName.WC)
        (OUT / "trees_v1" / f"{name}.txt").write_text(root.into_str())

    # concise render of the whole repo (deterministic, no parsing)
    root = engine.from_seeds(
        (str(REPO),), tokenizer_name=tree_plus.TokenizerName.WC, concise=True
    )
    concise = root.into_str()
    # the root label embeds the repo dir name only; keep as-is
    (OUT / "trees" / "repo_concise.txt").write_text(concise)

    print(f"wrote {n_components} component files, {n_counts} counts,"
          f" {len(TREE_TARGETS) + 1} trees -> {OUT}")


if __name__ == "__main__":
    main()
