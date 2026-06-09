# tests/golden/diff_components.py
"""Diff Rust extract output against legacy goldens for given fixture paths.

Usage: python tests/golden/diff_components.py <fixture-rel-path>...
Runs the Rust example binary and prints a unified diff per fixture.
"""
import difflib
import json
import subprocess
import sys
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]


def main() -> None:
    paths = sys.argv[1:]
    out = subprocess.run(
        ["cargo", "run", "-q", "-p", "tree_plus_core", "--example", "extract", "--"]
        + paths,
        capture_output=True,
        text=True,
        cwd=REPO,
    )
    if out.returncode != 0:
        print(out.stderr)
        sys.exit(1)
    decoder = json.JSONDecoder()
    rest = out.stdout.strip()
    results = []
    while rest:
        obj, idx = decoder.raw_decode(rest)
        results.append(obj)
        rest = rest[idx:].lstrip()
    for result in results:
        rel = result["path"]
        golden_name = rel.replace("/", "__") + ".json"
        golden = json.loads(
            (REPO / "tests/golden/legacy/components" / golden_name).read_text()
        )
        expected = golden["components"]
        actual = result["components"]
        if expected == actual:
            print(f"OK {rel}")
            continue
        print(f"DIFF {rel} (expected {len(expected)}, actual {len(actual)})")
        diff = difflib.unified_diff(
            [json.dumps(c) for c in expected],
            [json.dumps(c) for c in actual],
            "legacy",
            "rust",
            lineterm="",
        )
        for line in diff:
            print(" ", line)


if __name__ == "__main__":
    main()
