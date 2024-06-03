# tree_plus_src/ignore.py
from typing import Optional, Tuple, Union, Any, FrozenSet
from dataclasses import dataclass
from functools import lru_cache
from pathlib import Path
import os
import re

from rich import print as rich_print
import fnmatch

from tree_plus_src.debug import debug_print

DEFAULT_IGNORE_FROZENSET = frozenset(
    {
        "__init__.py",
        "__pycache__",
        "._*",
        ".angular",
        ".cache",
        ".coverage",
        ".DS_Store",
        # ".env*", # no, we parse this
        ".flake8",
        ".git",
        ".hypothesis",
        ".idea",
        ".ipynb_checkpoints",
        ".pytest_cache",
        ".rustc_info.json",
        ".vscode",
        "*_memmap",
        "*:Zone.Identifier",
        # "*.7z",
        "*.a",
        # "*.aac",
        "*.ai",
        # "*.avi",
        "*.bak",
        "*.bin",
        "*.bz2",
        "*.chk",
        "*.class",
        # "*.csv", # no, we parse this
        "*.d",
        "*.dat",
        "*.dll",
        # "*.doc",
        # "*.docx",
        "*.dylib",
        "*.ear",
        "*.egg-info",
        "*.eot",
        "*.eps",
        # "*.exe",
        "*.flac",
        "*.flv",
        "*.framework",
        # "*.gdoc",
        # "*.gif",
        # "*.gsheet",
        # "*.gz",
        # "*.ico",
        "*.img",
        "*.ipa",
        "*.iso",
        "*.jar",
        # "*.jpeg",
        # "*.jpg",
        "*.lib",
        "*.lock",
        "*.log",
        # "*.mov",
        # "*.mp3",
        # "*.mp4",
        "*.nib",
        "*.node",
        "*.o",
        "*.obj",
        "*.odg",
        # "*.onnx",
        "*.pack",
        # "*.pdf",
        # "*.png",
        # "*.ppt",
        # "*.pptx",
        "*.psd",
        "*.pyc",
        "*.pyd",
        "*.pyo",
        "*.rar",
        "*.rlib",
        "*.rmeta",
        "*.so",
        "*.storyboardc",
        "*.swo",
        "*.swp",
        "*.tar",
        "*.tml",
        "*.ttf",
        "*.war",
        # "*.wav",
        # "*.wmv",
        "*.woff",
        "*.xcarchive",
        # "*.xlsx",
        "*.zip",
        "*.zst",
        # "**/target/debug/*",
        "**/target/debug/**",
        "**/tmp/",
        "*~",
        # "Anaconda3*.sh",
        "babel-webpack",
        "build",
        "CACHEDIR.TAG",
        "Cargo.lock",
        "detritus",  # bion's name for unused code folder
        # "archive",  # another common name for unused code
        "dist",
        "env",
        # "Miniconda3*.sh",
        "node_modules",
        "target",
        "venv",
    }
)

# TODO: incorporate gitignore
# def load_gitignore_patterns(gitignore_path: str) -> set:
#     """Read and parse patterns from a .gitignore file."""
#     patterns = set()
#     with open(gitignore_path, "r") as file:
#         for line in file:
#             stripped_line = line.strip()
#             # Skip comments and empty lines
#             if stripped_line and not stripped_line.startswith("#"):
#                 patterns.add(stripped_line)
#     return patterns

# def update_ignore_frozenset(gitignore_patterns: set) -> frozenset:
#     """Update DEFAULT_IGNORE_FROZENSET with .gitignore patterns."""
#     combined_patterns = DEFAULT_IGNORE_FROZENSET.union(gitignore_patterns)
#     return frozenset(combined_patterns)

DEFAULT_IGNORE = tuple(DEFAULT_IGNORE_FROZENSET)


def _is_all_str(x: Any) -> bool:
    try:
        return all(isinstance(y, str) for y in x)
    except Exception as e:
        debug_print(f"engine._is_all_str Exception {e=}")
        return False


def can_parse(x) -> bool:
    "PUBLIC: check for maybe_ignore/maybe_globs TUPLES"
    if x is None:
        return True
    if isinstance(x, tuple):
        return _is_all_str(x)
    return False


is_parsed_ignore = can_parse


@lru_cache
def parse_ignore(
    maybe_ignore_tuple: Optional[Tuple[str]] = None, override: bool = False
) -> Optional[Tuple[str, ...]]:
    "make_ignore: Give me tuple[str] or give me None"
    debug_print(f"parse_ignore: input {maybe_ignore_tuple=}")
    if maybe_ignore_tuple is None:
        if override:
            return None
        return DEFAULT_IGNORE
    if not isinstance(maybe_ignore_tuple, tuple):
        raise TypeError(f"{maybe_ignore_tuple=} must be None or tuple[str]")
    ignore_set = set()
    for maybe_ignored_item in maybe_ignore_tuple:
        if not isinstance(maybe_ignored_item, str):
            print(f"NOT A str: {maybe_ignored_item=}")
            continue
        ignore_set.add(maybe_ignored_item)
    if not override:
        ignore_set = ignore_set.union(DEFAULT_IGNORE)
    ignore_tuple = tuple(ignore_set)
    # rich_print(f"parsed {ignore_tuple=}")
    return ignore_tuple


@lru_cache
def is_glob(x: str) -> bool:
    "decide if a str is a glob"
    debug_print(f"is {x=} a glob?")
    if match := re.search(r"(\*)?\*|\?|\[(.\-.)+\]", x):
        debug_print(f"{match=}")
        return True
    return False


@lru_cache
def parse_globs(
    maybe_globs_tuple: Optional[Tuple[str]] = None,
) -> Optional[Tuple[str]]:
    "make_globs: Give me tuple[str] as in ('*.rs',) or give me None"
    debug_print(f"make_globs: input {maybe_globs_tuple=}")
    if not maybe_globs_tuple:
        return None
    if not isinstance(maybe_globs_tuple, tuple):
        raise TypeError(f"{maybe_globs_tuple=} must be None or tuple[str]")
    globs_set = set()
    for maybe_glob in maybe_globs_tuple:
        if not is_glob(maybe_glob):
            print(f"NOT A GLOB: {maybe_glob=}")
            continue
        globs_set.add(maybe_glob)
    globs_tuple = tuple(globs_set)
    rich_print(f"parsed {globs_tuple=}")
    return globs_tuple


@dataclass(frozen=True)
class AmortizedGlobs:
    paths: Tuple[Path, ...]
    globs: Tuple[str, ...]
    matches: FrozenSet[Path]


def amortize_globs(
    paths: Tuple[Path, ...], globs: Tuple[str, ...]
) -> Optional[AmortizedGlobs]:
    "amortize glob lookup"
    if not paths:
        debug_print(f"amortize_glob_prefixes: no paths")
        return None
    if not globs:
        debug_print(f"amortize_glob_prefixes: no globs")
        return None
    matches = set()
    for path in paths:
        if not isinstance(path, Path):
            raise TypeError(f"amortize_glob_prefixes not a pathlib.Path {path=}")
        for glob in globs:
            if not isinstance(glob, str):
                raise TypeError(f"amortize_glob_prefixes not a str {glob=}")
            # find matches for this glob in this path, recursively
            for nested_path in path.rglob(glob):
                # skip already seen
                if nested_path in matches:
                    continue
                matches.add(nested_path)
                # add each parent path to the prefix set
                for parent in nested_path.parents:
                    matches.add(parent)
    if not matches:
        debug_print(f"amortize_glob_prefixes None {matches=}")
        return None
    matches = frozenset(matches)
    glob_prefixes = AmortizedGlobs(
        paths=tuple(paths),
        globs=tuple(globs),
        matches=matches,
    )
    debug_print(f"amortize_glob_prefixes {glob_prefixes=}")
    return glob_prefixes


@lru_cache(maxsize=None)
def should_ignore(
    path: Path,
    ignore: Optional[Tuple[str, ...]] = DEFAULT_IGNORE,
    globs: Optional[AmortizedGlobs] = None,
) -> bool:
    "Determine if a given path should be ignored based on ignore FROZENSET"
    if not ignore and not globs:
        debug_print(f"should_ignore NO CHECK NEEDED {path=}")
        return False

    # check the amortized globs first
    if globs and path not in globs.matches:
        debug_print(f"should_ignore: GLOB SKIP {path=}")
        return True

    if ignore:
        # Normalize the path to handle different OS path separators
        normalized_path = os.path.normpath(path)

        # Check each part of the path against the ignore patterns
        for part in normalized_path.split(os.sep):
            if any(fnmatch.fnmatch(part, pattern) for pattern in ignore):
                debug_print(f"should_ignore: IGNORE SKIP {path=}")
                return True

    debug_print(f"should_ignore DO NOT IGNORE {path=}")
    return False


# how to handle globs well?
#
# globs: Optional[Tuple[str]] = None,
# # If globs are provided, check if any part of the path matches any glob pattern
# if globs:
#     if not any(
#         fnmatch.fnmatch(part, pattern)
#         for pattern in globs
#         for part in normalized_path.split(os.sep)
#     ):
#         debug_print(f"should_ignore: Ignored due to not matching globs: {path=}")
#         return True
# filename = os.path.basename(path)
# dir_path = os.path.dirname(path)

# Check if file matches any ignore pattern
# is_ignored = any(fnmatch.fnmatch(filename, pattern) for pattern in ignore) or any(
#     fnmatch.fnmatch(dir_path, pattern) for pattern in ignore
# )
# if is_ignored:
#     debug_print(f"[should_ignore] Ignored due to ignore pattern: {path=}")
#     return True

# # If globs are provided, check if file matches any glob pattern
# if globs:
#     matches_glob = any(
#         fnmatch.fnmatch(filename, pattern) for pattern in globs
#     ) or any(fnmatch.fnmatch(dir_path, pattern) for pattern in globs)
#     if not matches_glob:
#         debug_print(f"[should_ignore] Ignored due to not matching globs: {path=}")
#         return True
