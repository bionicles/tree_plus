# tree_plus_src/ignore.py
from typing import Optional, Union, Set, Tuple, FrozenSet
from functools import lru_cache
import os

import fnmatch

from tree_plus_src.debug import debug_print

DEFAULT_IGNORE = {
    "detritus",
    "__init__.py",
    "__pycache__",
    ".git",
    "*.egg-info",
    ".pytest_cache",
    "node_modules",
    ".hypothesis",
    "babel-webpack",
    ".angular",
    ".vscode",
    "dist",
    "build",
    "venv",
    "env",
    ".idea",
    ".DS_Store",
    ".ipynb_checkpoints",
    ".flake8",
    ".env*",
    "*.log",
    ".cache",
    "*.so",
    "*.dll",
    "*.dylib",
    "*.pyc",
    "*.swp",
    "*.swo",
    "*~",
    "._*",
    ".coverage",
    "*.png",
    "*.jpg",
    "*.jpeg",
    "*.gif",
    "*.ico",
    "*.woff",
    "*.eot",
    "*.ttf",
    "*.zip",
    "*.mp4",
    "*.mp3",
    "*.avi",
    "*.wav",
    "*.mov",
    "*.flv",
    "*.wmv",
    "*_memmap",
    "*.pdf",
    "*.jar",
    "*.pack",
    "*.odg",
    "Anaconda3*.sh",
    "Miniconda3*.sh",
    "*.gz",
    "*.rlib",
    "*.rmeta",
    "*.d",
    "**/target/debug/*",
    "**/target/debug/**",
    ".rustc_info.json",
    "**/tmp/",
    "CACHEDIR.TAG",
    "*.onnx",
    "Cargo.lock",
    "*.o",  # Object files
    "*.a",  # Archive files
    "*.lib",  # Library files
    "*.exe",  # Executable files
    "*.dll",  # Dynamic Link Library files
    "*.bin",  # Binary files
    "target",  # Rust build directory
    "build",  # Common build directory
    "*:Zone.Identifier",
}

IgnoreInput = Optional[Union[str, FrozenSet[str], Tuple[str]]]
Ignore = FrozenSet[str]


@lru_cache
def make_ignore(ignore: IgnoreInput) -> Ignore:
    "CACHED = HASHABLE"
    if ignore is None:
        ignore = frozenset()
    elif isinstance(ignore, str):
        ignore = frozenset(ignore.split(","))
    elif isinstance(ignore, tuple):
        ignore = frozenset(ignore)
    elif isinstance(ignore, frozenset):
        pass
    else:
        print(f"{ignore=} {type(ignore)=}")
        raise TypeError("tree_plus ignore arg must be a string, set or None")
    ignore = ignore.union(DEFAULT_IGNORE)
    return frozenset(ignore)


@lru_cache
def make_globs(globs: IgnoreInput) -> FrozenSet:
    "CACHED = HASHABLE"
    if globs is None:
        globs = frozenset()
    elif isinstance(globs, str):
        globs = frozenset(globs.split(","))
    elif isinstance(globs, tuple):
        globs = frozenset(globs)
    elif isinstance(globs, set):
        globs = frozenset(globs)
    elif isinstance(globs, frozenset):
        pass
    else:
        print(f"{globs=} {type(globs)=}")
        raise TypeError("tree_plus globs arg must be a string, set or None")
    return frozenset(globs)


TEXTCHARS = bytearray({7, 8, 9, 10, 12, 13, 27} | set(range(0x20, 0x100)) - {0x7F})
BINARY_CHECK_SIZE = 1024


@lru_cache()
def is_binary_string(data: bytes) -> bool:
    return bool(data.translate(None, TEXTCHARS))


@lru_cache()
def is_binary(file_path: str) -> bool:
    """
    Check if a file is binary or not.
    """
    try:
        # read the file in binary mode
        with open(file_path, "rb") as f:
            return is_binary_string(f.read(BINARY_CHECK_SIZE))
    except Exception as e:
        print(f"Error opening file {file_path}: {e}")
        return False


@lru_cache(maxsize=None)
def should_ignore(path: str, ignore: Ignore, globs: Optional[Ignore] = None) -> bool:
    "Determine if a given path should be ignored based on ignore and glob patterns."
    # Normalize the path to handle different OS path separators
    normalized_path = os.path.normpath(path)

    # Check each part of the path against the ignore patterns
    for part in normalized_path.split(os.sep):
        if any(fnmatch.fnmatch(part, pattern) for pattern in ignore):
            debug_print(f"[should_ignore] Ignored due to ignore pattern: {path=}")
            return True
    # If globs are provided, check if any part of the path matches any glob pattern
    if globs:
        if not any(
            fnmatch.fnmatch(part, pattern)
            for pattern in globs
            for part in normalized_path.split(os.sep)
        ):
            debug_print(f"[should_ignore] Ignored due to not matching globs: {path=}")
            return True

    return False

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
