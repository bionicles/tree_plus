# tree_plus_src/ignore.py
from typing import Optional, Union, Set, Tuple
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
}

IgnoreInput = Optional[Union[str, Set[str], Tuple[str]]]
Ignore = Set[str]


def make_ignore(ignore: IgnoreInput) -> Ignore:
    if ignore is None:
        ignore = set()
    elif isinstance(ignore, str):
        ignore = set(ignore.split(","))
    elif isinstance(ignore, tuple):
        ignore = set(ignore)
    elif isinstance(ignore, set):
        pass
    else:
        print(f"{ignore=} {type(ignore)=}")
        raise TypeError("tree_plus ignore arg must be a string, set or None")
    ignore |= DEFAULT_IGNORE
    return ignore


def make_globs(globs: IgnoreInput) -> Ignore:
    if globs is None:
        globs = set()
    elif isinstance(globs, str):
        globs = set(globs.split(","))
    elif isinstance(globs, tuple):
        globs = set(globs)
    elif isinstance(globs, set):
        pass
    else:
        print(f"{globs=} {type(globs)=}")
        raise TypeError("tree_plus globs arg must be a string, set or None")
    return globs


def is_binary_string(bytes):
    textchars = bytearray({7, 8, 9, 10, 12, 13, 27} | set(range(0x20, 0x100)) - {0x7F})
    return bool(bytes.translate(None, textchars))


def is_binary(file_path: str) -> bool:
    """
    Check if a file is binary or not.
    """
    try:
        # read the file in binary mode
        with open(file_path, "rb") as f:
            return is_binary_string(f.read(1024))
    except Exception as e:
        print(f"Error opening file {file_path}: {e}")
        return False


def should_ignore(path: str, ignore: Ignore, globs: Optional[Ignore] = None) -> bool:
    """
    Determine if a given path should be ignored based on ignore and glob patterns.

    If globs is provided, a file must match one of the glob patterns to be included.
    """
    filename = os.path.basename(path)
    dir_path = os.path.dirname(path)

    # Check if file matches any ignore pattern
    is_ignored = any(fnmatch.fnmatch(filename, pattern) for pattern in ignore) or any(
        fnmatch.fnmatch(dir_path, pattern) for pattern in ignore
    )
    if is_ignored:
        debug_print(f"[should_ignore] Ignored due to ignore pattern: {path=}")
        return True

    # If globs are provided, check if file matches any glob pattern
    if globs:
        matches_glob = any(
            fnmatch.fnmatch(filename, pattern) for pattern in globs
        ) or any(fnmatch.fnmatch(dir_path, pattern) for pattern in globs)
        if not matches_glob:
            debug_print(f"[should_ignore] Ignored due to not matching globs: {path=}")
            return True

    return False
