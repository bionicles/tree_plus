# tree_plus_src/ignore.py
from typing import Optional, Union, Set, Tuple, FrozenSet
from functools import lru_cache
import os

import fnmatch

from tree_plus_src.debug import debug_print

DEFAULT_IGNORE = {
    "__init__.py",
    "__pycache__",
    "._*",
    ".angular",
    ".cache",
    ".coverage",
    ".DS_Store",
    ".env*",
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
    "*.7z",
    "*.a",
    "*.aac",
    "*.ai",
    "*.avi",
    "*.bak",
    "*.bin",
    "*.bz2",
    "*.chk",
    "*.class",
    "*.csv",
    "*.d",
    "*.dat",
    "*.dll",
    "*.doc",
    "*.docx",
    "*.dylib",
    "*.ear",
    "*.egg-info",
    "*.eot",
    "*.eps",
    "*.exe",
    "*.flac",
    "*.flv",
    "*.framework",
    "*.gdoc",
    "*.gif",
    "*.gsheet",
    "*.gz",
    "*.ico",
    "*.img",
    "*.ipa",
    "*.iso",
    "*.jar",
    "*.jpeg",
    "*.jpg",
    "*.lib",
    "*.lock",
    "*.log",
    "*.mov",
    "*.mp3",
    "*.mp4",
    "*.nib",
    "*.node",
    "*.o",
    "*.obj",
    "*.odg",
    "*.onnx",
    "*.pack",
    "*.pdf",
    "*.png",
    "*.ppt",
    "*.pptx",
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
    "*.wav",
    "*.wmv",
    "*.woff",
    "*.xcarchive",
    "*.xlsx",
    "*.zip",
    "*.zst",
    "**/target/debug/*",
    "**/target/debug/**",
    "**/tmp/",
    "*~",
    "Anaconda3*.sh",
    "babel-webpack",
    "build",
    "CACHEDIR.TAG",
    "Cargo.lock",
    "detritus",
    "dist",
    "env",
    "Miniconda3*.sh",
    "node_modules",
    "target",
    "venv",
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
