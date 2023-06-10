# tree_plus_src/traverse_directory.py
from typing import Optional, List, Set
import fnmatch
import os

from tree_plus_src.default_ignore import DEFAULT_IGNORE


def traverse_directory(directory_path: str, ignore: Optional[Set] = None) -> List[str]:
    """
    Traverse a directory and return a list of all file paths.
    """
    # Convert relative path to absolute path
    # directory_path = os.path.abspath(directory_path)
    if not os.path.isdir(directory_path):
        raise NotADirectoryError(f"{directory_path} is not a directory")

    if ignore is None:
        ignore = DEFAULT_IGNORE
    elif isinstance(ignore, set):
        ignore = ignore | DEFAULT_IGNORE
    else:
        raise TypeError("traverse_directory ignore arg must be a set or None")

    file_paths = []

    for root, dirs, files in os.walk(directory_path):
        dirs[:] = [
            d
            for d in dirs
            if not any(fnmatch.fnmatch(d, pattern) for pattern in ignore)
        ]  # modify dirs in-place
        for file in files:
            # skip files that are in the ignore list
            if any(fnmatch.fnmatch(file, pattern) for pattern in ignore):
                continue
            file_paths.append(os.path.join(root, file))

    return file_paths
