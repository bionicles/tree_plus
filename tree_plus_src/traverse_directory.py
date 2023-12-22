# tree_plus_src/traverse_directory.py
from typing import List
import fnmatch
import os

from tree_plus_src.default_ignore import IgnoreInput, make_ignore


def traverse_directory(directory_path: str, ignore: IgnoreInput = None) -> List[str]:
    """
    Traverse a directory and return a list of all file paths.
    """
    # Correctly expand tilde to home directory path
    directory_path = os.path.expanduser(directory_path)
    if os.path.isfile(directory_path):
        return [directory_path]

    ignore = make_ignore(ignore)
    file_paths = []

    for root, dirs, files in os.walk(directory_path):
        # modify dirs in-place
        dirs[:] = [
            d
            for d in dirs
            if not any(fnmatch.fnmatch(d, pattern) for pattern in ignore)
        ]
        # Add empty directories to file_paths
        if not dirs and not files:
            file_paths.append(root)
        for file in files:
            # skip files that are in the ignore list
            if any(fnmatch.fnmatch(file, pattern) for pattern in ignore):
                continue
            file_paths.append(os.path.join(root, file))

    return file_paths
