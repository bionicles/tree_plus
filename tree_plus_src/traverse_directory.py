# tree_plus_src/traverse_directory.py
from typing import List, Optional, FrozenSet
import os

from tree_plus_src.ignore import should_ignore
from tree_plus_src.debug import debug_print


def traverse_directory(
    directory_path: str,
    maybe_ignore: FrozenSet[str] = None,
    maybe_globs: Optional[FrozenSet[str]] = None,
) -> List[str]:
    """
    Traverse a directory and return a list of all file paths.
    """
    debug_print(f"traverse_directory {directory_path=}")
    if maybe_ignore is not None:
        if not isinstance(maybe_ignore, frozenset):
            raise TypeError(
                f"traverse_directory needs None or FrozenSet[str] {maybe_ignore=}"
            )
    if maybe_globs is not None:
        if not isinstance(maybe_globs, frozenset):
            raise TypeError(
                f"traverse_directory needs None or FrozenSet[str] {maybe_globs=} "
            )

    # Correctly expand tilde to home directory path
    directory_path = os.path.expanduser(directory_path)
    if os.path.isfile(directory_path):
        return [directory_path]

    file_paths = []

    for root, dirs, files in os.walk(directory_path):
        # modify dirs in-place
        dirs[:] = [d for d in dirs if not should_ignore(d, maybe_ignore, maybe_globs)]
        # Add empty directories to file_paths
        if not dirs and not files:
            file_paths.append(root)
        for file in files:
            # skip files that are in the ignore list
            if should_ignore(file, maybe_ignore, maybe_globs):
                continue
            file_paths.append(os.path.join(root, file))

    return file_paths


# def traverse_directory(directory_path: str, ignore: IgnoreInput = None) -> dict:
#     """
#     Traverse a directory and return a dictionary with keys as directory paths
#     and values as lists of file paths in each directory.
#     """
#     ignore = make_ignore(ignore)
#     directory_structure = {}

#     for root, dirs, files in os.walk(directory_path):
#         # Filter directories and files based on ignore patterns
#         dirs[:] = [
#             d for d in dirs if not should_ignore(os.path.join(root, d), ignore, set())
#         ]
#         files = [
#             f for f in files if not should_ignore(os.path.join(root, f), ignore, set())
#         ]

#         if dirs or files:
#             directory_structure[root] = files

#     return directory_structure
