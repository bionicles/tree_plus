# tree_plus_src/traverse_directory.py
from typing import List
import os

from tree_plus_src.ignore import make_ignore, make_globs, IgnoreInput, should_ignore


def traverse_directory(
    directory_path: str, ignore: IgnoreInput = None, globs: IgnoreInput = None
) -> List[str]:
    """
    Traverse a directory and return a list of all file paths.
    """
    # Correctly expand tilde to home directory path
    directory_path = os.path.expanduser(directory_path)
    if os.path.isfile(directory_path):
        return [directory_path]

    ignore = make_ignore(ignore)
    globs = make_globs(globs)
    file_paths = []

    for root, dirs, files in os.walk(directory_path):
        # modify dirs in-place
        dirs[:] = [d for d in dirs if not should_ignore(d, ignore, globs)]
        # Add empty directories to file_paths
        if not dirs and not files:
            file_paths.append(root)
        for file in files:
            # skip files that are in the ignore list
            if should_ignore(file, ignore, globs):
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
