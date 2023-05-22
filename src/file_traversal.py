# file_traversal.py

import os


def traverse_directory(directory_path):
    """
    Traverse a directory and return a list of all file paths.
    """
    if not os.path.isdir(directory_path):
        raise NotADirectoryError(f"{directory_path} is not a directory")

    file_paths = []

    for root, dirs, files in os.walk(directory_path):
        for file in files:
            file_paths.append(os.path.join(root, file))

    return file_paths
