# src/cli.py
from typing import Optional, Union
from collections import defaultdict
import os
import re

from rich.console import Console
from rich import print as rprint
from rich.tree import Tree
import click

from tree_plus_src import (
    DEFAULT_IGNORE,
    count_tokens_lines,
    TokenLineCount,
    traverse_directory,
    parse_file,
)

console = Console()


@click.command()
@click.argument("directories", default=".")
@click.option(
    "--ignore",
    "-I",
    "-i",
    multiple=True,
    help="Names of files or directories to ignore.",
)
@click.option("--color/--no-color", default=True)
def main(directories, ignore, color):
    ignore = set(ignore)
    ignore |= DEFAULT_IGNORE  # always ignore these
    tree = tree_plus(directories, ignore)
    if color:
        rprint(tree)
    else:
        print(tree_to_string(tree))


def tree_plus(directory: str, ignore: Optional[Union[str, set]] = None) -> Tree:
    "an enhanced tree util with file component leaves and token / line counts"
    # QUESTION: Could this screw up the parent_dir_path in the path_to_tree dict?
    directory = os.path.abspath(directory)

    if ignore is None:
        ignore = DEFAULT_IGNORE
    elif isinstance(ignore, str):
        ignore = set(ignore.split(","))
        ignore |= DEFAULT_IGNORE
    elif isinstance(ignore, set):
        ignore |= DEFAULT_IGNORE
    else:
        raise TypeError("tree_plus ignore arg must be a string, set or None")

    # If the directory argument is a comma-separated string of multiple directories,
    # recursively call tree_plus on each directory and return a combined tree.
    if "," in directory:
        directories = directory.split(",")
        combined_tree = Tree(
            "Multiple Directories:", guide_style="bold cyan", highlight=True
        )
        for dir in directories:
            # strip to remove leading/trailing whitespaces
            dir_tree = tree_plus(dir.strip())
            combined_tree.add(dir_tree)
        return combined_tree

    root_tree = Tree(
        f"{directory} ({0} tokens, {0} lines)",
        guide_style="bold cyan",
        highlight=True,
    )

    # BUG: parent_dir_path is not in path_to_tree. Tests pass but the CLI doesn't run.
    # why do we use this dictionary? what is a better name for path_to_tree?
    path_to_tree = {directory: root_tree}  # Dictionary to map paths to Trees

    file_paths = traverse_directory(directory, ignore)
    total_count = TokenLineCount(n_tokens=0, n_lines=0)

    # Create a defaultdict to aggregate files under their parent directories
    directories = defaultdict(list)

    for file_path in file_paths:
        dir_path, file_name = os.path.split(file_path)
        directories[dir_path].append(file_name)

    # Process directories in lexicographical order of their paths
    sorted_directories = sorted(directories.keys())
    # Check if parent directories exist in path_to_tree, if not add them
    for dir_path in sorted_directories:
        current_path = dir_path
        path_parts = []

        # Traverse up the directory path, building a list of parent directories
        while current_path not in path_to_tree and current_path.startswith(directory):
            path_parts.insert(0, current_path)
            current_path = os.path.dirname(current_path)

        # Now add each part from the list into path_to_tree
        for part in path_parts:
            parent_dir_path = os.path.dirname(part)
            parent_tree = path_to_tree[parent_dir_path]
            dir_tree = parent_tree.add(f":file_folder: {os.path.basename(part)}")
            path_to_tree[part] = dir_tree

    # Here, we sort directories like Linux tree utility
    directories = dict(
        sorted(
            directories.items(),
            key=lambda item: (item[0], sorted(item[1], key=str.lower)),
        )
    )

    for dir_path, files in directories.items():
        dir_tree = path_to_tree[dir_path]

        dir_count = TokenLineCount(n_tokens=0, n_lines=0)
        for file_name in files:
            file_path = os.path.join(dir_path, file_name)
            components = parse_file(file_path)
            file_count = count_tokens_lines(file_path)
            dir_count.n_tokens += file_count.n_tokens
            dir_count.n_lines += file_count.n_lines
            file_label = f":page_facing_up: {file_name}"
            file_label += f" ({file_count.n_tokens} tokens, {file_count.n_lines} lines)"
            file_tree = dir_tree.add(file_label)
            for component in components:
                file_tree.add(component)

        dir_label = f":file_folder: {os.path.basename(dir_path)}"
        dir_label += f" ({dir_count.n_tokens} tokens, {dir_count.n_lines} lines)"
        dir_tree.label = dir_label
        total_count.n_tokens += dir_count.n_tokens
        total_count.n_lines += dir_count.n_lines

    root_tree.label = f":file_folder: {os.path.basename(os.path.abspath(directory))} ({total_count.n_tokens} tokens, {total_count.n_lines} lines)"
    return root_tree


def tree_to_string(tree: Tree) -> str:
    console = Console(force_terminal=True, no_color=True)
    with console.capture() as capture:
        console.print(tree)
    captured_str = capture.get()
    ansi_escape = re.compile(r"\x1B(?:[@-Z\\-_]|\[[0-?]*[ -/]*[@-~])")
    return ansi_escape.sub("", captured_str)


if __name__ == "__main__":
    main()


# def tree_plus(directory: str, ignore: Optional[Union[str, set]] = None) -> Tree:
#     "?"
#     # Resolve the absolute path from the start
#     directory = os.path.abspath(directory)

#     # Handle the ignore argument and unify it into a set
#     if ignore is None:
#         ignore = DEFAULT_IGNORE
#     elif isinstance(ignore, str):
#         ignore = set(ignore.split(","))
#         ignore |= DEFAULT_IGNORE
#     elif isinstance(ignore, set):
#         ignore |= DEFAULT_IGNORE
#     else:
#         raise TypeError("tree_plus ignore arg must be a string, set or None")

#     root_tree = Tree(
#         f"{directory} ({0} tokens, {0} lines)",
#         guide_style="bold cyan",
#         highlight=True,
#     )

#     file_paths = traverse_directory(directory, ignore)
#     total_count = TokenLineCount(n_tokens=0, n_lines=0)

#     # Group files by their directories
#     directories = {}
#     for file_path in file_paths:
#         dir_path, file_name = os.path.split(file_path)
#         if dir_path not in directories:
#             directories[dir_path] = []
#         directories[dir_path].append(file_name)

#     print(f"Directories after grouping files: {directories}")  # Debug print
#     # At this point, directories should be a dict mapping paths to files

#     # Now, we'll construct the tree...
#     for dir_path, files in directories.items():
#         print(f"Processing directory: {dir_path}")  # Debug print
#         basename = os.path.basename(os.path.abspath(dir_path))
#         parent_tree = root_tree  # by default, the parent is the root

#         # If the directory is not the root directory, find its parent
#         if basename and basename != os.path.basename(directory):
#             parent_tree = find_parent_tree(root_tree, basename)

#         dir_count = TokenLineCount(n_tokens=0, n_lines=0)
#         for file_name in files:
#             file_path = os.path.join(dir_path, file_name)
#             components = parse_file(file_path)
#             file_count = count_tokens_lines(file_path)
#             dir_count.n_tokens += file_count.n_tokens
#             dir_count.n_lines += file_count.n_lines
#             file_label = f":page_facing_up: {file_name}"
#             file_label += f" ({file_count.n_tokens} tokens, {file_count.n_lines} lines)"
#             file_tree = parent_tree.add(file_label)
#             for component in components:
#                 file_tree.add(component)

#         if basename and basename != os.path.basename(directory):
#             parent_tree.label += (
#                 f" ({dir_count.n_tokens} tokens, {dir_count.n_lines} lines)"
#             )

#         total_count.n_tokens += dir_count.n_tokens
#         total_count.n_lines += dir_count.n_lines

#     root_tree.label = f"{os.path.abspath(directory)} ({total_count.n_tokens} tokens, {total_count.n_lines} lines)"
#     return root_tree


# def find_parent_tree(root_tree, basename):
#     # Function to find the parent tree of a given directory
#     for child in root_tree.children:
#         if basename in child.label:
#             return child
#     return root_tree


# def tree_plus(directory: str, ignore: Optional[Union[str, set]] = None) -> Tree:
#     "?"
#     # Resolve the absolute path from the start
#     directory = os.path.abspath(directory)
#     if ignore is None:
#         ignore = DEFAULT_IGNORE
#     elif isinstance(ignore, str):
#         ignore = set(ignore.split(","))
#         ignore |= DEFAULT_IGNORE
#     elif isinstance(ignore, set):
#         ignore |= DEFAULT_IGNORE
#     else:
#         raise TypeError("tree_plus ignore arg must be a string, set or None")

#     root_tree = Tree(
#         f"{directory} ({0} tokens, {0} lines)",
#         guide_style="bold cyan",
#         highlight=True,
#     )

#     file_paths = traverse_directory(directory, ignore)
#     total_count = TokenLineCount(n_tokens=0, n_lines=0)

#     # Group files by their directories
#     directories = {}
#     for file_path in file_paths:
#         dir_path, file_name = os.path.split(file_path)
#         if dir_path not in directories:
#             directories[dir_path] = []
#         directories[dir_path].append(file_name)

#     print(f"Directories after grouping files: {directories}")  # Debug print
#     # At this point, directories should be a dict mapping paths to files
#     # Now, we'll construct the tree...
#     for dir_path, files in directories.items():
#         print(f"Processing directory: {dir_path}")  # Debug print
#         basename = os.path.basename(os.path.abspath(dir_path))
#         # If basename is not an empty string, add it as a directory to the tree.
#         # Here, we use the original dir_path value to keep ".." as it is, if that's the case.
#         if basename and basename != os.path.basename(directory):
#             dir_tree = root_tree.add(f":file_folder: {os.path.basename(dir_path)}")
#             dir_count = TokenLineCount(n_tokens=0, n_lines=0)
#             for file_name in files:
#                 file_path = os.path.join(dir_path, file_name)
#                 components = parse_file(file_path)
#                 file_count = count_tokens_lines(file_path)
#                 dir_count.n_tokens += file_count.n_tokens
#                 dir_count.n_lines += file_count.n_lines
#                 file_label = f":page_facing_up: {file_name}"
#                 file_label += (
#                     f" ({file_count.n_tokens} tokens, {file_count.n_lines} lines)"
#                 )
#                 file_tree = dir_tree.add(file_label)
#                 for component in components:
#                     file_tree.add(component)
#             dir_label = f":file_folder: {os.path.basename(dir_path)}"
#             dir_label += f" ({dir_count.n_tokens} tokens, {dir_count.n_lines} lines)"
#             dir_tree.label = dir_label
#             total_count.n_tokens += dir_count.n_tokens
#             total_count.n_lines += dir_count.n_lines

#     root_tree.label = f"{os.path.abspath(directory)} ({total_count.n_tokens} tokens, {total_count.n_lines} lines)"
#     return root_tree
