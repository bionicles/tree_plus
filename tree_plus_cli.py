# tree_plus_cli.py
from collections import defaultdict
from unidecode import unidecode
from typing import Optional, Union
import platform
import sys
import os
import re

from rich.console import Console
from rich import print as rich_print
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


def tree_to_string(tree: Tree) -> str:
    console = Console(force_terminal=True, no_color=True)
    with console.capture() as capture:
        console.print(tree)
    captured_str = capture.get()
    ansi_escape = re.compile(r"\x1B(?:[@-Z\\-_]|\[[0-?]*[ -/]*[@-~])")
    return ansi_escape.sub("", captured_str)


# another option would potentially lose information
# def clean_string(input_str):
#     return input_str.encode("ascii", errors="replace").decode("ascii")


def clean_string(input_str):
    return input_str.encode("unicode-escape").decode("ascii")


def safe_print(tree):
    try:
        # Attempt to print the tree normally
        rich_print(tree)
    except UnicodeEncodeError:
        try:
            # Attempt to print a cleaned version of the tree
            print(clean_string(tree_to_string(tree)))
        except Exception as e:
            # If all else fails, print an error message
            print("An error occurred when attempting to print the tree.")
            print(e)


# passing on ubuntu and mac
# def safe_print(tree):
#     if console.is_terminal:
#         try:
#             rich_print(tree)
#         except UnicodeEncodeError:
#             print(unidecode(tree_to_string(tree)))
#     else:
#         print(tree_to_string(tree))


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
    safe_print(tree)


def tree_plus(directory: str, ignore: Optional[Union[str, set]] = None) -> Tree:
    "an enhanced tree util with file component leaves and token / line counts"

    # Check for platform and encoding
    operate_normally = not (
        platform.system() == "Windows" and sys.stdout.encoding == "cp1252"
    )

    # Use special characters or alternatives based on the check
    folder_char = ":file_folder:" if operate_normally else "[folder]"
    file_char = ":page_facing_up:" if operate_normally else "[file]"

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
    sorted_directories_keys = sorted(directories.keys())
    # Check if parent directories exist in path_to_tree, if not add them
    for dir_path in sorted_directories_keys:
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
            dir_tree = parent_tree.add(f"{folder_char} {os.path.basename(part)}")
            path_to_tree[part] = dir_tree

    # Here, we sort directories like Linux tree utility
    sorted_directories = {
        dir_path: sorted(files, key=str.lower)
        for dir_path, files in sorted(directories.items(), key=lambda item: item[0])
    }
    for dir_path, files in sorted_directories.items():
        dir_tree = path_to_tree[dir_path]

        dir_count = TokenLineCount(n_tokens=0, n_lines=0)
        for file_name in files:
            file_path = os.path.join(dir_path, file_name)
            components = parse_file(file_path)
            file_count = count_tokens_lines(file_path)
            dir_count.n_tokens += file_count.n_tokens
            dir_count.n_lines += file_count.n_lines
            file_label = f"{file_char} {file_name}"
            file_label += f" ({file_count.n_tokens} tokens, {file_count.n_lines} lines)"
            file_tree = dir_tree.add(file_label)
            for component in components:
                file_tree.add(component)

        dir_label = f"{folder_char} {os.path.basename(dir_path)}"
        dir_label += f" ({dir_count.n_tokens} tokens, {dir_count.n_lines} lines)"
        dir_tree.label = dir_label
        total_count.n_tokens += dir_count.n_tokens
        total_count.n_lines += dir_count.n_lines

    root_tree.label = f"{folder_char} {os.path.basename(os.path.abspath(directory))} ({total_count.n_tokens} tokens, {total_count.n_lines} lines)"
    return root_tree


if __name__ == "__main__":
    main()
