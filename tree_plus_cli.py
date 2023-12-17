# tree_plus_cli.py
from collections import defaultdict
from typing import Optional, Union, Tuple, Set, List, Dict
import platform
import glob
import sys
import os
import re

from rich.traceback import install
from rich.console import Console
from rich.tree import Tree
from rich import print as rich_print
import click

DEBUG = 0


def debug_print(*args, **kwargs):
    if DEBUG:
        print("debug:", *args, **kwargs)


install(show_locals=True)

from tree_plus_src import (  # noqa E402
    traverse_directory,
    count_tokens_lines,
    add_tokens_lines,
    TokenLineCount,
    make_ignore,
    parse_file,
    IgnoreInput,
    Ignore,
)

console = Console()


def tree_to_string(tree: Tree) -> str:
    console = Console(force_terminal=True, no_color=True)
    with console.capture() as capture:
        console.print(tree)
    captured_str = capture.get()
    ansi_escape = re.compile(r"\x1B(?:[@-Z\\-_]|\[[0-?]*[ -/]*[@-~])")
    return ansi_escape.sub("", captured_str)


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


Collection = Union[List, Tuple, Set]
PathOrPaths = Union[str, Tuple[str]]
PathsInput = Optional[PathOrPaths]
Paths = Tuple[str]

operate_normally = platform.system() != "Windows" or sys.stdout.encoding != "cp1252"
root_char = ":cactus:" if operate_normally else "[multiple paths]"
folder_char = ":file_folder:" if operate_normally else "[folder]"
file_char = ":page_facing_up:" if operate_normally else "[file]"
glob_char = ":cyclone:" if operate_normally else "[glob]"


@click.command()
@click.argument("paths", nargs=-1)  # Accepts multiple arguments
@click.option(
    "--ignore",
    "-I",
    "-i",
    multiple=True,
    help="Names of files or directories to ignore.",
)
def main(paths: PathsInput, ignore: IgnoreInput):
    debug_print(f"tree_plus main received {paths=} {ignore=}")
    ignore = make_ignore(ignore)
    path_or_paths = paths or "."
    tree = tree_plus(path_or_paths, ignore)
    safe_print(tree)


def tree_plus(
    path_or_paths: Union[str, Tuple[str]], ignore: IgnoreInput = None
) -> Tree:
    """An enhanced tree util with file component leaves and token/line counts."""
    ignore: Ignore = make_ignore(ignore)
    paths: Paths = _parse_paths(path_or_paths)
    tree: Tree = _handle_paths(paths, ignore)
    return tree


# Idea: instead of eagerly extending paths from globs,
# we could use globs to signal the need to find their shared common ancestor
def _parse_paths(path_or_paths: Union[str, Tuple[str]]) -> Tuple[str]:
    """Expands globs and splits comma-separated paths using Path."""
    paths = (path_or_paths,) if isinstance(path_or_paths, str) else path_or_paths
    debug_print(f"_parse_paths 1st {paths=}")
    # Flatten nested lists, tuples, and sets
    paths = flatten_to_str(paths)
    debug_print(f"_parse_paths 2nd {paths=}")

    # Remove empty paths
    paths = tuple(path.strip() for path in paths if path.strip())
    debug_print(f"_parse_paths 3rd {paths=}")

    parsed_paths = set()
    for path in paths:
        if "," in path:
            debug_print(f", in path {path}")
            # Split comma-separated paths
            for split_path in path.split(","):
                parsed_paths.add(split_path.strip())
        else:
            parsed_paths.add(path)
    paths = tuple(parsed_paths)
    rich_print(f"{paths=}")
    return paths


def flatten_to_str(collection: Collection):
    flat_list = []
    for item in collection:
        if isinstance(item, (list, tuple, set)):
            flat_list.extend(flatten_to_str(item))
        else:
            flat_list.append(str(item))
    return flat_list


def _handle_paths(paths: Tuple[str], ignore: Ignore) -> Tree:
    """handle multiple paths to generate a tree with deduplicated intermediate folders"""
    debug_print(f"_handle_paths {paths=} {ignore=}")

    # guard for null input
    if not paths:
        raise TypeError("zero paths provided, cannot handle zero paths")

    # single path case
    if len(paths) == 1:
        path = paths[0]
        debug_print(f"only 1 path {path=}")
        tree, _ = _handle_path(path, ignore, {})
        return tree

    # multiple path case
    root = Tree(
        f"{root_char} {'Multiple Paths'}",
        guide_style="bold cyan",
        highlight=True,
    )
    total_count = TokenLineCount(0, 0)

    # Create a dictionary of file paths to trees and counts
    paths_to_trees = {"root": (root, total_count)}

    for path in paths:
        path_tree, path_count = _handle_path(path, ignore, paths_to_trees)
        total_count = add_tokens_lines(total_count, path_count)
        root.add(path_tree)

    root.label = (
        f"{root_char} Root ({total_count.n_tokens} tokens, {total_count.n_lines} lines)"
    )
    return root


def _handle_path(
    path: str, ignore: Ignore, paths_to_trees: dict
) -> Tuple[Tree, TokenLineCount]:
    """Handle a single path, generating a tree and calculating tokens/lines."""
    debug_print(f"_handle_path {path=}  {ignore=}")

    # save the original path, just in case
    og_path = path
    # Normalize path to resolve '..' and similar relative paths
    path = os.path.abspath(path)

    # Handle glob paths
    if "*" in path:
        try:
            glob_paths = glob.glob(path)
            # glob_commonpath = os.path.commonpath(glob_paths)
            glob_root_count = TokenLineCount(0, 0)
            glob_root = Tree(
                f"{glob_char} {og_path}",
                guide_style="bold cyan",
            )
            for glob_focus in glob_paths:
                glob_focus_tree, glob_node_count = _handle_path(
                    glob_focus, ignore, paths_to_trees
                )
                glob_root.add(glob_focus_tree)
                glob_root_count = add_tokens_lines(glob_root_count, glob_node_count)
            glob_root.label += (
                f" ({glob_root_count.n_tokens} tokens, {glob_root_count.n_lines} lines)"
            )
            return glob_root, glob_root_count
        except ValueError:
            raise ValueError(f"Invalid glob pattern: {path}")

    # Handle paths to files:
    elif os.path.isfile(path):
        file_path = path
        if file_path in paths_to_trees:
            return paths_to_trees[file_path]
        # Handle file input
        components = parse_file(file_path)
        file_count = count_tokens_lines(file_path)
        file_tree = Tree(
            f"{file_char} {os.path.basename(file_path)} ({file_count.n_tokens} tokens, {file_count.n_lines} lines)",
            guide_style="bold cyan",
            highlight=True,
        )
        for component in components:
            file_tree.add(component)
        paths_to_trees[file_path] = (file_tree, file_count)
        return file_tree, file_count
    # Handle paths to folders:
    else:
        folder_path = path

        if folder_path in paths_to_trees:
            # Already processed, reuse existing tree
            return paths_to_trees[folder_path]

        root_tree = Tree(
            f"{folder_path} ({0} tokens, {0} lines)",
            guide_style="bold cyan",
            highlight=True,
        )
        root_count = TokenLineCount(n_tokens=0, n_lines=0)

        # Dictionary to map paths to Trees
        paths_to_trees[folder_path] = (root_tree, root_count)

        file_paths = traverse_directory(folder_path, ignore)

        directories = defaultdict(list)

        for file_path in file_paths:
            dir_path, file_name = os.path.split(file_path)
            directories[dir_path].append(file_name)

        # Process directories in lexicographical order of their paths
        sorted_directories_keys = sorted(directories.keys())
        # Check if parent directories exist in paths_to_trees, if not add them
        for dir_path in sorted_directories_keys:
            current_path = dir_path
            path_parts = []

            # Traverse up the folder_path path, building a list of parent directories
            while current_path not in paths_to_trees and current_path.startswith(
                folder_path
            ):
                path_parts.insert(0, current_path)
                current_path = os.path.dirname(current_path)

            # Now add each part from the list into paths_to_trees
            for part in path_parts:
                parent_dir_path = os.path.dirname(part)
                # NOTE: parent_count unused, is that ok?
                parent_tree, _parent_count = paths_to_trees[parent_dir_path]
                dir_tree = parent_tree.add(f"{folder_char} {os.path.basename(part)}")
                dir_count = TokenLineCount(n_tokens=0, n_lines=0)
                paths_to_trees[part] = (dir_tree, dir_count)

        # Here, we sort the directories in a similar way to the Linux tree utility:
        sorted_directories = {
            dir_path: sorted(files, key=str.lower)
            for dir_path, files in sorted(directories.items(), key=lambda item: item[0])
        }
        # Now, we build the tree:
        for dir_path, files in sorted_directories.items():
            dir_tree, dir_count = paths_to_trees[dir_path]

            for file_name in files:
                # handle a file with a recursive call
                file_path = os.path.join(dir_path, file_name)
                file_tree, file_count = _handle_path(file_path, ignore, paths_to_trees)
                dir_count = add_tokens_lines(dir_count, file_count)
                dir_tree.add(file_tree)

            dir_label = f"{folder_char} {os.path.basename(dir_path)}"
            dir_label += f" ({dir_count.n_tokens} tokens, {dir_count.n_lines} lines)"
            dir_tree.label = dir_label
            root_count = add_tokens_lines(root_count, dir_count)

            paths_to_trees[dir_path] = (dir_tree, dir_count)

        return root_tree, root_count


if __name__ == "__main__":
    main()
