# src/cli.py
import os
import io

from rich.console import Console
from rich import print as rprint
from rich.tree import Tree
import click

from .count_tokens_lines import count_tokens_lines, TokenLineCount
from .traverse_directory import traverse_directory
from .parse_file import parse_file

console = Console()


@click.command()
@click.argument("directories", nargs=-1)
def main(directories):
    for directory in directories:
        tree = tree_plus(directory)
        rprint(tree)


def tree_plus(directory):
    root_tree = Tree(
        f"{directory} ({0} tokens, {0} lines)", guide_style="bold cyan", highlight=True
    )

    file_paths = traverse_directory(directory)
    total_count = TokenLineCount(n_tokens=0, n_lines=0)

    # Group files by their directories
    directories = {}
    for file_path in file_paths:
        dir_path, file_name = os.path.split(file_path)
        if dir_path not in directories:
            directories[dir_path] = []
        directories[dir_path].append(file_name)

    for dir_path, files in directories.items():
        dir_tree = root_tree.add(f":file_folder: {os.path.basename(dir_path)}")
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

    root_tree.label = (
        f"{directory} ({total_count.n_tokens} tokens, {total_count.n_lines} lines)"
    )
    return root_tree


def tree_to_string(tree: Tree) -> str:
    console = Console(file=io.StringIO(), force_terminal=True)
    console.print(tree)
    return console.file.getvalue()


if __name__ == "__main__":
    main()
