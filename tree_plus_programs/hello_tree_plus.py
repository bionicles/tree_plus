import inspect

from rich.console import Console
from rich.table import Table
from rich.theme import Theme
import click


console = Console(
    # this avoids a bug with '::' in code
    style="black on rgb(255,255,248)",
    theme=Theme({"repr.ipv6": "default"}),
    width=128,
    soft_wrap=False,
)


class ItHappened:
    "for future reference!"
    pass


@click.command()
def main():
    "A basic example of tree_plus as a library"
    console.print("Attempting to `import tree_plus_src as tp`.")
    # because of this line in pyproject.toml:
    # [tool.setuptools]
    # packages = ["tree_plus_src"] # <--- here
    try:
        import tree_plus_src as tp  # <--- library

        assert inspect.ismodule(tp)
    except Exception as e:
        console.print("[red]Import Failed![/]")
        console.print(f"[red]Exception: {e}[/]")
        console.print('[red]try "pip install -U tree_plus"![/]')
        console.print(
            "[link=https://github.com/bionicles/tree_plus/issues]Clickable link to Tree Plus' GitHub Repo Issues.[/]"
        )
        from rich.traceback import install

        install(show_locals=True)
        raise e
    console.print("\n[green]Success!! Tree Plus Source imported. Proof:[/]\n")

    table = Table(
        title=f'The "tree_plus_src" library v({tp.__version__}):',
        caption="[link=https://github.com/bionicles/tree_plus/blob/main/README.md]Clickable Link to tree_plus' README.md[/link]!",
    )

    table.add_column("Number", justify="right", no_wrap=True)
    table.add_column("Name")

    numbered_entries_sorted = sorted(dir(tp), key=lambda entry: str(entry))
    for n, entry in enumerate(numbered_entries_sorted):
        entry_name = str(entry)
        if entry_name.startswith("_"):
            continue
        table.add_row(str(n), str(entry))

    console.print(table)

    console.print("\nTrees or it didn't happen!")
    tree_for_file = tp.from_seed(__file__, syntax_highlighting=True)
    tree_for_file.render()
    console.print("stats:", tree_for_file.stats())


def trees(
    or_: bool,
    it: int,
    didnt: str,
    happen: tuple,
) -> ItHappened:
    pass


if __name__ == "__main__":
    main()
