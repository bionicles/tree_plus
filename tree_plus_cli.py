# tree_plus_cli.py
from typing import Optional, Union, Tuple
from time import perf_counter

import click

from tree_plus_src import (  # noqa E402
    enable_debug,
    debug_print,
    __version__,
    engine as tree_plus,
    DEFAULT_IGNORE,
)


CONTEXT_SETTINGS = dict(help_option_names=["--help", "-h", "-H"])


@click.command(
    context_settings=CONTEXT_SETTINGS,
    epilog=f"""
    \b
    (v{__version__}) --- https://github.com/bionicles/tree_plus
""",
)
@click.option(
    "--ignore",
    "-i",
    "-I",
    multiple=True,
    help='Patterns to ignore, in quotes: -i "*.java"',
)
@click.option(
    "--glob",
    "-g",
    "-G",
    multiple=True,
    help='Patterns to find, in quotes: -g "*.rs"',
)
@click.option(
    "--version",
    "-v",
    "-V",
    is_flag=True,
    default=False,
    help="Print the version and exit.",
)
@click.option(
    "--debug",
    "-d",
    "-D",
    is_flag=True,
    default=False,
    help="Enables $DEBUG_TREE_PLUS.",
)
@click.option(
    "--lex",
    "-l",
    "-L",
    is_flag=True,
    default=False,
    help="Enables Pygments Lexing (WIP).",
)
@click.argument("paths", nargs=-1, type=click.UNPROCESSED)  # Accepts multiple arguments
def main(
    glob: Optional[Tuple[str]],
    paths: Optional[Union[str, Tuple[str]]],
    ignore: Tuple[str],
    debug: bool,
    version: bool,
    lex: bool,
):
    """A `tree` util enhanced with tokens, lines, and components.

    Wrap glob patterns in quotes: -i "*.py" / -g "*.rs"

    Examples:

        \b
        Show tree_plus_src and tests simultaneously
            > tree_plus tree_plus_src tests

        \b
        Show files matching "*.*s" within tests/more_languages
            > tree_plus -g "*.*s" tests/more_languages

        \b
        Ignore Java files
            > tree_plus tests -i "*.java"
    """
    if debug:
        enable_debug()
    if version:
        print(__version__)
        return
    debug_print(f"tree_plus main received {paths=} {ignore=} {glob=}")

    if isinstance(paths, str):
        paths = (paths,)
    assert ignore is None or isinstance(
        ignore, tuple
    ), f"{ignore=} must be None or Tuple[str]"
    assert glob is None or isinstance(
        glob, tuple
    ), f"{glob=} must be None or Tuple[str]"
    if not ignore:
        ignore = DEFAULT_IGNORE
    start_time = perf_counter()
    root = tree_plus.from_seeds(
        paths, maybe_ignore=ignore, maybe_globs=glob, syntax_highlighting=lex
    )
    root.render()
    print(f"\n{root.stats()} in {perf_counter() - start_time:.02f} seconds.")


if __name__ == "__main__":
    main()
