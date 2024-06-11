# tree_plus_cli.py
from typing import List, Optional, Union, Tuple
from time import perf_counter

import click

from tree_plus_src import (  # noqa E402
    enable_debug,
    debug_print,
    __version__,
    engine as tree_plus,
    TreePlus,
    DEFAULT_IGNORE,
    web,
)


CONTEXT_SETTINGS = dict(help_option_names=["--help", "-h", "-H"])
DEFAULT_QUERY = "best tree data structures"


@click.command(
    context_settings=CONTEXT_SETTINGS,
    epilog=f"""
    \b
    v({__version__}) --- https://github.com/bionicles/tree_plus/blob/main/README.md
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
    "--override",
    "-o",
    "-O",
    is_flag=True,
    default=False,
    help='Override DEFAULT_IGNORE (includes ignored content): -o -i "*.java"',
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
    "--syntax",
    "-s",
    "-S",
    is_flag=True,
    default=False,
    help="Enables Syntax Highlighting (WIP).",
)
@click.option(
    "--concise",
    "-c",
    "-C",
    is_flag=True,
    default=False,
    help="Omit module components. (False)",
)
@click.option(
    "--yc",
    "--hn",
    is_flag=True,
    help=f"Include ycombinator (False)",
    default=False,
)
@click.option(
    "--number",
    "-n",
    "-N",
    help="number of results (--yc mode only, default 3)",
    default=3,
)
@click.option(
    "--max-depth",
    "-m",
    "-M",
    help="maximum number of steps (depth / level) from root (--yc mode only, default 3)",
    default=3,
)
@click.option(
    "--links",
    "-l",
    "-L",
    help="include links (web mode only, default False)",
    is_flag=True,
)
@click.argument("paths", nargs=-1, type=click.UNPROCESSED)  # Accepts multiple arguments
def main(
    glob: Optional[Tuple[str, ...]],
    paths: Optional[Union[str, Tuple[str, ...]]],
    ignore: Tuple[str, ...],
    override: bool,
    debug: bool,
    version: bool,
    syntax: bool,
    concise: bool,
    # web_action: Optional[Tuple[str, ...]],
    # query: Optional[Tuple[str, ...]],
    yc: bool,
    number: int,
    max_depth: int,
    links: bool,
):
    """A `tree` util enhanced with tokens, lines, and components.

    Wrap patterns in quotes: -i "*.py" / -g "*.rs"

    Examples:

        \b
        Show tree_plus_src and tests simultaneously
            > tree_plus tree_plus_src tests

        \b
        Show files matching "*.*s" within tests/more_languages
            > tree_plus -g "*.*s" tests/more_languages

        \b
        Ignore Java files
            > tree_plus -i "*.java" tests

        \b
        Override DEFAULT_IGNORE: Only ignore .ini files.
            > tree_plus -o -i "*.ini" tests/dot_dot

        \b
        Syntax Highlight python files in src and tests
            > tree_plus -s tree_plus_src/*.py tests/*.py

        \b
        Concise Mode (No Parsing)
            > tree_plus -c

        \b
        URL + Tag Categories (tree plus for the web)
            > tree_plus https://en.wikipedia.org/wiki/Binary_search_tree

        \b
        Hacker News Mode (3 articles, max depth 3)
            > tree_plus --yc

        \b
        Hacker News Mode (6 articles, max depth 6)
            > tree_plus --yc -n 6 -m 6
    """
    start_time = perf_counter()
    if debug:
        enable_debug()
    if version:
        print(__version__)
        return
    debug_print(f"tree_plus main received {paths=} {ignore=} {glob=}")

    _paths: Tuple[Union[str, TreePlus], ...] = ()
    if isinstance(paths, str):
        _paths = (paths,)
    elif isinstance(paths, tuple):
        _paths = paths
    assert ignore is None or isinstance(
        ignore, tuple
    ), f"{ignore=} must be None or Tuple[str]"
    assert glob is None or isinstance(
        glob, tuple
    ), f"{glob=} must be None or Tuple[str]"

    og_ignore = ignore
    if not ignore and not override:
        ignore = DEFAULT_IGNORE

    if yc:
        hacker_news_articles = web.articles_from_hacker_news(
            max_depth=max_depth, n_articles=number
        )
        article_comment_tree = tree_plus.from_hacker_news_articles(hacker_news_articles)
        _paths += (article_comment_tree,)

    root = tree_plus.from_seeds(
        _paths,
        maybe_ignore=ignore,
        maybe_globs=glob,
        syntax_highlighting=syntax,
        override_ignore=override,
        concise=concise,
    )
    root.render(markup=True, highlight=True)
    if links:
        root.render_hrefs()
    yc_part = f" yc={yc} n={number} m={max_depth}" if yc else ""
    line1 = f"\n[link=https://github.com/bionicles/tree_plus/blob/main/README.md]tree_plus[/link] v({__version__}){yc_part} ignore={og_ignore} globs={glob}"
    line1 += f" {concise=} {paths=}" if concise else f" {syntax=} {paths=}"
    line2 = f"\n{root.stats()} in {perf_counter() - start_time:.02f} second(s)."
    tree_plus.safe_print(
        line1 + line2,
        # style="bold white on black",
        highlight=True,
        markup=True,
    )


if __name__ == "__main__":
    main()
