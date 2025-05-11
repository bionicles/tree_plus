# tree_plus_cli.py

### fix for "evil logging.py bug"
# TLDR: if you run a CLI with dependencies that use "logging" from the python standard library
# from within a folder with a "logging.py" 
# python tries to dynamically import the LOCAL logging.py, 
# not the standard library version

def move_cwd_to_end_of_sys_path():
    """
    Locates the current working directory in sys.path and moves it to the end.

    If the current working directory is not found in sys.path, it will be
    appended to the end.
    """
    # print("GEMINI IS COOL!") # used AI to help
    import sys
    import os
    # Get the current working directory
    current_working_directory = os.getcwd()

    # Check if the current working directory is in sys.path
    if current_working_directory in sys.path:
        # Remove it from its current position
        # We iterate through a copy of sys.path to safely modify the original list
        for path_entry in list(sys.path): # Iterate over a copy
            if path_entry == current_working_directory:
                sys.path.remove(path_entry)
        # Append it to the end
        sys.path.append(current_working_directory)
        # print(f"Moved '{current_working_directory}' to the end of sys.path.")
    else:
        # If not found, just append it (optional behavior, could also do nothing)
        sys.path.append(current_working_directory)
        # print(f"'{current_working_directory}' was not in sys.path. Appended it to the end.")

move_cwd_to_end_of_sys_path()

from typing import Optional, Union, Tuple
from time import perf_counter

import click

from tree_plus_src import (  # noqa E402
    enable_debug,
    debug_print,
    __version__,
    engine as tree_plus,
    TreePlus,
    DEFAULT_IGNORE,
    DEFAULT_REGEX_TIMEOUT,
    web,
)
from tree_plus_src.count_tokens_lines import TokenizerName


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
    help="Include ycombinator (False)",
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
    help="max number of steps (depth / level) from root (--yc mode only, default 3)",
    default=3,
)
@click.option(
    "--links",
    "-l",
    "-L",
    help="include links (web mode only, default False)",
    is_flag=True,
)
@click.option(
    "--tiktoken",
    "-t",
    help="a shorthand for tiktoken with the gpt4o tokenizer",
    is_flag=True,
    default=False,
)
@click.option(
    "--tokenizer-name",
    "-T",
    help="name of the tokenizer to use, for now only 'gpt4o' works",
    default=None,
    type=str,
)
@click.option(
    "--timeout",
    help=f"regex timeout in seconds (optional, default {DEFAULT_REGEX_TIMEOUT})",
    default=None,
    type=float,
)
@click.argument("paths", nargs=-1, type=click.UNPROCESSED)  # Accepts multiple arguments
def main(
    # these are NON-MUTUALLY-EXCLUSIVE OPTIONS
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
    tiktoken: bool,
    tokenizer_name: Optional[str],
    timeout: Optional[float],
):
    """A `tree` util enhanced with tokens, lines, and components.

    Wrap patterns in quotes: -i "*.py" / -g "*.rs"

    Example Invocations:

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
        URL + Tag Categories for a website
            > tree_plus example.com

        \b
        URL + Tag Categories for multiple websites with a link tree
            > tree_plus example.com example.org -l

        \b
        Hacker News Mode (3 articles, max depth 3)
            > tree_plus --yc

        \b
        Hacker News Mode (6 articles, max depth 6, warning, slow!)
            > tree_plus --yc -n 6 -m 6

        \b
        Use the Tiktoken gpt4o Model Tokenizer to tokenize Rust files
            > tree_plus -t -g '*.rs'
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

    # TOO SOON! need to support py38, thanks CI/CD testing matrix!
    # _tokenizer_name = TokenizerName.WC
    # match (tiktoken, tokenizer_name):
    #     case (False, None) | (True, "wc"):
    #         pass
    #     case (True, None) | (_, "gpt4o"):
    #         _tokenizer_name = TokenizerName.GPT4O
    #     case (_, "gpt4"):
    #         _tokenizer_name = TokenizerName.GPT4O

    _tokenizer_name = TokenizerName.WC
    if (not tiktoken and tokenizer_name is None) or (
        tiktoken and tokenizer_name == "wc"
    ):
        pass
    elif (tiktoken and tokenizer_name is None) or (tokenizer_name == "gpt4o"):
        _tokenizer_name = TokenizerName.GPT4O
    elif tokenizer_name == "gpt4":
        _tokenizer_name = TokenizerName.GPT4
    else:
        raise ValueError(f"unsupported {tiktoken=} {tokenizer_name=}")

    # guard against negative timeouts
    if timeout is not None and timeout <= 0:
        timeout = None

    root = tree_plus.from_seeds(
        _paths,
        maybe_ignore=ignore,
        maybe_globs=glob,
        syntax_highlighting=syntax,
        override_ignore=override,
        concise=concise,
        tokenizer_name=_tokenizer_name,
        regex_timeout=timeout,
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
    # move_cwd_to_end_of_sys_path()
    main() # type: ignore (click)

# Reminder to those rewriting this in Rust:
# How many commands are there in the `tree_plus` CLI?
# There are ZERO. You just call `tree_plus` [options] [paths].
# If I were to rewrite this with clap, how many commands would there be? ZERO.
# How many subcommands would there be? Zero!
