from typing import (
    Dict,
    FrozenSet,
    Iterable,
    List,
    Literal,
    Sequence,
    Tuple,
    Optional,
    Union,
)
from dataclasses import dataclass, field
from functools import lru_cache
from itertools import chain
from pathlib import Path
from enum import Enum
import platform
import sys
import os
import re

# time limit on file parsing makes sense
# from func_timeout import func_timeout
from rich.console import Console
from rich.pretty import Pretty
from rich.syntax import Syntax
from rich.theme import Theme
from rich.table import Table
from rich.tree import Tree
from rich.text import Text
from rich.markdown import Markdown
import requests
from rich.markdown import Markdown
from fake_useragent import UserAgent
from bs4 import Tag, NavigableString

from bs4 import BeautifulSoup, PageElement

from natsort import os_sorted


from tree_plus_src import (
    debug_print,
    parse_file,
    count_tokens_lines,
    count_tokens_lines_from_contents,
    TokenLineCount,
    should_ignore,
    DEFAULT_IGNORE,
    is_glob,
    is_parsed_ignore,
    AmortizedGlobs,
    amortize_globs,
    parse_ignore,
)

# MAX ALLOWED PARSE_FILE RUNTIME HERE
# TODO: MOVE TIMEOUT_SECONDS TO ENV VAR & CLI INPUT
TIMEOUT_SECONDS = 0.5


# emojis here
operate_normally = platform.system() != "Windows" or sys.stdout.encoding != "cp1252"
ROOT_CHAR = ":cactus:" if operate_normally else "[root]"
FOLDER_CHAR = ":file_folder:" if operate_normally else "[folder]"
FILE_CHAR = ":page_facing_up:" if operate_normally else "[file]"
GLOB_CHAR = ":cyclone:" if operate_normally else "[glob]"
URL_CHAR = ":spider_web:" if operate_normally else "[url]"
TAG_CHAR = ""

colors = {
    "colorblind_gold": "#FFC20A",
    "colorblind_blue": "#0C7BDC",
    "fde": "#877348",
    "cyberpink": "#d600ff",
    "cybercyan": "#00b8ff",
}

BLACK = "black"
BLUE = colors["colorblind_blue"]
GOLD = colors["colorblind_gold"]
FDE = colors["fde"]
CYBERPINK = colors["cyberpink"]
CYBERCYAN = colors["cybercyan"]

# NOTE: you can customize the color here, and we could make this functional
LINK_COLOR = f"{BLUE} on black"
TEXT_COLOR = f"{GOLD} on black"

THEME = {
    "repr.ipv6": "default",
    "repr.ipv4": "default",
    "repr.call": colors["colorblind_blue"],
    "repr.str": colors["colorblind_blue"],
    "repr.tag_name": colors["colorblind_blue"],
}


class Category(Enum):
    "PUBLIC: TreePlus"
    ROOT = 1
    GLOB = 2
    FOLDER = 3
    FILE = 4
    COMPONENT = 5
    URL = 6
    TAG = 7


from rich.pretty import Pretty
from rich.panel import Panel


@dataclass
class TreePlus:
    "PUBLIC: data structure"
    category: Category = Category.COMPONENT
    name: Union[str, Panel, Text, Markdown, Table, Pretty] = ""
    n_folders: int = 0
    n_files: int = 0
    n_lines: Optional[int] = 0
    n_tokens: Optional[int] = 0
    # TODO: clarify subtree types -- make this a DataFrame tbh
    subtrees: Union[
        List["TreePlus"],
        List[Syntax],
        List[Markdown],
        List[Table],
        List[Text],
        List[str],
    ] = field(default_factory=list)
    hrefs: Optional[Dict[str, list]] = None

    def is_root(self) -> bool:
        "Category.ROOT"
        return self.category is Category.ROOT

    def is_folder(self) -> bool:
        "Category.FOLDER"
        return self.category is Category.FOLDER

    def is_file(self) -> bool:
        "Category.FILE"
        return self.category is Category.FILE

    def is_glob(self) -> bool:
        "Category.GLOB"
        return self.category is Category.GLOB

    def is_component(self) -> bool:
        "Category.COMPONENT"
        return self.category is Category.COMPONENT

    def is_url(self) -> bool:
        "Category.URL"
        return self.category is Category.URL

    def into_rich_tree(self) -> Tree:
        "PUBLIC: Convert a TreePlus into a rich.tree.Tree to render"
        return into_rich_tree(root=self)

    def into_str(self) -> str:
        "PUBLIC: Convert a TreePlus into a rich.tree.Tree to render"
        return tree_to_string(into_rich_tree(root=self))

    def render(
        self,
        style: Optional[str] = None,
        highlight: bool = False,
        markup: bool = True,
        capturing: bool = False,
    ):
        "PUBLIC: Safely print a TreePlus"
        _inner_tree: Tree = into_rich_tree(root=self)
        safe_print(
            _inner_tree,
            style=style,
            highlight=highlight,
            markup=markup,
            capturing=capturing,
        )

    def render_hrefs(self):

        subtree_href_tree_pluses = []
        if (
            self.hrefs
            and "node_index_str" in self.hrefs
            and self.hrefs["node_index_str"]
        ):
            hrefs_tree_plus = from_hrefs(self.hrefs, root_panel_text=self.name)
            subtree_href_tree_pluses.append(hrefs_tree_plus)
        elif self.subtrees:
            for i, subtree in enumerate(self.subtrees, start=1):
                if isinstance(subtree, TreePlus) and subtree.hrefs is not None:
                    hrefs_tree_plus = from_hrefs(
                        subtree.hrefs, root_panel_text=self.name
                    )
                    subtree_href_tree_pluses.append(hrefs_tree_plus)

        if not subtree_href_tree_pluses:
            return

        from rich import print as rprint

        hrefs_root = TreePlus(
            Category.URL,
            Panel("Links", expand=False),
            subtrees=subtree_href_tree_pluses,
        )
        hrefs_rich_tree = hrefs_root.into_rich_tree()
        rprint(hrefs_rich_tree)

    def stats(self) -> str:
        "PUBLIC: statistics"
        return stats(self)


def from_hrefs(
    hrefs: dict,
    root_panel_text,
    link_color: str = LINK_COLOR,
) -> Optional[TreePlus]:
    root = empty_tag_tree()
    # hrefs_table = Table("href_n", "node_index", "href", highlight=True)
    subtrees: List[TreePlus] = []
    if not hrefs["node_index_str"]:
        return None

    href_n = 1
    for node_index_str, href in zip(hrefs["node_index_str"], hrefs["href"]):
        if href.startswith("#"):
            continue
        node = empty_tag_tree()
        # node.name = f"{node_index_str} {href}"
        node.name = Panel(
            f"[{link_color}]{href}[/{link_color}]",
            title=f"[{href_n}] at {node_index_str}",
            title_align="left",
            expand=False,
        )
        subtrees.append(node)
        href_n += 1

    # hrefs_table_panel = "Links"
    if root_panel_text is None:
        root_panel_text = "Links"
    if isinstance(root_panel_text, str):
        root_panel_text = Panel(
            f"[{link_color}]{root_panel_text}[/{link_color}]",
            title=f"[0] at ()",
            title_align="left",
            expand=False,
        )
    root.name = root_panel_text
    root.subtrees = subtrees
    return root


def stats(tree: TreePlus) -> str:
    debug_print("stats:")
    debug_print("tree.n_folders:", tree.n_folders)
    debug_print("tree.n_files:", tree.n_files)
    debug_print("tree.n_lines:", tree.n_lines)
    debug_print("tree.n_tokens:", tree.n_tokens)
    if tree.n_folders is None:
        folders = "(?)"
    else:
        folders = f"{tree.n_folders:,}"
    if tree.n_files is None:
        files = "(?)"
    else:
        files = f"{tree.n_files:,}"
    if tree.n_lines is None:
        lines = "(?)"
    else:
        lines = f"{tree.n_lines:,}"
    if tree.n_tokens is None:
        tokens = "(?)"
    else:
        tokens = f"{tree.n_tokens:,}"
    try:
        stats = (
            f"{folders} folder(s), {files} file(s), {lines} line(s), {tokens} token(s)"
        )
        return stats
    except Exception as e:
        debug_print(f"Exception e: {type(e)} = {e}")
    return ""


@lru_cache
def remove_trailing_space(x: str) -> str:
    # debug_print("remove_trailing_space")
    # debug_print(f"'{x=}'")
    y = re.sub(r" +\n", "\n", x)
    # debug_print(f"'{y=}'")
    return y


def tree_to_string(
    tree: Tree,
    markup: bool = False,
    highlight: bool = False,
    text_style: str = TEXT_COLOR,
) -> str:
    console = Console(
        # force_terminal=True,
        no_color=True,
        # soft_wrap=True,
        markup=markup,
        highlight=highlight,
        style=text_style,
        theme=Theme({"repr.ipv6": "default"}),  # maybe unnecessary given no_color
    )
    with console.capture() as capture:
        console.print(tree, markup=markup, highlight=highlight)
    captured_str = capture.get()
    # captured_str = ansi_escape.sub("", captured_str)
    captured_str = remove_trailing_space(captured_str)
    return captured_str


def clean_string(input_str: str) -> str:
    return input_str.encode("unicode-escape").decode("ascii")


def safe_print(
    tree: Union[Tree, Panel, str],
    style: Optional[str] = None,
    highlight: bool = True,
    markup: bool = False,
    capturing: bool = False,
):
    try:
        console = Console(
            # reduce the tab size to fit content
            tab_size=2,
            # NOTE: sometimes we need wider trees (deploy)
            width=128 if os.environ.get("TREE_PLUS_UPDATE_README") == "YES" else None,
            markup=markup,
            highlight=highlight,
            style=style,
            theme=Theme(THEME),
        )
        # Attempt to print the tree normally
        if capturing:
            with console.capture() as capture:
                console.print(tree)
            return capture.get()
        else:
            console.print(
                tree,
                highlight=highlight,
                markup=markup,
                style=style,
            )
    except UnicodeEncodeError as e:
        debug_print(f"UnicodeEncodeError printing tree normally: ", e)
        try:
            debug_print(f"Attempt to print a cleaned version of the tree:")
            if isinstance(tree, Tree):
                tree_string = tree_to_string(tree, markup=markup, highlight=highlight)
            else:
                tree_string = tree
            # debug_print(f"{tree_string=}")
            if isinstance(tree_string, str):
                clean_tree_string = clean_string(tree_string)
            else:
                clean_tree_string = tree_string
            # debug_print(f"{clean_tree_string=}")
            print(clean_tree_string)
        except Exception as e:
            print("An error occurred when attempting to print the tree.")
            print(e)


def _make_rich_tree(
    label,
    style: str = TEXT_COLOR,
    guide_style: str = LINK_COLOR,
    highlight: bool = True,
) -> Tree:
    "PRIVATE: build 1 renderable rich.tree.Tree instance"
    return Tree(
        label,
        style="bold " + style,
        guide_style=guide_style,
        highlight=highlight,
    )


def into_rich_tree(*, root: Optional[TreePlus] = None) -> Tree:
    "PUBLIC: Convert a TreePlus into a rich.tree.Tree to render"
    if not isinstance(root, TreePlus):
        raise TypeError(f"tree_plus.into_rich_tree got non-TreePlus {root=}")
    rich_tree = None
    if root.category is Category.FILE:
        label = f"{FILE_CHAR} {root.name}"
        if root.n_tokens is not None and root.n_lines is not None:
            label += f" ({root.n_tokens:,} token{'' if root.n_tokens == 1 else 's'}, {root.n_lines:,} line{'' if root.n_lines == 1 else 's'})"
        elif root.n_tokens is None and root.n_lines is not None:
            label += f" ({root.n_lines:,} line{'' if root.n_lines == 1 else 's'})"
        rich_tree = _make_rich_tree(label)
        for subtree in root.subtrees:
            rich_tree.add(subtree)  # type: ignore
    elif root.category is Category.FOLDER:
        label = f"{FOLDER_CHAR} {root.name} ({root.n_folders:,} folder{'' if root.n_folders == 1 else 's'}, {root.n_files:,} file{'' if root.n_files == 1 else 's'})"
        rich_tree = _make_rich_tree(label)
        for subtree in root.subtrees:
            # RECURSION HERE
            rich_subtree = into_rich_tree(root=subtree)  # type: ignore
            rich_tree.add(rich_subtree)
        counts = f" "
        rich_tree.label += counts  # type: ignore
    elif root.category is Category.ROOT:
        label = f"{ROOT_CHAR} {root.name}"
        if root.n_tokens and root.n_lines:
            label += f"({root.n_folders:,} folder{'' if root.n_folders == 1 else 's'}, {root.n_files:,} file{'' if root.n_files == 1 else 's'})"
        rich_tree = _make_rich_tree(label)
        for subtree in root.subtrees:
            rich_subtree = into_rich_tree(root=subtree)  # type: ignore
            rich_tree.add(rich_subtree)
    elif root.category is Category.GLOB:
        n_matches = len(root.subtrees)
        label = f"{GLOB_CHAR} {root.name} ({n_matches:,} match{'' if n_matches == 1 else 'es'})"
        rich_tree = _make_rich_tree(label)
        for subtree in root.subtrees:
            rich_subtree = into_rich_tree(root=subtree)  # type: ignore
            rich_tree.add(rich_subtree)
    elif root.category is Category.URL:
        n_matches = len(root.subtrees)
        if isinstance(root.name, str):
            label = f"{URL_CHAR}  {root.name}"
            if root.n_tokens and root.n_lines:
                label += f" ({root.n_tokens:,} token{'' if root.n_tokens == 1 else 's'}, {root.n_lines:,} line{'' if root.n_lines == 1 else 's'})"
        else:
            label = root.name
        rich_tree = _make_rich_tree(label)
        for subtree in root.subtrees:
            if isinstance(subtree, TreePlus):
                # subtree = tree_to_string(subtree.into_rich_tree())
                rich_subtree = into_rich_tree(root=subtree)  # type: ignore
                rich_tree.add(rich_subtree)
            elif isinstance(
                subtree, (str, Markdown, Text, Syntax, Table, Pretty, Panel)
            ):
                rich_tree.add(subtree)
                # rich_subtree = into_rich_tree(root=subtree)  # type: ignore
    elif root.category is Category.TAG:
        if isinstance(root.name, str):
            label = f"{TAG_CHAR} {root.name}"
        else:
            label = root.name
        rich_tree = _make_rich_tree(label)
        for subtree in root.subtrees:
            rich_subtree = into_rich_tree(root=subtree)  # type: ignore
            rich_tree.add(rich_subtree)
    if not rich_tree:
        raise TypeError(f"engine.into_rich_tree: unsupported {root=}")
    return rich_tree


def is_url(x: str) -> bool:
    x_is_url = (
        x.startswith("http://")
        or x.startswith("https://")
        or x.startswith("www.")
        or x.endswith(".com")
        or x.endswith(".org")
    )
    debug_print(f"{x=} {'IS' if x_is_url else 'IS NOT'} a url")
    return x_is_url


@lru_cache
def categorize(
    x: Union[Path, Tuple[str], str],
    check_strs_globs: bool = True,
    check_strs_paths: bool = True,
    check_strs_urls: bool = True,
    raise_if_component: bool = True,
) -> Category:
    "assign x to a category enum DEFAULTS TO CHECK GLOBS AND PATHS"
    y = None
    # DECISION BEGINS
    if isinstance(x, str):  # STR first, happy path
        # could deactivate checking for performance
        y = Category.COMPONENT
        if check_strs_urls and is_url(x):
            y = Category.URL
        elif check_strs_globs and is_glob(x):
            y = Category.GLOB
        elif y is not Category.GLOB and check_strs_paths:
            if os.path.exists(x):
                if os.path.isfile(x):
                    y = Category.FILE
                elif os.path.isdir(x):
                    y = Category.FOLDER
    elif isinstance(x, Path):
        if x.is_file():
            y = Category.FILE  # Fixed
        elif x.is_dir():
            y = Category.FOLDER
    elif isinstance(x, tuple):
        if all(isinstance(y, str) for y in x):
            y = Category.ROOT
        else:
            raise TypeError(
                f"engine.categorize found non-str seed in aspirant root {x=}"
            )
    else:
        raise TypeError(
            "engine.categorize not pathlib.Path, tuple[str] or str (glob|path|code) input {x=}"
        )
    # DECISION ENDS
    if y:
        if raise_if_component and y is Category.COMPONENT:
            raise TypeError(
                f"engine.categorize got str which isn't a glob, file, or folder {x=}"
            )
        return y
    else:
        raise TypeError("engine.categorize failure {x=}")


def from_seed(
    maybe_seed: Optional[Union[str, TreePlus]] = None,
    *,
    maybe_ignore: Optional[Tuple[str, ...]] = DEFAULT_IGNORE,
    maybe_globs: Optional[Tuple[str, ...]] = None,
    syntax_highlighting: bool = False,
    override_ignore: bool = False,
    concise: bool = False,
) -> TreePlus:
    "PUBLIC: Construct a TreePlus from maybe_seed = Union[str, TreePlus] or None"
    debug_print(
        f"from_seed {maybe_seed=} {maybe_globs=} {override_ignore=} {syntax_highlighting=}"
    )
    return from_seeds(
        maybe_seeds=None if maybe_seed is None else (maybe_seed,),
        maybe_ignore=maybe_ignore,
        maybe_globs=maybe_globs,
        syntax_highlighting=syntax_highlighting,
        override_ignore=override_ignore,
        concise=concise,
    )


def from_seeds(
    maybe_seeds: Optional[Tuple[Union[str, TreePlus], ...]] = None,
    *,
    maybe_ignore: Optional[Tuple[str, ...]] = DEFAULT_IGNORE,
    maybe_globs: Optional[Tuple[str, ...]] = None,
    syntax_highlighting: bool = False,
    override_ignore: bool = False,
    concise: bool = False,
) -> TreePlus:
    "PUBLIC: Construct a TreePlus from maybe_seeds = tuple[str] or None"
    debug_print(
        f"from_seeds {maybe_seeds=} {maybe_globs=} {syntax_highlighting=} {override_ignore=} {concise=}"
    )
    try:
        # default to current working directory
        if not maybe_seeds:
            debug_print(f"tree_plus.from_root defaulting to current working directory")
            maybe_seeds = (str(Path.cwd()),)
        if not all(isinstance(s, (str, TreePlus)) for s in maybe_seeds):
            raise TypeError(
                f"tree_plus.from_root tuple must contain str, got {maybe_seeds}"
            )
        # dedupe
        seeds = tuple(set(s for s in maybe_seeds if isinstance(s, str)))
        seeds += tuple(s for s in maybe_seeds if isinstance(s, TreePlus))
        if maybe_ignore != DEFAULT_IGNORE:
            maybe_ignore = parse_ignore(maybe_ignore, override=override_ignore)
        # mapper
        subtrees = _map_seeds(
            seeds=seeds,
            maybe_ignore=maybe_ignore,
            maybe_globs=maybe_globs,
            syntax_highlighting=syntax_highlighting,
            concise=concise,
        )
        # reducer
        n_subtrees = len(subtrees)
        if n_subtrees == 0:
            root = TreePlus(category=Category.ROOT, name="No match")
        elif n_subtrees == 1:
            root = subtrees[0]
        else:
            root = _reduce_forest(forest=subtrees)
        return root
    except Exception as e:
        debug_print(f"tree_plus.from_root Exception {e=}")
        raise e


def _reduce_forest(
    *,
    forest: Tuple[TreePlus, ...],
) -> TreePlus:
    "PRIVATE: merge a forest of trees into a single root"
    root = TreePlus(
        category=Category.ROOT,
        name="Root",
    )
    for tree in forest:
        _add_subtree(root=root, subtree=tree)
    return root


def _map_seeds(
    *,
    seeds: Optional[Tuple[Union[str, TreePlus], ...]] = None,
    maybe_ignore: Optional[Tuple[str, ...]] = DEFAULT_IGNORE,
    maybe_globs: Optional[Tuple[str, ...]] = None,
    syntax_highlighting: bool = False,
    concise: bool = False,
) -> Tuple[TreePlus, ...]:
    "PRIVATE (MAPPER): grow a forest from a tuple of seed strings"
    debug_print(f"_map_seeds {seeds=}, {maybe_ignore=}, {maybe_globs=}")
    assert seeds is not None and isinstance(
        seeds, tuple
    ), f"_map_seeds got non-tuple {seeds=}"
    folder_paths = []
    file_paths = []
    glob_paths = []
    url_paths = []
    trees_done = []
    debug_print(f"_map_seeds BEGIN CATEGORIZING SEEDS!")
    for seed in seeds:
        if isinstance(seed, TreePlus):
            trees_done.append(seed)
            continue
        if not isinstance(seed, str):
            print(f"WARNING: non-str seed, skipping {seed=}")
        try:
            # web actions sometimes independently handle trees
            category = categorize(
                seed,
                check_strs_globs=True,
                check_strs_paths=True,
                check_strs_urls=True,
                raise_if_component=True,
            )
        except Exception as e:
            print(f"_map_seeds: categorization Exception\n\t{seed=}\n\t{e=}")
            continue
        if category is Category.FILE:
            file_seed_path = Path(seed)
            file_paths.append(file_seed_path)
        elif category is Category.URL:
            url_paths.append(seed)
        elif category is Category.FOLDER:
            folder_seed_path = Path(seed)
            folder_paths.append(folder_seed_path)
        elif category is Category.GLOB:
            # unclear how this differs from glob input
            # TODO: research & decide about globs as paths instead of as filters
            debug_print(f"WARNING: _map_seeds got a GLOB {seed=}, deciding...")
            if seed.startswith("*."):
                print(f'assuming {seed=} is a normal glob because it starts with "*."')
                if maybe_globs is None:
                    maybe_globs = (seed,)
                elif isinstance(maybe_globs, (tuple, list)):  # heathens using list here
                    maybe_globs = maybe_globs + (seed,)
            else:
                glob_seed = Path(seed)
                glob_paths.append(glob_seed)
        else:
            print(f"WARNING: _map_seeds got a BAD SEED {seed=} {category=}")
            continue
    folder_paths = tuple(folder_paths)
    debug_print("_map_seeds CATEGORIZED SEEDS")
    debug_print("_map_seeds FOLDER PATHS", folder_paths)
    debug_print("_map_seeds FILE PATHS", file_paths)
    debug_print("_map_seeds GLOB PATHS", glob_paths)
    debug_print("_map_seeds URL PATHS", url_paths)
    # debug_print("_map_seeds MAYBE_GLOBS BEFORE MERGE", maybe_globs)
    # debug_print("_map_seeds GLOB_PATHS BEFORE MERGE", glob_paths)
    # globs = tuple(set(chain(maybe_globs, glob_paths)))
    # debug_print("_map_seeds GLOBS AFTER MERGE", globs)
    if maybe_globs:
        if not folder_paths:
            folder_paths = (Path.cwd(),)
        globs = amortize_globs(paths=folder_paths, globs=maybe_globs)
        if globs and file_paths:
            print(
                f"_map_seeds WARNING: {len(file_paths)} file and {len(glob_paths)} glob path(s) directly provided \nwill dodge glob filter"
            )
            print("\t\t\t(...because we assume you did this on purpose...)")
            globs = AmortizedGlobs(
                paths=globs.paths,
                globs=globs.globs,  # nice naming, bionicles
                # NOTE: here we add directly input file_paths to the amortized glob matches
                matches=globs.matches.union(frozenset(file_paths)),
            )
    else:
        globs = None
    debug_print("_map_seeds GLOBS", globs)
    # assert 0, "manually inspect tree_plus_src/engine.py _map_seeds glob amortization"
    url_paths = [(Category.URL, url_path) for url_path in url_paths]
    parsed_seeds = tuple(
        os_sorted(chain(folder_paths, file_paths, glob_paths, url_paths, trees_done))
    )
    debug_print("_map_seeds os_sorted SEEDS", seeds)
    if not parsed_seeds:
        return ()
    forest = []
    for n, parsed_seed in enumerate(parsed_seeds):
        debug_print(f"_map_seeds invoking _from_seed {n=} {parsed_seed=}")
        is_url = False
        if isinstance(parsed_seed, Tuple):
            parsed_seed = parsed_seed[1]
            is_url = True
        seed_tree_plus = _from_seed(
            seed_path=parsed_seed,
            maybe_ignore=maybe_ignore,
            maybe_globs=globs,
            syntax_highlighting=syntax_highlighting,
            concise=concise,
            is_url=is_url,
        )
        assert isinstance(seed_tree_plus, TreePlus)
        debug_print(f"_map_seeds got TreePlus from seed {n=}")
        # make sure it's a file, folder, or glob
        assert (
            seed_tree_plus.is_file()
            or seed_tree_plus.is_folder()
            or seed_tree_plus.is_glob()
            or seed_tree_plus.is_url()
        )
        forest.append(seed_tree_plus)
    debug_print(f"_map_seeds  DONE!")
    return tuple(forest)


def _from_seed(
    *,
    seed_path: Optional[Union[Path, str]] = None,
    maybe_ignore: Optional[Tuple[str, ...]] = DEFAULT_IGNORE,
    maybe_globs: Optional[AmortizedGlobs] = None,
    syntax_highlighting: bool = False,
    concise: bool = False,
    is_url: bool = False,
) -> TreePlus:
    "PRIVATE: dispatcher to either file or folder"
    if seed_path is None:
        debug_print(f"tree_plus.from_seed defaulting to current working directory")
        seed_path = Path.cwd()
    elif isinstance(seed_path, TreePlus):
        return seed_path
    else:
        if not isinstance(seed_path, (Path, str)):
            raise TypeError(
                f"tree_plus::from_path: not a pathlib.Path or str: {seed_path=}"
            )
    try:
        if isinstance(seed_path, str) and is_url:
            result = _from_url(
                url=seed_path,
                syntax_highlighting=syntax_highlighting,
                concise=concise,
            )
        elif isinstance(seed_path, Path) and not is_url:
            if seed_path.is_file():
                result = _from_file(
                    file_path=seed_path,
                    syntax_highlighting=syntax_highlighting,
                    concise=concise,
                )
            elif seed_path.is_dir():
                result = _from_folder(
                    folder_path=seed_path,
                    maybe_ignore=maybe_ignore,
                    maybe_globs=maybe_globs,
                    syntax_highlighting=syntax_highlighting,
                    concise=concise,
                )
            else:
                seed_pattern = str(seed_path)
                if not is_glob(seed_pattern):
                    raise TypeError(
                        f"tree_plus::from_path: not a file or folder: {seed_path=}"
                    )
                result = _from_glob(
                    pattern=seed_pattern,
                    maybe_ignore=maybe_ignore,
                    maybe_globs=None,  # TODO: decide if we apply glob patterns to glob paths (currently NO)
                    syntax_highlighting=syntax_highlighting,
                    concise=concise,
                )
        else:
            raise ValueError(
                f"need a Path or (str and is_url), got {seed_path} {is_url}"
            )
        return result
    except Exception as e:
        debug_print(f"tree_plus::from_seed Exception {e=}")
        raise e


def _add_subtree(
    *,
    root: TreePlus,
    subtree: TreePlus,
):
    "PRIVATE: add a subtree TreePlus to a root TreePlus"
    # debug_print(f"_add_subtree {root=} {subtree=}")
    root.subtrees.append(subtree)  # type: ignore
    if subtree.is_file():
        root.n_files += 1
    elif subtree.is_folder():
        root.n_folders += 1
        root.n_files += subtree.n_files
    if (
        root.n_tokens is not None
        and root.n_lines
        and subtree.n_tokens is not None
        and subtree.n_lines is not None
    ):
        root.n_tokens += subtree.n_tokens
        root.n_lines += subtree.n_lines


def _from_glob(
    *,
    pattern: str,
    maybe_ignore: Optional[Tuple[str, ...]] = DEFAULT_IGNORE,
    maybe_globs: Optional[AmortizedGlobs] = None,
    syntax_highlighting: bool = False,
    concise: bool = False,
) -> TreePlus:
    "PRIVATE: handle a glob seed"
    debug_print(f"engine._from_folder {pattern=} {maybe_globs=}")
    assert isinstance(pattern, str), f"_from_glob got non-str {pattern=}"
    # TODO: decide between glob and rglob in _from_glob
    # glob_paths = os_sorted(filter(lambda x: x != ".", Path().rglob(pattern)))
    glob_paths = os_sorted(filter(lambda x: x != ".", Path().glob(pattern)))
    glob_tree_plus = TreePlus(
        Category.GLOB,
        name=pattern,
    )
    # TODO: decide if we need to re-amortize the globs in the glob seed
    for glob_n, glob_path in enumerate(glob_paths):
        # TODO: clarify ignore in glob seed context, skipping for now
        # if should_ignore(glob_path, ignore=maybe_ignore, globs=maybe_globs):
        #     continue
        if glob_path.is_dir():
            subtree_plus = _from_folder(
                folder_path=glob_path,
                maybe_ignore=maybe_ignore,
                maybe_globs=maybe_globs,
                syntax_highlighting=syntax_highlighting,
                concise=concise,
            )
        elif glob_path.is_file():
            subtree_plus = _from_file(
                file_path=glob_path,
                syntax_highlighting=syntax_highlighting,
                concise=concise,
            )
        else:
            debug_print(
                f"engine._from_glob skip non(folder|file) {glob_n=} {glob_path=}"
            )
            continue
        _add_subtree(root=glob_tree_plus, subtree=subtree_plus)
    return glob_tree_plus


def _from_folder(
    *,
    folder_path: Path,
    maybe_ignore: Optional[Tuple[str, ...]] = DEFAULT_IGNORE,
    maybe_globs: Optional[AmortizedGlobs] = None,
    syntax_highlighting: bool = False,
    concise: bool = False,
) -> TreePlus:
    "PRIVATE: walk a folder and construct a tree"
    debug_print(f"engine._from_folder {folder_path=} {maybe_globs=}")
    assert folder_path.is_dir(), f"tree_plus._from_folder got a non-dir {folder_path=}"
    assert is_parsed_ignore(
        maybe_ignore
    ), f"tree_plus._from_folder got a non-parsed {maybe_ignore=}"
    assert maybe_globs is None or isinstance(
        maybe_globs, AmortizedGlobs
    ), f"engine._from_folder needs None or AmortizedGlobs, got {maybe_globs=}"
    subtree_paths = os_sorted(filter(lambda x: x != ".", folder_path.iterdir()))
    folder_tree_plus = TreePlus(
        category=Category.FOLDER,
        name=folder_path.resolve().name,  # FIXES THE Path(".").name == "" bug
        n_folders=1,
    )
    for subtree_n, subtree_path in enumerate(subtree_paths):
        # debug_print(f"engine._from_folder {subtree_n=} {subtree_path=}")
        if should_ignore(subtree_path, ignore=maybe_ignore, globs=maybe_globs):
            continue
        if subtree_path.is_dir():
            subtree_plus = _from_folder(
                folder_path=subtree_path,
                maybe_ignore=maybe_ignore,
                maybe_globs=maybe_globs,
                syntax_highlighting=syntax_highlighting,
                concise=concise,
            )
        elif subtree_path.is_file():
            subtree_plus = _from_file(
                file_path=subtree_path,
                syntax_highlighting=syntax_highlighting,
                concise=concise,
            )
        else:
            debug_print(f"engine._from_folder skip non(folder|file) {subtree_n=}")
            continue
        _add_subtree(root=folder_tree_plus, subtree=subtree_plus)
    return folder_tree_plus


# TODO: re-enable func_timeout for parsing
# def _parse(file_path: str, timeout=TIMEOUT_SECONDS) -> List[str]:
#     try:
#         components = func_timeout(timeout, parse_file, args=(file_path,))
#         return components
#     except Exception as e:
#         print(f"WARNING: parse_file Exception\n{e}")
#         return []


def _from_file(
    *,
    file_path: Path,
    syntax_highlighting: bool = False,
    concise: bool = False,
    max_tokens: int = 100_000,
) -> TreePlus:
    "PRIVATE: parse a file_path into a TreePlus"
    debug_print(f"engine._from_file {file_path=}")
    assert file_path.is_file(), f"tree_plus._from_file got a non-file {file_path=}"
    counts = TokenLineCount(0, 0)
    try:
        new_counts = count_tokens_lines(file_path)
        if new_counts:
            counts = new_counts
    except Exception as e:
        debug_print(f"engine._from_file counts Exception {e=}")
    try:
        components = []
        if not concise and (counts is not None and counts.n_tokens <= max_tokens):
            components = parse_file(file_path)
    except Exception as e:
        debug_print(f"engine._from_file components Exception {e=}")
        components = []
    if syntax_highlighting:
        debug_print(f"_from_file SYNTAX HIGHLIGHTING {file_path=}")
        try:
            components = _syntax_highlight(
                file_path=file_path,
                components=components,
            )
        except Exception as e:
            debug_print(f"engine._from_file syntax highlighting exception {e=}")
    else:
        debug_print(f"_from_file NOT SYNTAX HIGHLIGHTING {file_path=}")
    debug_print(f"engine._from_file got counts:", counts)
    # debug_print(f"engine._from_file got components:", components)
    file_tree_plus = TreePlus(
        subtrees=components,  # type: ignore
        category=Category.FILE,
        name=file_path.name,
        n_folders=0,
        n_files=1,
        n_tokens=None if counts is None else counts.n_tokens,
        n_lines=None if counts is None else counts.n_lines,
    )
    return file_tree_plus


ua = UserAgent(browsers=["chrome"])


def _from_url(
    *,
    url: str,
    syntax_highlighting: bool = False,
    concise: bool = False,
) -> TreePlus:
    "PRIVATE: build TreePlus from a URL (not recursive for now)"
    debug_print(f"engine._from_url {url=}")
    try:
        if not (url.startswith("http://") or url.startswith("https://")):
            url = f"https://{url}"
        html_response = requests.get(url, headers={"User-Agent": ua.random})
        html_text = html_response.text
        count = count_tokens_lines_from_contents(html_text)
        hrefs = None
        if concise:
            components = []
        elif url.endswith(".md"):
            components = parse_file(url, contents=html_text)
        else:
            components = [_from_html_text(html_text, maybe_url_base=url)]
            hrefs = components[0].hrefs

        url_tree_plus = TreePlus(
            subtrees=components,
            # type: ignore
            category=Category.URL,
            name=url,
            n_folders=0,
            n_files=1,
            n_tokens=count.n_tokens,
            n_lines=count.n_lines,
            hrefs=hrefs,
        )
        return url_tree_plus
    except Exception as e:
        raise e


default_keepers = frozenset({"title", "h1", "h2", "h3", "table", "tr", "ol", "li"})


def base_url(url: str, with_path: bool = False) -> str:
    import urllib.parse

    parsed = urllib.parse.urlparse(url)
    path = "/".join(parsed.path.split("/")[:-1]) if with_path else ""
    parsed = parsed._replace(path=path)
    parsed = parsed._replace(params="")
    parsed = parsed._replace(query="")
    parsed = parsed._replace(fragment="")
    parsed_url = parsed.geturl()
    parsed_url = str(parsed_url)
    return parsed_url


def _from_html_text(contents: str, maybe_url_base: Optional[str] = None) -> TreePlus:
    soup = BeautifulSoup(contents, "html.parser")
    if maybe_url_base:
        maybe_url_base = base_url(maybe_url_base)
    soup_tree = _from_soup(soup, maybe_url_base=maybe_url_base)
    assert soup_tree is not None
    return soup_tree


def empty_tag_tree(n: str = "?"):
    return TreePlus(Category.TAG, n, subtrees=[])


def union_from_element(elem: PageElement) -> Union[Tag, NavigableString]:
    "to satisfy the type checker about the elem"
    if isinstance(elem, Tag):
        return elem
    elif isinstance(elem, NavigableString):
        return elem
    else:
        raise ValueError(f"NOT IN UNION: {elem}")


def node_index_str_from_tuple(
    node_index: Tuple[int, ...],
    prefix: str = "(",
    suffix: str = ")",
    number_color: str = CYBERCYAN,
    dot_color: str = GOLD,
) -> str:
    node_index_str = ""
    for node_index_i in node_index:
        node_index_str += f"[{number_color}]{node_index_i}[/{number_color}][{dot_color}].[/{dot_color}]"
    node_index_str = f"{prefix}{node_index_str}{suffix}"
    return node_index_str


def _from_soup(
    tag: Union[Tag, NavigableString],
    tree: Optional[TreePlus] = None,
    node_index: Tuple[int, ...] = (),
    maybe_url_base: Optional[str] = None,
    hrefs: Optional[Dict[str, list]] = None,
) -> Optional[TreePlus]:
    if not tree:
        tree = empty_tag_tree()

    if node_index == () and not hrefs:
        hrefs = {
            "node_index_str": [],
            "href": [],
        }
    assert hrefs is not None
    subtrees = []
    if isinstance(tag, NavigableString):
        tag_string = "\n".join(tag.stripped_strings)
        tree.name = tag_string  # type: ignore
        if not tag_string:
            return None
    elif isinstance(tag, Tag):
        tag_attrs = tag.attrs

        node_index_str = node_index_str_from_tuple(node_index)
        # prepend the base url if it's available and the href points to a slash route
        if tag.name == "a":
            debug_print("a")
            if "href" in tag_attrs:
                href = tag_attrs["href"]
                if maybe_url_base is not None:
                    if href.startswith("./"):
                        href = href.lstrip(".")
                    if href.startswith("/"):
                        href_with_base = f"{maybe_url_base}{href}"
                        tag_attrs["href"] = href_with_base
                        href = href_with_base
                    hrefs["node_index_str"].append(node_index_str)
                hrefs["href"].append(tag_attrs["href"])

        maybe_newline = "\n" if len(tag_attrs) > 1 else ""
        tag_attrs_strings = [f"{maybe_newline}{k}={v}" for k, v in tag_attrs.items()]
        tag_attrs_string = " ".join(tag_attrs_strings)
        if tag_attrs_string:
            tag_attrs_string = " " + tag_attrs_string
        tree.name = f"<{tag.name}{tag_attrs_string}> at {node_index_str}"

        child_n = 1
        for child_tag in tag.children:
            # NOTE: this is only to satisfy the type checker
            child_tag_union = union_from_element(child_tag)
            subtree = _from_soup(
                child_tag_union,
                tree=empty_tag_tree(),
                node_index=node_index + (child_n,),
                maybe_url_base=maybe_url_base,
                hrefs=hrefs,
            )
            if subtree is None:
                continue
            subtrees.append(subtree)
            child_n += 1

    tree.subtrees = subtrees
    tree.hrefs = hrefs

    return tree


Articles = Tuple[Tuple[dict, Tuple[dict, ...]], ...]


A_TAG_PATTERN = r'<a href="(?P<href>.*?)">(?P<text>.*?)</a>'


def from_hacker_news_articles(
    articles: Articles,
    depth: int = 0,
    max_depth: int = -1,
    title: Union[str, Panel, Text, Table, Markdown, Pretty] = "Hacker News Front Page",
    parent_num: Tuple[int, ...] = (),
) -> TreePlus:
    "construct a TreePlus given articles hlist from web.articles_from_hacker_news"
    article_forest = []
    total_files = 0

    for i, (article, kids) in enumerate(articles, start=1):
        article_tree = process_hacker_news_item(
            article,
            kids,  # type: ignore
            depth,
            max_depth,
            parent_num + (i,),
        )
        article_forest.append(article_tree)
        total_files += article_tree.n_files

    article_root = TreePlus(
        category=Category.URL,
        name=title,
        subtrees=article_forest,
        n_files=total_files,
    )
    return article_root


def format_link(
    url: str,
    text: str,
    link_color: str = LINK_COLOR,
) -> str:
    return f"[{link_color}][link={url}]{text}[/link][/{link_color}]"


def process_hacker_news_item(
    item: dict,
    kids: Articles,
    depth: int,
    max_depth: int,
    parent_num: Tuple[int, ...],
    parser: Union[Literal["lxml"], Literal["html.parser"]] = "html.parser",
    link_color: str = LINK_COLOR,
) -> TreePlus:
    item_number = f"{'.'.join(str(n) for n in parent_num)}. "
    item_name = ""
    if "title" in item:
        title_text = item["title"]
        if "url" in item:
            url = item["url"]
            title_text = format_link(url, title_text)
        item_name = f"{title_text}"
    item_text = ""
    if "text" in item:
        item_html = item["text"]
        item_soup = BeautifulSoup(item_html, features=parser)
        item_links = rich_links_from_soup(item_soup)
        item_links = "\n".join(item_links)
        item_text = item_soup.get_text()
        item_text += item_links
        if item_text:
            item_text = " " + item_text
        # item_with_links = re.sub(A_TAG_PATTERN, replace_link, item_text)
        # item_markdown = markdownify.markdownify(item_with_links, autolinks=False)

    if not item_name and not item_text:
        raise ValueError("neither name nor value")
    elif item_name and not item_text:
        pass
    elif not item_name and item_text:
        item_name = Markdown(item_text)
    elif item_name and item_text:
        item_name = Panel(item_name, title=item_number)
        item_name = _make_rich_tree(item_name)
        item_name.add(Markdown(item_text))

    # item_counts = count_tokens_lines_from_contents(item_name)
    kid_trees = []

    if max_depth < 0 or (max_depth > 0 and depth < max_depth):
        for j, (kid, kid_comments) in enumerate(kids, start=1):
            kid_subtree = process_hacker_news_item(
                kid,
                kid_comments,  # type: ignore
                depth + 1,
                max_depth,
                parent_num + (j,),
            )
            kid_trees.append(kid_subtree)

    item_tree = TreePlus(
        category=Category.URL,
        name=Panel(
            item_name,
            title=f"{item_number}[{link_color}][link=https://news.ycombinator.com/item?id={item['id']}]{item['type'].title()} {item['id']:,}[/link][/{link_color}]",
            title_align="left",
        ),
        subtrees=kid_trees,
        n_files=1 + len(kids),
        # n_lines=item_counts.n_lines,
        # n_tokens=item_counts.n_tokens,
    )
    return item_tree


def rich_links_from_soup(
    item_soup: BeautifulSoup,
    recursive: bool = True,
) -> List[str]:
    item_links = item_soup.find_all("a", recursive=recursive)
    links = []
    for i, link in enumerate(item_links, start=1):
        url = link["href"]
        # text = link.text
        # color = colors["colorblind_blue"]
        rich_link = f"\n- {i}. {url}"
        links.append(rich_link)
    return links


def ordered_list_from(l: Iterable[str]) -> List[str]:
    return [f" - {i}. {x}" for i, x in enumerate(l, start=1)]


BACKUP_LEXERS = {
    "kt": "kotlin",
    "cbl": "cobol",
    "rst": "markdown",
    "cc": "cpp",
    "h": "c",
    "md": "markdown",
    "html": "markdown",
}

DENY_SUFFIXES = {"json"}


def _get_lexer(file_path: Path) -> str:
    extension_no_period = file_path.suffix.lstrip(".")
    debug_print(f"_get_lexer for extension_no_period={extension_no_period}")
    if "makefile" in file_path.name:
        lexer = "make"
    elif extension_no_period in DENY_SUFFIXES:
        lexer = ""
    elif extension_no_period in BACKUP_LEXERS:
        lexer = BACKUP_LEXERS[extension_no_period]
    else:
        lexer = extension_no_period
    debug_print(f"_get_lexer chose {lexer=} for {file_path=}")
    return lexer


def _syntax_highlight(
    *,
    file_path: Path,
    components: List[str],
) -> Union[List[Syntax], List[str]]:
    "PRIVATE: either Syntax highlighting, or fallback to str"
    debug_print(
        f"_syntax_highlight {len(components)} component(s) from file_path={file_path}"
    )
    lexer = _get_lexer(file_path)
    if not lexer:
        return components
    highlighted = []
    for component in components:
        try:
            syntax = Syntax(component, lexer)
            if syntax.lexer is None:
                debug_print(f"_syntax_highlight failed with {lexer=}")
                return components
            highlighted.append(syntax)
        except Exception as e:
            debug_print(f"_syntax_highlight failed with {lexer=}\n{e=}")
            return components
    debug_print(f"_syntax_highlight succeeded with {lexer=}")
    return highlighted
