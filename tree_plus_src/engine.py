from typing import (
    List,
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
from rich.syntax import Syntax
from rich.theme import Theme
from rich.tree import Tree
from natsort import os_sorted

from tree_plus_src import (
    debug_print,
    parse_file,
    count_tokens_lines,
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


class Category(Enum):
    "PUBLIC: TreePlus"
    ROOT = 1
    GLOB = 2
    FOLDER = 3
    FILE = 4
    COMPONENT = 5


@dataclass
class TreePlus:
    "PUBLIC: data structure"
    category: Category = Category.COMPONENT
    name: str = ""
    n_folders: int = 0
    n_files: int = 0
    n_lines: Optional[int] = 0
    n_tokens: Optional[int] = 0
    subtrees: List[Union["TreePlus", str]] = field(default_factory=list)

    def is_root(self) -> bool:
        return self.category is Category.ROOT

    def is_folder(self) -> bool:
        return self.category is Category.FOLDER

    def is_file(self) -> bool:
        return self.category is Category.FILE

    def is_glob(self) -> bool:
        return self.category is Category.GLOB

    def is_component(self) -> bool:
        return self.category is Category.COMPONENT

    def into_rich_tree(self) -> Tree:
        "PUBLIC: Convert a TreePlus into a rich.tree.Tree to render"
        return into_rich_tree(root=self)

    def into_str(self) -> str:
        "PUBLIC: Convert a TreePlus into a rich.tree.Tree to render"
        return tree_to_string(into_rich_tree(root=self))

    def render(self):
        "PUBLIC: Safely print a TreePlus"
        safe_print(into_rich_tree(root=self))

    def stats(self) -> str:
        "PUBLIC: statistics"
        # grammar-focused:
        # stats = ""
        # stats += f"{self.n_folders} folder{'' if self.n_folders == 1 else 's'}"
        # stats += f", {self.n_files} file{'' if self.n_files == 1 else 's'}"
        # stats += f", {self.n_lines} line{'' if self.n_lines == 1 else 's'}"
        # stats += f", {self.n_tokens} token{'' if self.n_tokens == 1 else 's'}"
        # faster:
        stats = f"{self.n_folders:,} folder(s), {self.n_files:,} file(s), {self.n_lines:,} line(s), {self.n_tokens:,} token(s)"
        return stats


@lru_cache
def remove_trailing_space(x: str) -> str:
    # debug_print("remove_trailing_space")
    # debug_print(f"'{x=}'")
    y = re.sub(r" +\n", "\n", x)
    # debug_print(f"'{y=}'")
    return y


def tree_to_string(tree: Tree) -> str:
    console = Console(
        # force_terminal=True,
        no_color=True,
        # soft_wrap=True,
        # markup=False,
        # highlight=False,
        theme=Theme({"repr.ipv6": "default"}),
    )
    with console.capture() as capture:
        console.print(tree, markup=False)
    captured_str = capture.get()
    # captured_str = ansi_escape.sub("", captured_str)
    captured_str = remove_trailing_space(captured_str)
    return captured_str


def clean_string(input_str: str) -> str:
    return input_str.encode("unicode-escape").decode("ascii")


def safe_print(
    tree: Tree,
    style: Optional[str] = None,
    highlight: bool = True,
    markup: bool = False,
):
    try:
        # Attempt to print the tree normally
        console = Console(
            # reduce the tab size to fit content
            tab_size=2,
            width=128 if os.environ.get("TREE_PLUS_UPDATE_README") == "YES" else None,
            markup=markup,
            highlight=highlight,
            style=style,
            theme=Theme({"repr.ipv6": "default"}),
        )
        console.print(tree)
    except UnicodeEncodeError as e:
        debug_print(f"UnicodeEncodeError printing tree normally: ", e)
        try:
            debug_print(f"Attempt to print a cleaned version of the tree:")
            tree_string = tree_to_string(tree)
            # debug_print(f"{tree_string=}")
            clean_tree_string = clean_string(tree_string)
            # debug_print(f"{clean_tree_string=}")
            print(clean_tree_string)
        except Exception as e:
            print("An error occurred when attempting to print the tree.")
            print(e)


STYLE = "bold"


def _make_rich_tree(label: str) -> Tree:
    return Tree(label, guide_style=STYLE, highlight=True)


def into_rich_tree(*, root: TreePlus = None) -> Tree:
    "PUBLIC: Convert a TreePlus into a rich.tree.Tree to render"
    if not isinstance(root, TreePlus):
        raise TypeError(f"tree_plus.into_rich_tree got non-TreePlus {root=}")
    rich_tree = None
    if root.category is Category.FILE:
        label = f"{FILE_CHAR} {root.name}"
        if root.n_tokens is not None and root.n_lines is not None:
            label += f" ({root.n_tokens} token{'' if root.n_tokens == 1 else 's'}, {root.n_lines} line{'' if root.n_lines == 1 else 's'})"
        rich_tree = _make_rich_tree(label)
        for subtree in root.subtrees:
            rich_tree.add(subtree)
    elif root.category is Category.FOLDER:
        label = f"{FOLDER_CHAR} {root.name} ({root.n_folders} folder{'' if root.n_folders == 1 else 's'}, {root.n_files} file{'' if root.n_files == 1 else 's'})"
        rich_tree = _make_rich_tree(label)
        for subtree in root.subtrees:
            # RECURSION HERE
            rich_subtree = into_rich_tree(root=subtree)
            rich_tree.add(rich_subtree)
        counts = f" "
        rich_tree.label += counts
    elif root.category is Category.ROOT:
        label = f"{ROOT_CHAR} {root.name} ({root.n_folders} folder{'' if root.n_folders == 1 else 's'}, {root.n_files} file{'' if root.n_files == 1 else 's'})"
        rich_tree = _make_rich_tree(label)
        for subtree in root.subtrees:
            rich_subtree = into_rich_tree(root=subtree)
            rich_tree.add(rich_subtree)
    elif root.category is Category.GLOB:
        n_matches = len(root.subtrees)
        label = f"{GLOB_CHAR} {root.name} ({n_matches} match{'' if n_matches == 1 else 'es'})"
        rich_tree = _make_rich_tree(label)
        for subtree in root.subtrees:
            rich_subtree = into_rich_tree(root=subtree)
            rich_tree.add(rich_subtree)
    if not rich_tree:
        raise TypeError(f"engine.into_rich_tree: unsupported {root=}")
    return rich_tree


@lru_cache
def categorize(
    x: Union[Path, Tuple[str], str],
    check_strs_globs: bool = True,
    check_strs_paths: bool = True,
    raise_if_component: bool = True,
) -> Category:
    "assign x to a category enum DEFAULTS TO CHECK GLOBS AND PATHS"
    y = None
    # DECISION BEGINS
    if isinstance(x, str):  # STR first, happy path
        # could deactivate checking for performance
        y = Category.COMPONENT
        if check_strs_globs and is_glob(x):
            y = Category.GLOB
        if y is not Category.GLOB and check_strs_paths:
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
    maybe_seed_str=None,
    *,
    maybe_ignore: Optional[Tuple[str]] = DEFAULT_IGNORE,
    maybe_globs: Optional[Tuple[str]] = None,
    syntax_highlighting: bool = False,
    override_ignore: bool = False,
    concise: bool = False,
) -> TreePlus:
    "PUBLIC: Construct a TreePlus from maybe_seed_strs = tuple[str] or None"
    debug_print(
        f"from_seed {maybe_seed_str=} {maybe_globs=} {override_ignore=} {syntax_highlighting=}"
    )
    return from_seeds(
        maybe_seed_strs=(maybe_seed_str,),
        maybe_ignore=maybe_ignore,
        maybe_globs=maybe_globs,
        syntax_highlighting=syntax_highlighting,
        override_ignore=override_ignore,
        concise=concise,
    )


def from_seeds(
    maybe_seed_strs: Optional[Tuple[str]] = None,
    *,
    maybe_ignore: Optional[Tuple[str]] = DEFAULT_IGNORE,
    maybe_globs: Optional[Tuple[str]] = None,
    syntax_highlighting: bool = False,
    override_ignore: bool = False,
    concise: bool = False,
) -> TreePlus:
    "PUBLIC: Construct a TreePlus from maybe_seed_strs = tuple[str] or None"
    debug_print(
        f"from_seeds {maybe_seed_strs=} {maybe_globs=} {syntax_highlighting=} {override_ignore=} {concise=}"
    )
    try:
        # default to current working directory
        if not maybe_seed_strs:
            debug_print(f"tree_plus.from_root defaulting to current working directory")
            maybe_seed_strs = (str(Path.cwd()),)
        if not all(isinstance(s, str) for s in maybe_seed_strs):
            raise TypeError(
                f"tree_plus.from_root tuple must contain str, got {maybe_seed_strs}"
            )
        # dedupe
        seeds = tuple(set(maybe_seed_strs))
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


def _reduce_forest(*, forest: Tuple[TreePlus] = None) -> TreePlus:
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
    seeds: Tuple[str] = None,
    maybe_ignore: Optional[Tuple[str]] = DEFAULT_IGNORE,
    maybe_globs: Optional[Tuple[str]] = None,
    syntax_highlighting: bool = False,
    concise: bool = False,
) -> Tuple[TreePlus]:
    "PRIVATE (MAPPER): grow a forest from a tuple of seed strings"
    debug_print(f"_map_seeds {seeds=}, {maybe_ignore=}, {maybe_globs=}")
    assert seeds is not None and isinstance(
        seeds, tuple
    ), f"_map_seeds got non-tuple {seeds=}"
    folder_paths = []
    file_paths = []
    glob_paths = []
    debug_print(f"_map_seeds BEGIN CATEGORIZING SEEDS!")
    for seed in seeds:
        if not isinstance(seed, str):
            print(f"WARNING: non-str seed, skipping {seed=}")
        try:
            category = categorize(
                seed,
                check_strs_globs=True,
                check_strs_paths=True,
                raise_if_component=True,
            )
        except Exception as e:
            print(f"_map_seeds: categorization Exception\n\t{seed=}\n\t{e=}")
            continue
        if category is Category.FILE:
            file_seed_path = Path(seed)
            file_paths.append(file_seed_path)
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
    debug_print("_map_seeds CATEGORIZED SEEDS")
    debug_print("_map_seeds FOLDER PATHS", folder_paths)
    debug_print("_map_seeds FILE PATHS", file_paths)
    debug_print("_map_seeds GLOB PATHS", glob_paths)
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
    parsed_seeds = tuple(os_sorted(chain(folder_paths, file_paths, glob_paths)))
    debug_print("_map_seeds os_sorted SEEDS", seeds)
    if not parsed_seeds:
        return []
    forest = []
    for n, parsed_seed in enumerate(parsed_seeds):
        debug_print(f"_map_seeds invoking _from_seed {n=} {parsed_seed=}")
        seed_tree_plus = _from_seed(
            seed_path=parsed_seed,
            maybe_ignore=maybe_ignore,
            maybe_globs=globs,
            syntax_highlighting=syntax_highlighting,
            concise=concise,
        )
        assert isinstance(seed_tree_plus, TreePlus)
        debug_print(f"_map_seeds got TreePlus from seed {n=}")
        # make sure it's a file, folder, or glob
        assert (
            seed_tree_plus.is_file()
            or seed_tree_plus.is_folder()
            or seed_tree_plus.is_glob()
        )
        forest.append(seed_tree_plus)
    debug_print(f"_map_seeds  DONE!")
    return forest


def _from_seed(
    *,
    seed_path: Optional[Path] = None,
    maybe_ignore: Optional[Tuple[str]] = DEFAULT_IGNORE,
    maybe_globs: Optional[AmortizedGlobs] = None,
    syntax_highlighting: bool = False,
    concise: bool = False,
) -> TreePlus:
    "PRIVATE: dispatcher to either file or folder"
    if seed_path is None:
        debug_print(f"tree_plus.from_seed defaulting to current working directory")
        seed_path = Path.cwd()
    else:
        if not isinstance(seed_path, Path):
            raise TypeError(f"tree_plus::from_path: not a pathlib.Path: {seed_path=}")
    try:
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
        return result
    except Exception as e:
        debug_print(f"tree_plus::from_seed Exception {e=}")
        raise e


def _add_subtree(
    *,
    root: TreePlus = None,
    subtree: TreePlus = None,
):
    "PRIVATE: add a subtree TreePlus to a root TreePlus"
    # debug_print(f"_add_subtree {root=} {subtree=}")
    root.subtrees.append(subtree)
    if subtree.is_file():
        root.n_files += 1
    elif subtree.is_folder():
        root.n_folders += 1
        root.n_files += subtree.n_files
    if subtree.n_tokens is not None and subtree.n_lines is not None:
        root.n_tokens += subtree.n_tokens
        root.n_lines += subtree.n_lines


def _from_glob(
    *,
    pattern: str,
    maybe_ignore: Optional[Tuple[str]] = DEFAULT_IGNORE,
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
    maybe_ignore: Optional[Tuple[str]] = DEFAULT_IGNORE,
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
) -> TreePlus:
    "PRIVATE: parse a file_path into a TreePlus"
    debug_print(f"engine._from_file {file_path=}")
    assert file_path.is_file(), f"tree_plus._from_file got a non-file {file_path=}"
    try:
        counts = count_tokens_lines(file_path)
    except Exception as e:
        debug_print(f"engine._from_file counts Exception {e=}")
        counts = TokenLineCount(0, 0)
    try:
        components = parse_file(file_path) if not concise else []
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
        subtrees=components,
        category=Category.FILE,
        name=file_path.name,
        n_folders=0,
        n_files=1,
        n_tokens=None if counts is None else counts.n_tokens,
        n_lines=None if counts is None else counts.n_lines,
    )
    return file_tree_plus


BACKUP_LEXERS = {
    "kt": "kotlin",
    "cbl": "cobol",
    "rst": "markdown",
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
    file_path: Path = None,
    components: List[str] = None,
) -> Union[Syntax, str]:
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
