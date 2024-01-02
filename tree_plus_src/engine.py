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

from natsort import os_sorted
from rich.console import Console
from rich.syntax import Syntax
from rich.tree import Tree

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
)

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
    n_lines: int = 0
    n_tokens: int = 0
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
        self_as_rich_tree = into_rich_tree(root=self)
        return self_as_rich_tree

    def into_str(self) -> str:
        "PUBLIC: Convert a TreePlus into a rich.tree.Tree to render"
        self_as_rich_tree = into_rich_tree(root=self)
        self_as_str = tree_to_string(self_as_rich_tree)
        return self_as_str

    def render(self):
        "PUBLIC: Safely print a TreePlus"
        safe_print(self.into_rich_tree())


console = Console()


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
    )
    with console.capture() as capture:
        console.print(tree, markup=False)
    captured_str = capture.get()
    # captured_str = ansi_escape.sub("", captured_str)
    captured_str = remove_trailing_space(captured_str)
    return captured_str


def clean_string(input_str: str) -> str:
    return input_str.encode("unicode-escape").decode("ascii")


def safe_print(tree: Tree):
    try:
        # Attempt to print the tree normally
        console = Console(
            # reduce the tab size to fit content
            tab_size=2,
            width=128 if os.environ.get("TREE_PLUS_UPDATE_README") == "YES" else None,
            markup=False,
            highlight=True,
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


def into_rich_tree(*, root: TreePlus = None) -> Tree:
    "PUBLIC: Convert a TreePlus into a rich.tree.Tree to render"
    if not isinstance(root, TreePlus):
        raise TypeError(f"tree_plus.into_rich_tree got non-TreePlus {root=}")
    rich_tree = None
    if root.category is Category.FILE:
        label = f"{FILE_CHAR} {root.name}"
        rich_tree = Tree(label)
        for subtree in root.subtrees:
            rich_tree.add(subtree)
        counts = f" ({root.n_tokens} token{'' if root.n_tokens == 1 else 's'}, {root.n_lines} line{'' if root.n_lines == 1 else 's'})"
        rich_tree.label += counts
    elif root.category is Category.FOLDER:
        label = f"{FOLDER_CHAR} {root.name}"
        rich_tree = Tree(label)
        for subtree in root.subtrees:
            # RECURSION HERE
            rich_subtree = into_rich_tree(root=subtree)
            rich_tree.add(rich_subtree)
        counts = f" ({root.n_folders} folder{'' if root.n_folders == 1 else 's'}, {root.n_files} file{'' if root.n_files == 1 else 's'})"
        rich_tree.label += counts
    elif root.category is Category.ROOT:
        label = f"{ROOT_CHAR} {root.name}"
        rich_tree = Tree(label)
        for subtree in root.subtrees:
            rich_subtree = into_rich_tree(root=subtree)
            rich_tree.add(rich_subtree)
        counts = f" ({root.n_folders} folder{'' if root.n_folders == 1 else 's'}, {root.n_files} file{'' if root.n_files == 1 else 's'})"
        rich_tree.label += counts
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
            y = Category.FOLDER
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
    maybe_ignore: Optional[Tuple[str]] = None,
    maybe_globs: Optional[Tuple[str]] = None,
    syntax_highlighting: bool = True,
) -> TreePlus:
    "PUBLIC: Construct a TreePlus from maybe_seed_strs = tuple[str] or None"
    return from_seeds(
        maybe_seed_strs=(maybe_seed_str,),
        maybe_ignore=maybe_ignore,
        maybe_globs=maybe_globs,
        syntax_highlighting=syntax_highlighting,
    )


def from_seeds(
    maybe_seed_strs: Optional[Tuple[str]] = None,
    *,
    maybe_ignore: Optional[Tuple[str]] = None,
    maybe_globs: Optional[Tuple[str]] = None,
    syntax_highlighting: bool = True,
) -> TreePlus:
    "PUBLIC: Construct a TreePlus from maybe_seed_strs = tuple[str] or None"
    try:
        # default to current working directory
        if maybe_seed_strs is None:
            debug_print(f"tree_plus.from_root defaulting to current working directory")
            maybe_seed_strs = (".",)
        if not all(isinstance(s, str) for s in maybe_seed_strs):
            raise TypeError(
                f"tree_plus.from_root tuple must contain str, got {maybe_seed_strs}"
            )
        seeds = maybe_seed_strs
        # mapper
        subtrees = _map_seeds(
            seeds=seeds,
            maybe_ignore=maybe_ignore,
            maybe_globs=maybe_globs,
            syntax_highlighting=syntax_highlighting,
        )
        # reducer
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
    syntax_highlighting: bool = True,
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
            print("WARNING: non-str seed, skipping")
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
            glob_seed = seed
            print(f"WARNING: _map_seeds got a GLOB {seed=}, moving into maybe_globs...")
            glob_paths.append(glob_seed)
        else:
            print(f"WARNING: _map_seeds got a BAD SEED {seed=} {category=}")
            continue
    debug_print("_map_seeds CATEGORIZED SEEDS")
    debug_print("_map_seeds FOLDER PATHS", folder_paths)
    debug_print("_map_seeds FILE PATHS", file_paths)
    if maybe_globs is None:
        maybe_globs = []
    debug_print("_map_seeds MAYBE_GLOBS BEFORE MERGE", maybe_globs)
    debug_print("_map_seeds GLOB_PATHS BEFORE MERGE", glob_paths)
    globs = tuple(set(chain(maybe_globs, glob_paths)))
    debug_print("_map_seeds GLOBS AFTER MERGE", globs)
    if globs:
        globs = amortize_globs(paths=folder_paths, globs=globs)
        if globs and file_paths:
            print(
                f"_map_seeds WARNING: {len(file_paths)} file path(s) directly provided will dodge glob filter"
            )
            print("\t\t\t(...because we assume you did this on purpose...)")
            globs = AmortizedGlobs(
                paths=globs.paths,
                globs=globs.globs,  # nice naming, bionicles
                matches=globs.matches.union(frozenset(file_paths)),
            )
    else:
        globs = None
    debug_print("_map_seeds GLOBS", globs)
    # assert 0, "manually inspect tree_plus_src/engine.py _map_seeds glob amortization"
    parsed_seeds = tuple(os_sorted(chain(folder_paths, file_paths)))
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
        )
        debug_print(f"_map_seeds TreePlus from seed {n=}", seed_tree_plus)
        assert isinstance(seed_tree_plus, TreePlus)
        assert seed_tree_plus.is_file() or seed_tree_plus.is_folder()
        forest.append(seed_tree_plus)
    debug_print(f"_map_seeds  DONE!")
    return forest


def _from_seed(
    *,
    seed_path: Optional[Path] = None,
    maybe_ignore: Optional[Tuple[str]] = DEFAULT_IGNORE,
    maybe_globs: Optional[AmortizedGlobs] = None,
    syntax_highlighting: bool = True,
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
            )
        elif seed_path.is_dir():
            result = _from_folder(
                folder_path=seed_path,
                maybe_ignore=maybe_ignore,
                maybe_globs=maybe_globs,
                syntax_highlighting=syntax_highlighting,
            )
        else:
            raise TypeError(f"tree_plus::from_path: not a file or folder: {seed_path=}")
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
    debug_print(f"_add_subtree {root=} {subtree=}")
    root.subtrees.append(subtree)
    if subtree.is_file():
        root.n_files += 1
    elif subtree.is_folder():
        root.n_folders += 1
        root.n_files += subtree.n_files
    root.n_tokens += subtree.n_tokens
    root.n_lines += subtree.n_lines


def _from_folder(
    *,
    folder_path: Path,
    maybe_ignore: Optional[Tuple[str]] = tuple(DEFAULT_IGNORE),
    maybe_globs: Optional[AmortizedGlobs] = None,
    syntax_highlighting: bool = True,
) -> TreePlus:
    "PRIVATE: walk a folder and construct a tree"
    debug_print(f"engine._from_folder {folder_path=} {maybe_ignore=} {maybe_globs=}")
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
        name=folder_path.name,
    )
    for subtree_n, subtree_path in enumerate(subtree_paths):
        debug_print(f"engine._from_folder {subtree_n=} {subtree_path=}")
        if should_ignore(subtree_path, ignore=maybe_ignore, globs=maybe_globs):
            continue
        if subtree_path.is_dir():
            subtree_plus = _from_folder(
                folder_path=subtree_path,
                maybe_ignore=maybe_ignore,
                maybe_globs=maybe_globs,
            )
        elif subtree_path.is_file():
            subtree_plus = _from_file(
                file_path=subtree_path,
                syntax_highlighting=syntax_highlighting,
            )
        else:
            debug_print(f"engine._from_folder skip nonfolder nonfile {subtree_n=}")
            continue
        _add_subtree(root=folder_tree_plus, subtree=subtree_plus)
    return folder_tree_plus


def _from_file(
    *,
    file_path: Path,
    syntax_highlighting: bool = True,
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
        components = parse_file(file_path)
    except Exception as e:
        debug_print(f"engine._from_file components Exception {e=}")
        components = []
    if syntax_highlighting:
        try:
            extension = file_path.suffix
            components = [
                _from_code(
                    lexer=extension,
                    code=component,
                )
                for component in components
            ]
        except Exception as e:
            debug_print(f"engine._from_file syntax highlighting exception {e=}")
    debug_print(f"engine._from_file got counts:", counts)
    debug_print(f"engine._from_file got components:", components)
    file_tree_plus = TreePlus(
        subtrees=components,
        category=Category.FILE,
        name=file_path.name,
        n_folders=0,
        n_files=0,
        n_tokens=counts.n_tokens,
        n_lines=counts.n_lines,
    )
    return file_tree_plus


def _from_code(
    *,
    lexer: str = None,
    code: Union[Syntax, str] = None,
) -> Union[Syntax, str]:
    "PRIVATE: either Syntax highlighting, or fallback to str"
    assert isinstance(lexer, str), f"tree_plus._from_code not str {code=}"
    if isinstance(code, Syntax):
        debug_print(f"engine._from_code skip {code=}")
        return code
    assert isinstance(code, str), f"tree_plus._from_code not str {code=}"
    assert len(code) > 0, f"tree_plus._from_code len 0 {code=}"
    try:
        lexer = lexer.lstrip(".")
        syntax = Syntax(code=code, lexer=lexer)
        if syntax.lexer is None:
            debug_print(f"engine.from_code no {lexer=}")
            return code
        debug_print(
            f"engine._from_code found Pygment {syntax.lexer.__class__.__name__}"
        )
        return syntax
    except Exception as e:
        debug_print(f"engine._from_code Syntax exception:\n{e=}\nfallback to str...")
        return code
