from typing import (
    List,
    Tuple,
    Optional,
    Union,
)
from functools import lru_cache
from pathlib import Path
from enum import Enum
from glob import glob
import re
import os

from rich.syntax import Syntax
from rich.tree import Tree


from tree_plus_src.debug import debug_print


# class syntax
class Category(Enum):
    ROOT = 1
    GLOB = 2
    FOLDER = 3
    FILE = 4
    COMPONENT = 5


@lru_cache
def is_glob(x: str) -> bool:
    "decide if a str is a glob"
    debug_print(f"is {x=} a glob?")
    if match := re.search(r"(\*)?\*|\?|\[(.\-.)+\]", x):
        debug_print(f"{match=}")
        return True
    return False


@lru_cache
def categorize(
    x: Union[Path, Tuple[str], str],
    check_strs_globs: bool = True,
    check_strs_paths: bool = True,
) -> Category:
    "assign x to a category enum DEFAULTS TO CHECK GLOBS AND PATHS"
    y = None
    if isinstance(x, Path):
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
    elif isinstance(x, str):
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
    else:
        raise TypeError(
            "engine.categorize not pathlib.Path, tuple[str] or str (glob|path|code) input {x=}"
        )
    if y:
        return y
    else:
        raise TypeError("engine.categorize failure {x=}")


class TreePlus:
    "PUBLIC: data structure"
    subtrees: List["TreePlus"]
    category: Category
    name: str
    n_folders: int
    n_files: int
    n_lines: int
    n_tokens: int
    # no other fields needed
    __slots__ = (
        "subtrees",
        "category",
        "name",
        "n_folders",
        "n_files",
        "n_lines",
        "n_tokens",
    )


def from_root(
    *,
    maybe_paths_tuple: Optional[Tuple[str]] = None,
    maybe_ignore: Optional[Tuple[str]] = None,
    maybe_globs: Optional[Tuple[str]] = None,
) -> TreePlus:
    "PUBLIC: Construct a TreePlus from tuple[str] or None"
    try:
        # default to current working directory
        if maybe_paths_tuple is None:
            debug_print(f"tree_plus.from_root defaulting to current working directory")
            return _from_path()
        else:
            if not all(isinstance(s, str) for s in maybe_paths_tuple):
                raise TypeError(
                    f"tree_plus.from_root tuple must contain str, got {maybe_paths_tuple}"
                )
            seeds = maybe_paths_tuple
            subtrees = _map_seeds(
                seeds=seeds,
                maybe_ignore=maybe_ignore,
                maybe_globs=maybe_globs,
            )
            root = _reduce_forest(forest=subtrees)
            return root
    except Exception as e:
        debug_print(f"tree_plus.from_root Exception {e=}")
        raise e


def into_rich_tree(*, root: TreePlus = None) -> Tree:
    "PUBLIC: Convert a TreePlus into a rich.tree.Tree to render"
    if not isinstance(root, TreePlus):
        raise TypeError(f"tree_plus.into_rich_tree got non-TreePlus {root=}")
    pass


def _map_seeds(
    *,
    seeds: Tuple[str],
    maybe_ignore: Optional[Tuple[str]] = None,
    maybe_globs: Optional[Tuple[str]] = None,
) -> Tuple[TreePlus]:
    "PRIVATE: grow a forest from a tuple of seed strings"
    pass


def _reduce_forest(*, forest: Tuple[TreePlus] = None) -> TreePlus:
    "PRIVATE: merge a forest of trees into a single root"
    pass


def _from_glob(
    *,
    pattern: Optional[str] = None,
    maybe_ignore: Optional[Tuple[str]] = None,
    maybe_globs: Optional[Tuple[str]] = None,
) -> TreePlus:
    "PRIVATE: construct a TreePlus from a glob pattern"
    try:
        hits = glob.glob(pattern)
        debug_print(f"tree_plus._from_glob {pattern=} {hits=}")
        forest = _map_seeds(hits)
        root = _reduce_forest(forest)
        return root
    except Exception as e:
        debug_print(f"tree_plus._from_glob {pattern=} Exception {e=}")
        raise e


def _from_path(
    *,
    path: Optional[Path] = None,
    maybe_ignore: Optional[Tuple[str]] = None,
    maybe_globs: Optional[Tuple[str]] = None,
) -> TreePlus:
    "PRIVATE: construct a TreePlus from a pathlib.Path, default to current working directory"
    if path is None:
        debug_print(f"tree_plus.from_path defaulting to current working directory")
        path = Path.cwd()
    else:
        if not isinstance(path, Path):
            raise TypeError(f"tree_plus::from_path: not a pathlib.Path: {path=}")
    try:
        if path.is_file():
            result = _from_file(
                path=path,
            )
        elif path.is_dir():
            result = _from_folder()
        else:
            raise TypeError(f"tree_plus::from_path: not a file or folder: {path=}")
        return result
    except Exception as e:
        debug_print(f"tree_plus::from_path Exception {e=}")
        raise e


def _from_folder(
    *,
    folder_path: Path,
    maybe_ignore: Optional[Tuple[str]] = None,
    maybe_globs: Optional[Tuple[str]] = None,
) -> TreePlus:
    "PRIVATE: walk a folder with"
    assert folder_path.is_dir(), f"tree_plus._from_folder got a non-dir {folder_path=}"
    pass


def _from_file(
    *,
    file_path: Path,
    maybe_ignore: Optional[Tuple[str]] = None,
    maybe_globs: Optional[Tuple[str]] = None,
) -> TreePlus:
    "PRIVATE: parse a file_path into a TreePlus"
    assert file_path.is_file(), f"tree_plus._from_file got a non-file {file_path=}"
    pass


def _from_code(
    *,
    extension: str = None,
    code: str = None,
) -> Union[Syntax, str]:
    "PRIVATE: either Syntax highlighting, or fallback to str"
    assert extension.startswith(
        "."
    ), f"tree_plus._from_component no leading '.' in {extension=}"
    assert isinstance(code, str), f"tree_plus._from_component not str {code=}"
    assert len(code) > 0, f"tree_plus._from_component len 0 {code=}"
    pass
