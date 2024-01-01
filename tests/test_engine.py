import pathlib

import pytest

from tree_plus_src import engine


def is_(types, thing) -> bool:
    return isinstance(thing, types)


def nis_(types, thing) -> bool:
    return not is_(types, thing)


def test_engine_categorize_root():
    category = engine.categorize((".",))
    assert category is engine.Category.ROOT, "root 1"
    category = engine.categorize(("tree_plus_src", "tests"))
    assert category is engine.Category.ROOT, "root 2"


def test_engine_categorize_glob():
    category = engine.categorize("*.rs")
    assert category is engine.Category.GLOB, "glob 1"
    category = engine.categorize("?.rs")
    assert category is engine.Category.GLOB, "glob 2"
    category = engine.categorize("[a-zA-Z].rs")
    assert category is engine.Category.GLOB, "glob 3"


def test_engine_categorize_folder():
    category = engine.categorize("tests")
    assert category is engine.Category.FOLDER, "folder 1"
    category = engine.categorize(pathlib.Path("tests"))
    assert category is engine.Category.FOLDER, "folder 2"
    category = engine.categorize(".")
    assert category is engine.Category.FOLDER, "folder 3"
    category = engine.categorize("..")
    assert category is engine.Category.FOLDER, "folder 4"


def test_engine_categorize_file():
    category = engine.categorize("tests/test_engine.py")
    assert category is engine.Category.FILE, "file 1"
    category = engine.categorize("tree_plus_cli.py")
    assert category is engine.Category.FILE, "file 2"
    category = engine.categorize("tests/../tree_plus_cli.py")
    assert category is engine.Category.FILE, "file 3"
