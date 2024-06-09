from pathlib import Path
import pytest

from rich.syntax import Syntax
from rich.tree import Tree
from rich import print as rich_print

from tree_plus_src import (
    engine,
    debug_print,
    parse_ignore,
    AmortizedGlobs,
    amortize_globs,
)


def is_(types, thing) -> bool:
    return isinstance(thing, types)


def test_engine_categorize_root():
    category = engine.categorize((".",))
    assert category is engine.Category.ROOT, "root 1"
    category = engine.categorize(("tree_plus_src", "tests"))
    assert category is engine.Category.ROOT, "root 2"


def test_engine_is_glob():
    assert engine.is_glob("*.rs")
    assert engine.is_glob("?.rs")
    assert engine.is_glob("[a-zA-Z].rs")
    assert not engine.is_glob("tests")
    assert not engine.is_glob("tests/test_engine.py")


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
    category = engine.categorize(Path("tests"))
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


def test_engine_categorize_url():
    category = engine.categorize("https://news.ycombinator.com")
    assert category is engine.Category.URL, "file 1"
    category = engine.categorize("www.cnbc.com")
    assert category is engine.Category.URL, "file 2"
    category = engine.categorize("https://crates.io/search?q=async")
    assert category is engine.Category.URL, "file 3"


def test_engine__new():
    x = engine.TreePlus()
    assert is_(engine.TreePlus, x)
    assert x.is_component()
    assert x.name == ""
    assert x.n_folders == 0
    assert x.n_files == 0
    assert x.n_tokens == 0
    assert x.n_lines == 0


def test_engine__from_file():
    file_path = Path("tests/test_engine.py")
    x = engine._from_file(
        # noformat
        file_path=file_path,
    )
    print(x)
    assert is_(engine.TreePlus, x)
    assert x.is_file()
    y = engine.into_rich_tree(root=x)
    assert is_(Tree, y)
    engine.safe_print(y)
    tree_string = engine.tree_to_string(y)
    assert "def test_engine__from_file" in tree_string


def test_engine__from_glob():
    test_pattern = "tests/*.py"
    x = engine._from_glob(
        pattern=test_pattern,
    )
    print(x)
    assert is_(engine.TreePlus, x)
    x.render()
    assert x.is_glob()
    tree_string = x.into_str()
    assert f"tests/*.py ({len(x.subtrees)}" in tree_string


def test_engine__syntax_highlight():
    components = ["lambda x: x + 1", "lambda x: x + 2"]
    xs = engine._syntax_highlight(
        # noformat
        file_path=Path("nonexistent.exe"),
        components=components,
    )
    rich_print(xs)
    assert is_(list, xs)
    for xi in xs:
        assert is_(str, xi)
    ys = engine._syntax_highlight(file_path=Path("fake.py"), components=components)
    rich_print(ys)
    assert is_(list, ys)
    for yi in ys:
        # print(yi.lexer)
        assert is_(Syntax, yi)


def test_engine__from_folder():
    print("\nnormal print: test_engine__from_folder")
    folder_path = Path("tests/dot_dot")
    tree_plus_default_ignore = engine._from_folder(
        # noformat
        folder_path=folder_path,
    )
    debug_print(tree_plus_default_ignore)
    assert is_(engine.TreePlus, tree_plus_default_ignore)
    assert tree_plus_default_ignore.is_folder()
    rich_tree_default_ignore = engine.into_rich_tree(root=tree_plus_default_ignore)
    assert is_(Tree, rich_tree_default_ignore)
    engine.safe_print(rich_tree_default_ignore)
    default_ignore_tree_string = engine.tree_to_string(rich_tree_default_ignore)
    assert "test_tp_dotdot.py" in default_ignore_tree_string
    assert "__pycache__" not in default_ignore_tree_string


def test_engine__from_folder_with_ignore_None():
    print("\nnormal print: test_engine__from_folder")
    # again, ignoring nothing this time
    folder_path = Path("tests/dot_dot")
    tree_plus_no_ignore = engine._from_folder(
        folder_path=folder_path,
        maybe_ignore=None,
    )
    assert is_(engine.TreePlus, tree_plus_no_ignore)
    assert tree_plus_no_ignore.is_folder()
    rich_tree_no_ignore = engine.into_rich_tree(root=tree_plus_no_ignore)
    assert is_(Tree, rich_tree_no_ignore)
    engine.safe_print(rich_tree_no_ignore)
    no_ignore_tree_string = tree_plus_no_ignore.into_str()
    assert "test_tp_dotdot.py" in no_ignore_tree_string
    # does contain ignored things
    assert "__pycache__" in no_ignore_tree_string


def test_engine_amortize_globs():
    paths = (Path("tree_plus_src"), Path("tests"))
    globs = ("*.rs", "*.scm")
    amortized = amortize_globs(paths=paths, globs=globs)
    rich_print(amortized)
    assert isinstance(amortized, AmortizedGlobs)
    assert amortized.globs == globs
    assert amortized.matches == frozenset(
        {
            Path("tests/more_languages/group4"),
            Path("tests/more_languages/group5"),
            Path("tests/more_languages"),
            Path("tests/more_languages/group_lisp"),
            Path("."),
            Path("tests"),
            Path("tests/more_languages/group_lisp/test_scheme.scm"),
            Path("tests/more_languages/group5/rust_todo_test.rs"),
            Path("tests/more_languages/group4/rust_test.rs"),
        }
    )
    assert Path("tests") in amortized.matches
    assert Path("tests/path_to_test") not in amortized.matches
    assert Path("tests/more_languages") in amortized.matches
    # doesn't contain directories with 0 matches
    assert Path("tests/more_languages/group1") not in amortized.matches
    assert Path("tests/more_languages/group2") not in amortized.matches
    assert Path("tests/more_languages/group3") not in amortized.matches
    # does contain directories with matches:
    assert Path("tests/more_languages/group4") in amortized.matches
    assert Path("tests/more_languages/group5") in amortized.matches
    assert Path("tests/more_languages/group_lisp") in amortized.matches


def test_engine__from_folder_with_glob_no_ignore():
    print("\nnormal print: test_engine__from_folder_with_glob_no_ignore")
    # again, ignoring nothing this time
    tests_path = Path("tests/more_languages")
    rust_glob = "*.rs"
    paths = (tests_path,)
    globs = (rust_glob,)
    amortized_globs = amortize_globs(paths=paths, globs=globs)
    rich_print(amortized_globs)
    tree_plus_no_ignore = engine._from_folder(
        folder_path=tests_path,
        maybe_globs=amortized_globs,
    )
    assert is_(engine.TreePlus, tree_plus_no_ignore)
    assert tree_plus_no_ignore.is_folder()
    rich_tree_no_ignore = tree_plus_no_ignore.into_rich_tree()
    assert is_(Tree, rich_tree_no_ignore)
    engine.safe_print(rich_tree_no_ignore)
    rust_glob_tree_string = tree_plus_no_ignore.into_str()
    assert "rust_todo_test.rs" in rust_glob_tree_string
    assert "rust_todo_test.rs" in rust_glob_tree_string
    # does not contain ignored things
    assert "__pycache__" not in rust_glob_tree_string
    # does not contain other languages besides rust
    assert ".py" not in rust_glob_tree_string
    assert ".rs" in rust_glob_tree_string
    assert "more_languages" in rust_glob_tree_string
    assert "group4" in rust_glob_tree_string
    assert "group5" in rust_glob_tree_string


def test_engine__from_seed_for_file():
    file_path = Path("tests/test_engine.py")
    file_tree_plus = engine._from_seed(seed_path=file_path)
    rich_print(file_tree_plus)
    rich_file_tree = file_tree_plus.into_rich_tree()
    assert is_(Tree, rich_file_tree)
    engine.safe_print(rich_file_tree)
    rich_file_tree_str = file_tree_plus.into_str()
    assert is_(str, rich_file_tree_str)
    assert "test_engine__from_seed_for_file" in rich_file_tree_str


def test_engine__from_seed_for_folder():
    folder_path = Path("tests/path_to_test")
    folder_tree_plus = engine._from_seed(seed_path=folder_path)
    rich_print(folder_tree_plus)
    rich_folder_tree = folder_tree_plus.into_rich_tree()
    assert is_(Tree, rich_folder_tree)
    engine.safe_print(rich_folder_tree)
    rich_folder_tree_str = folder_tree_plus.into_str()
    assert is_(str, rich_folder_tree_str)
    assert "def my_multiline_signature_method" in rich_folder_tree_str
    assert ".txt" in rich_folder_tree_str


def test_engine__from_seed_for_folder_with_glob():
    folder_path = Path("tests/path_to_test")
    amortized_globs = amortize_globs(
        paths=(folder_path,),
        globs=("*.py",),
    )
    # ignore_txt = parse_ignore(maybe_ignore_tuple=("*.txt",))
    folder_tree_plus = engine._from_seed(
        seed_path=folder_path,
        # maybe_ignore=ignore_txt,
        maybe_globs=amortized_globs,
    )
    rich_print(folder_tree_plus)
    rich_folder_tree = folder_tree_plus.into_rich_tree()
    assert is_(Tree, rich_folder_tree)
    engine.safe_print(rich_folder_tree)
    rich_folder_tree_str = folder_tree_plus.into_str()
    assert is_(str, rich_folder_tree_str)
    assert "def my_multiline_signature_method" in rich_folder_tree_str
    assert ".txt" not in rich_folder_tree_str


def test_engine__from_seed_for_folder_with_ignore():
    folder_path = Path("tests/path_to_test")
    ignore_txt = parse_ignore(maybe_ignore_tuple=("*.txt",))
    folder_tree_plus = engine._from_seed(
        seed_path=folder_path,
        maybe_ignore=ignore_txt,
    )
    rich_print(folder_tree_plus)
    rich_folder_tree = folder_tree_plus.into_rich_tree()
    assert is_(Tree, rich_folder_tree)
    engine.safe_print(rich_folder_tree)
    rich_folder_tree_str = folder_tree_plus.into_str()
    assert is_(str, rich_folder_tree_str)
    assert "def my_multiline_signature_method" in rich_folder_tree_str
    assert ".txt" not in rich_folder_tree_str


def test_engine__map_seeds():
    folder_seed = "tests/path_to_test"
    file_seed = "tests/test_engine.py"
    glob_seed = "*.py"
    seeds = (folder_seed, file_seed, glob_seed)
    forest = engine._map_seeds(
        seeds=seeds,
    )
    rich_print(forest)
    tree1 = forest[0].into_rich_tree()
    tree2 = forest[1].into_rich_tree()
    rich_print(tree1)
    rich_print(tree2)
    str1 = forest[0].into_str()
    str2 = forest[1].into_str()
    assert "def my_multiline_signature_method" in str1
    assert ".txt" not in str1
    assert "def test_engine__map_seeds" in str2


def test_engine__reduce_forest():
    folder_seed = "tests/path_to_test"
    file_seed = "tests/test_engine.py"
    glob_seed = "*.py"
    seeds = (folder_seed, file_seed, glob_seed)
    forest = engine._map_seeds(
        seeds=seeds,
    )
    root = engine._reduce_forest(
        forest=forest,
    )
    root_rich_tree = root.into_rich_tree()
    rich_print(root_rich_tree)
    root_str = root.into_str()
    assert "path_to_test (1 folder," in root_str
    assert "tokens," in root_str
    assert "test_engine.py" in root_str


def test_engine_from_seeds():
    folder_seed = "tests/path_to_test"
    file_seed = "tests/test_engine.py"
    glob_seed = "Mak*"
    seeds = ("tree_plus_src", folder_seed, file_seed, glob_seed)
    root = engine.from_seeds(seeds, maybe_ignore=("debug.py",))
    rich_print(root.into_rich_tree())
    root_str = root.into_str()
    assert "path_to_test" in root_str
    assert "test_engine.py" in root_str
    assert "count_tokens_lines.py" in root_str  # tree_plus_src
    assert "__pycache__" not in root_str  # default ignore
    assert "def my_multiline_signature_function" in root_str  # tests/path_to_test
    assert "def debug_print" not in root_str  # explicit ignore
    assert "Mak* (" in root_str  # glob seed
    assert "Makefile" in root_str  # tests glob match
    # assert 0


def test_engine_from_seeds_override():
    folder_seed = "tests/path_to_test"
    file_seed = "tests/test_engine.py"
    # glob_seed = "*.py" # this screwed up the ignore override btw
    seeds = ("tree_plus_src", folder_seed, file_seed)
    root = engine.from_seeds(seeds, maybe_ignore=("debug.py",), override_ignore=True)
    rich_print(root.into_rich_tree())
    root_str = root.into_str()
    assert "__pycache__" in root_str
    assert "def my_multiline_signature_function" in root_str
    assert "def debug_print" not in root_str
    assert "count_tokens_lines.py" in root_str


def test_engine_class_treeplus_stats():
    tp1 = engine.TreePlus()
    tp1_stats = tp1.stats()
    # assert tp1_stats == f"0 folders, 0 files, 0 lines, 0 tokens"
    assert tp1_stats == f"0 folder(s), 0 file(s), 0 line(s), 0 token(s)"
    tp1.n_folders = 1
    tp1.n_files = 1
    tp1.n_lines = 1
    tp1.n_tokens = 1
    tp1_stats = tp1.stats()
    # assert tp1_stats == f"1 folder, 1 file, 1 line, 1 token"
    assert tp1_stats == f"1 folder(s), 1 file(s), 1 line(s), 1 token(s)"
    tp1.n_folders = 2
    tp1.n_files = 2
    tp1.n_lines = 2
    tp1.n_tokens = 2
    tp1_stats = tp1.stats()
    # assert tp1_stats == f"2 folders, 2 files, 2 lines, 2 tokens"
    assert tp1_stats == f"2 folder(s), 2 file(s), 2 line(s), 2 token(s)"
    tp1.n_folders = None
    tp1.n_files = None
    tp1.n_lines = None
    tp1.n_tokens = None
    tp1_stats = tp1.stats()
    # assert tp1_stats == f"2 folders, 2 files, 2 lines, 2 tokens"
    assert tp1_stats == f"(?) folder(s), (?) file(s), (?) line(s), (?) token(s)"
