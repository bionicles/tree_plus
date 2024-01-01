# tests/test_units.py
import pytest


from tree_plus_cli import remove_trailing_space
from tree_plus_src import (
    traverse_directory,
    parse_file,
    parse_markers,
    TokenLineCount,
    count_tokens_lines,
    make_ignore,
    make_globs,
    debug_print,
)


# TODO: test debug_disabled


def test_units_make_ignore():
    ignored = make_ignore()
    debug_print(f"{ignored=}")
    assert len(ignored) > 0
    ignored2 = make_ignore(("stuff.exe",))
    debug_print(f"{ignored2=}")
    assert "stuff.exe" in ignored2
    # assert 0


def test_units_make_globs():
    globs = make_globs()
    debug_print(f"{globs=}")
    assert globs is None
    globs2 = make_globs(("*.rs",))
    debug_print(f"{globs2=}")
    assert globs2 == frozenset(("*.rs",))
    # assert 0


def test_units_remove_trailing_space():
    x = """bob                                   
bobbins"""
    y = """bob
bobbins"""
    assert remove_trailing_space(x) == y


# test traversal
def test_units_valid_directory():
    result = traverse_directory("tests/path_to_test")
    assert isinstance(result, list)


def test_units_file_as_directory():
    result = traverse_directory("tests/path_to_test/file.txt")
    assert isinstance(result, list)


# test parsing
@pytest.mark.parametrize(
    "file,expected",
    [
        ("tests/path_to_test/version.py", ['__version__ = "1.2.3"']),
        ("tests/path_to_test/file.py", ["def hello_world()"]),
        ("tests/path_to_test/file.md", ["# Hello, world!"]),
        ("tests/path_to_test/file.txt", []),
        (
            "tests/path_to_test/class_method_type.py",
            [
                'T = TypeVar("T")',
                "def parse_py(contents: str) -> List[str]",
                "class MyClass",
                "    def my_method(self)",
                """    @staticmethod
    def my_typed_method(obj: dict) -> int""",
                """    def my_multiline_signature_method(
        self,
        alice: str = None,
        bob: int = None,
    ) -> tuple""",
                """@lru_cache(maxsize=None)
def my_multiline_signature_function(
    tree: tuple = (),
    plus: str = "+",
) -> tuple""",
                "class LogLevelEnum(str, Enum)",
                "class Algo(BaseModel)",
                """@dataclass
class TestDataclass""",
                'A = TypeVar("A", str, bytes)',
                "def omega_yikes(file: str, expected: List[str]) -> bool",
                "def ice[T](args: Iterable[T] = ())",
                "class list[T]",
                "    def __getitem__(self, index: int, /) -> T",
                '''    @classmethod
    def from_code(cls, toolbox, code: bytes, score=None) -> "Algo"''',
                """    @classmethod
    def from_str(cls, toolbox, string: str, score=None) -> 'Algo'""",
            ],
        ),
    ],
)
def test_units_file_parsing(file, expected):
    print(f"{file=}")
    result = parse_file(file)
    if file.endswith(".py"):
        result = [r.code for r in result]
    print(f"{expected=}")
    print(f"{result=}")
    assert result == expected


def test_units_parse_todo():
    content = open("tests/more_languages/group5/rust_todo_test.rs", "r").read()
    result = parse_markers(content)
    assert result == [
        "TODO (Line 23): This todo tests parse_todo",
    ]


bug_todo_note = (
    "BUG: This is a bug.\nTODO: Fix this soon.\nNOTE: Interesting observation."
)


def test_units_parse_markers():
    results = parse_markers(bug_todo_note)
    assert results == [
        "BUG (Line 1): This is a bug.",
        "TODO (Line 2): Fix this soon.",
        "NOTE (Line 3): Interesting observation.",
    ]


# test counting
@pytest.mark.parametrize(
    "file,expected",
    [
        ("tests/path_to_test/file.py", TokenLineCount(n_tokens=19, n_lines=3)),
        ("tests/path_to_test/empty.py", TokenLineCount(n_tokens=0, n_lines=0)),
    ],
)
def test_units_token_counting(file, expected):
    result = count_tokens_lines(file)
    assert isinstance(result, TokenLineCount)
    assert result == expected
