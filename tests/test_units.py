# tests/test_units.py
import pytest

# from tree_plus.detritus import traverse_directory


# from tree_plus_cli import remove_trailing_space
# from tree_plus_src import (
#     parse_file,
#     parse_markers,
#     TokenLineCount,
#     count_tokens_lines,
#     make_ignore,
#     make_globs,
#     debug_print,
# )

import tree_plus_src as tree_plus
from tree_plus_src.count_tokens_lines import TokenizerName

# TODO: test debug_disabled


def test_engine_safe_print_macro_export():
    expectation = "#[macro_export]"
    print("with markup=True")
    output_markup = tree_plus.safe_print(
        expectation,
        capturing=True,
        markup=True,
    )
    print(output_markup)
    print("with markup=False")
    output = tree_plus.safe_print(
        expectation,
        capturing=True,
        markup=False,
    )
    print(output)
    assert expectation in output # type: ignore
    # unnecessary test here because the type hint is bool
    # print("with markup=None")
    # output_markup_none = tree_plus.safe_print(
    #     expectation,
    #     capturing=True,
    #     markup=False,
    # )
    # print(output_markup_none)
    # assert expectation in output_markup_none # type: ignore
    # https://rich.readthedocs.io/en/latest/markup.html#escaping
    from rich.markup import escape

    print(f"with markup=True on escape({expectation})")
    output_markup_true_escape = tree_plus.safe_print(
        escape(expectation),
        capturing=True,
        markup=True,
    )
    print(output_markup_true_escape)
    assert expectation in output_markup_true_escape # type: ignore
    print("with markup default")
    default_output = tree_plus.safe_print(
        expectation,
        capturing=True,
        # markup=False,
    )
    print(default_output)
    assert expectation in default_output # type: ignore


def test_engine_parse_ignore_default():
    ignored = tree_plus.parse_ignore()
    tree_plus.debug_print(f"{ignored=}")
    assert ignored == tree_plus.DEFAULT_IGNORE


def test_engine_parse_ignore_none():
    ignored_with_override = tree_plus.parse_ignore(
        maybe_ignore_tuple=None, override=True
    )
    tree_plus.debug_print(f"{ignored_with_override=}")
    assert ignored_with_override is None


def test_engine_parse_ignore_one():
    x = tree_plus.parse_ignore(maybe_ignore_tuple=("externals",))
    assert isinstance(x, tuple)
    assert "externals" in x


def test_engine_parse_ignore_one_override():
    input1 = ("externals",)
    x = tree_plus.parse_ignore(maybe_ignore_tuple=input1, override=True)
    assert x == input1


def test_units_parse_globs():
    globs = tree_plus.parse_globs()
    tree_plus.debug_print(f"{globs=}")
    assert globs is None
    globs2 = tree_plus.parse_globs(("*.rs",))
    tree_plus.debug_print(f"{globs2=}")
    assert globs2 == ("*.rs",)
    # assert 0


def test_units_remove_trailing_space():
    x = """bob                                   
bobbins"""
    y = """bob
bobbins"""
    assert tree_plus.engine.remove_trailing_space(x) == y


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
                """    @staticmethod
    def physical_element_aval(dtype) -> core.ShapedArray""",
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
                '    CRITICAL = "CRITICAL"',
                '    GREETING = "GREETING"',
                '    WARNING = "WARNING"',
                '    ERROR = "ERROR"',
                '    DEBUG = "DEBUG"',
                '    INFO = "INFO"',
                '    OFF = "OFF"',
                "class Thingy(BaseModel)",
                "    metric: float",
                """@dataclass
class TestDataclass""",
                "    tree: str",
                'A = TypeVar("A", str, bytes)',
                "def omega_yikes(file: str, expected: List[str]) -> bool",
                "def ice[T](args: Iterable[T] = ())",
                "class list[T]",
                "    def __getitem__(self, index: int, /) -> T",
                '''    @classmethod
    def from_code(cls, toolbox, code: bytes, score=None) -> "Thingy"''',
                """    @classmethod
    def from_str(cls, toolbox, string: str, score=None) -> \"Thingy\"""",
                "class Router(hk.Module)",
            ],
        ),
    ],
)
def test_units_file_parsing(file, expected):
    print(f"{file=}")
    result = tree_plus.parse_file(file)
    print(f"{expected=}")
    print(f"{result=}")
    assert result == expected


def test_units_parse_todo():
    content = open("tests/more_languages/group5/rust_todo_test.rs", "r").read()
    result = tree_plus.parse_markers(content)
    assert result == [
        "TODO: This todo tests parse_todo",
    ]


bug_todo_note = (
    "BUG: This is a bug.\nTODO: Fix this soon.\nNOTE: Interesting observation."
)


def test_units_parse_markers():
    results = tree_plus.parse_markers(bug_todo_note)
    assert results == [
        "BUG: This is a bug.",
        "TODO: Fix this soon.",
        "NOTE: Interesting observation.",
    ]


# test counting
# @pytest.mark.parametrize(
#     "file,expected",
#     [
#         (
#             "tests/path_to_test/file.py",
#             tree_plus.TokenLineCount(n_tokens=19, n_lines=3),
#         ),
#         (
#             "tests/path_to_test/empty.py",
#             tree_plus.TokenLineCount(n_tokens=0, n_lines=0),
#         ),
#     ],
# )
# def test_units_token_counting_gpt4(file, expected):
#     result = tree_plus.count_tokens_lines(file, tokenizer_name=TokenizerName.GPT4)
#     assert isinstance(result, tree_plus.TokenLineCount)
#     assert result == expected


@pytest.mark.parametrize(
    "file,expected",
    [
        (
            "tests/path_to_test/file.py",
            tree_plus.TokenLineCount(n_tokens=19, n_lines=3),
        ),
        (
            "tests/path_to_test/empty.py",
            tree_plus.TokenLineCount(n_tokens=0, n_lines=0),
        ),
    ],
)
def test_units_token_counting_gpt_4o(file, expected):
    result = tree_plus.count_tokens_lines(file, tokenizer_name=TokenizerName.GPT_4O)
    assert isinstance(result, tree_plus.TokenLineCount)
    assert result == expected


@pytest.mark.parametrize(
    "file,expected",
    [
        (
            "tests/more_languages/group7/absurdly_huge.jsonl",
            tree_plus.TokenLineCount(n_tokens=8_347, n_lines=126),
        ),
        (
            "tests/path_to_test/file.py",
            tree_plus.TokenLineCount(n_tokens=18, n_lines=3),
        ),
        (
            "tests/path_to_test/empty.py",
            tree_plus.TokenLineCount(n_tokens=0, n_lines=0),
        ),
    ],
)
def test_units_token_counting_wc(file, expected):
    result = tree_plus.count_tokens_lines(file)
    print(result)
    assert isinstance(result, tree_plus.TokenLineCount)
    assert result == expected
