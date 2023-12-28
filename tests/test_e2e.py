# tests/test_e2e.py
import subprocess
import platform
import rich
import sys
import os

import pytest  # noqa: F401

from rich import print
from io import StringIO

from tree_plus_cli import main as tree_plus_main, tree_plus, tree_to_string

test_directory = "tests/path_to_test"

EXPECTATION_0 = """📄 file.py (19 tokens, 3 lines)
┗━━ def hello_world()
"""


def unify_tree_symbols(tree_string):
    if platform.system() == "Windows":
        # Existing mappings
        tree_string = tree_string.replace("\u2523", "\u251C")  # '┣' in Ubuntu/MacOS
        tree_string = tree_string.replace("\u2503", "\u2502")  # '┃' in Ubuntu/MacOS

        # Add mappings for the characters causing the test failures
        tree_string = tree_string.replace("\u2517", "\u2514")  # '┗' in Ubuntu/MacOS
        tree_string = tree_string.replace("\u2501", "\u2500")  # '━' in Ubuntu/MacOS

        # Additional mappings for the characters in the test failures
        tree_string = tree_string.replace("\u2514\u2500\u2500", "\u2517\u2501\u2501")
    return tree_string


def test_e2e_single_file():
    result = tree_plus(f"{test_directory}/file.py")
    assert isinstance(result, rich.tree.Tree)
    result_str = tree_to_string(result)
    print(result_str)
    assert unify_tree_symbols(result_str) == EXPECTATION_0


EXPECTATION_EMPTY = """📁 empty_folder (0 tokens, 0 lines)
┗━━ 📁 is_empty (0 tokens, 0 lines)
"""

EMPTY = "tests/empty_folder"
EMPTY_MEANS_EMPTY = "tests/empty_folder/is_empty"


def test_e2e_empty_folder():
    if not os.path.exists(EMPTY):
        os.makedirs(EMPTY)
    if not os.path.exists(EMPTY_MEANS_EMPTY):
        os.makedirs(EMPTY_MEANS_EMPTY)
    result = tree_plus(EMPTY)
    assert isinstance(result, rich.tree.Tree)
    result_str = tree_to_string(result)
    print(result_str)
    assert unify_tree_symbols(result_str) == EXPECTATION_EMPTY


EXPECTATION_1 = """📁 path_to_test (776 tokens, 157 lines)
┣━━ 📄 class_function.js (33 tokens, 9 lines)
┃   ┣━━ class MyClass
┃   ┣━━     myMethod
┃   ┗━━ function myFunction
┣━━ 📄 class_function_type.ts (169 tokens, 40 lines)
┃   ┣━━ type MyType
┃   ┣━━ interface MyInterface
┃   ┣━━ class TsClass
┃   ┣━━     myMethod
┃   ┣━━ class TicketsComponent implements AfterViewInit
┃   ┣━━     async myAsyncMethod
┃   ┣━━ function tsFunction
┃   ┣━━ const myArrowFunction: =>
┃   ┣━━ const myArrow: =>
┃   ┣━━ const myAsyncArrowFunction: async =>
┃   ┣━━ const myAsyncArrow: async =>
┃   ┗━━ let myWeirdArrow: =>
┣━━ 📄 class_method_type.py (499 tokens, 96 lines)
┃   ┣━━ T = TypeVar("T")
┃   ┣━━ def parse_py(contents: str) -> List[str]
┃   ┣━━ class MyClass
┃   ┣━━     def my_method(self)
┃   ┣━━     @staticmethod
┃   ┃       def my_typed_method(obj: dict) -> int
┃   ┣━━     def my_multiline_signature_method(
┃   ┃           self,
┃   ┃           alice: str = None,
┃   ┃           bob: int = None,
┃   ┃       ) -> tuple
┃   ┣━━ @lru_cache(maxsize=None)
┃   ┃   def my_multiline_signature_function(
┃   ┃       tree: tuple = (),
┃   ┃       plus: str = "+",
┃   ┃   ) -> tuple
┃   ┣━━ class LogLevelEnum(str, Enum)
┃   ┣━━ class Algo(BaseModel)
┃   ┣━━ @dataclass
┃   ┃   class TestDataclass
┃   ┣━━ A = TypeVar("A", str, bytes)
┃   ┣━━ def omega_yikes(file: str, expected: List[str]) -> bool
┃   ┣━━ def ice[T](args: Iterable[T] = ())
┃   ┣━━ class list[T]
┃   ┣━━     def __getitem__(self, index: int, /) -> T
┃   ┣━━     @classmethod
┃   ┃       def from_code(cls, toolbox, code: bytes, score=None) -> "Algo"
┃   ┗━━     @classmethod
┃           def from_str(cls, toolbox, string: str, score=None) -> 'Algo'
┣━━ 📄 empty.py
┣━━ 📄 file.js (14 tokens, 3 lines)
┃   ┗━━ function helloWorld
┣━━ 📄 file.md (12 tokens, 2 lines)
┃   ┗━━ # Hello, world!
┣━━ 📄 file.py (19 tokens, 3 lines)
┃   ┗━━ def hello_world()
┣━━ 📄 file.txt (11 tokens, 2 lines)
┗━━ 📄 version.py (19 tokens, 2 lines)
    ┗━━ __version__ = "1.2.3"
"""


def test_e2e_single_directory():
    result = tree_plus(test_directory)
    assert isinstance(result, rich.tree.Tree)
    result_str = tree_to_string(result)
    print(result_str)
    assert unify_tree_symbols(result_str) == unify_tree_symbols(EXPECTATION_1)


def test_e2e_multiple_directories():
    test_directory2 = "tests/path_to_test"
    result = tree_plus(f"{test_directory},{test_directory2}")
    assert isinstance(result, rich.tree.Tree)
    result_str = tree_to_string(result)
    print(result_str)
    assert unify_tree_symbols(result_str) == unify_tree_symbols(EXPECTATION_1)


def test_e2e_glob():
    result = tree_plus("tests/more_languages", globs=("*.*s",))
    assert isinstance(result, rich.tree.Tree)
    result_str = tree_to_string(result)
    print(result_str)
    assert ".rs" in result_str
    assert ".ts" in result_str
    assert ".hs" in result_str
    assert ".cs" in result_str
    assert ".exs" in result_str
    assert ".java" not in result_str
    assert "group1" not in result_str
    assert "group2" not in result_str
    assert "group4" in result_str
    assert "group5" in result_str
    assert "group_todo" in result_str


# Test ignore parameter
def test_e2e_ignore_parameter_filetype():
    result = tree_plus("tests/more_languages/group1", ignore=("*.kt",))
    assert isinstance(result, rich.tree.Tree)
    result_str = tree_to_string(result)
    print(result_str)
    assert ".kt" not in result_str


def test_e2e_ignore_parameter_directory():
    result = tree_plus("tests/more_languages", ignore=("group2",))
    assert isinstance(result, rich.tree.Tree)
    result_str = tree_to_string(result)
    print(result_str)
    assert "group2" not in result_str


def test_e2e_main_glob():
    # Redirect stdout to capture the output for testing
    old_stdout = sys.stdout
    sys.stdout = StringIO()

    # Simulate command-line arguments
    test_args = ["tree_plus_cli.py", "tree_plus_src/*.py"]
    with pytest.raises(SystemExit) as pytest_wrapped_e:
        tree_plus_main(test_args)

    # Capture and process the output
    output = sys.stdout.getvalue()
    sys.stdout = old_stdout
    print(output)

    # Assertions to verify the output
    # TODO: add more assertions here
    # assert "Expected content" in output
    assert pytest_wrapped_e.type == SystemExit
    assert pytest_wrapped_e.value.code == 0


def test_e2e_subprocess_glob():
    # Run the CLI command and capture the output
    result = subprocess.run(
        ["python", "tree_plus_cli.py", "tree_plus_src/*.py"],
        capture_output=True,
        text=True,
    )

    print("test_e2e_subprocess_glob", result)

    # Check that the process exited successfully
    assert result.returncode == 0

    # Assertions to verify the output
    # TODO: add more assertions here
    # assert "Expected content" in result.stdout
