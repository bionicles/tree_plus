# tests/test_e2e.py
from typing import Optional
import subprocess
import platform
import rich
import sys
import os
import re

import pytest  # noqa: F401

from io import StringIO

from tree_plus_cli import main as tree_plus_main
import tree_plus_src as tree_plus


def extract_tokens_lines(line: str) -> Optional[tree_plus.TokenLineCount]:
    tree_plus.debug_print(f"extract_tokens_lines: {line=}")
    if match := re.search(
        r"\((?P<n_tokens>\d+) tokens, (?P<n_lines>\d+) lines\)", line
    ):
        tree_plus.debug_print(f"extract_tokens_lines: {match=}")
        groups = tree_plus.extract_groups(match)
        if "n_tokens" in groups and "n_lines" in groups:
            return tree_plus.TokenLineCount(
                n_tokens=int(groups["n_tokens"]), n_lines=int(groups["n_lines"])
            )
    return None


test_directory = "tests/path_to_test"

EXPECTATION_0 = """📄 file.py (19 tokens, 3 lines)
└── def hello_world()
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
    result = tree_plus.from_seed(f"{test_directory}/file.py")
    assert isinstance(result, tree_plus.TreePlus)
    result.render()
    result_str = result.into_str()
    assert unify_tree_symbols(result_str) == EXPECTATION_0


EXPECTATION_EMPTY = """📁 empty_folder (2 folders, 0 files)
└── 📁 is_empty (1 folder, 0 files)
"""

EMPTY = "tests/empty_folder"
EMPTY_MEANS_EMPTY = "tests/empty_folder/is_empty"


def test_e2e_empty_folder():
    if not os.path.exists(EMPTY):
        os.makedirs(EMPTY)
    if not os.path.exists(EMPTY_MEANS_EMPTY):
        os.makedirs(EMPTY_MEANS_EMPTY)
    result = tree_plus.from_seeds((EMPTY,))
    assert isinstance(result, tree_plus.TreePlus)
    result_str = result.into_str()
    print(result_str)
    assert unify_tree_symbols(result_str) == EXPECTATION_EMPTY


EXPECTATION_1 = """📁 path_to_test (1 folder, 6 files)
├── 📄 class_method_type.py (541 tokens, 103 lines)
│   ├── T = TypeVar("T")
│   ├── def parse_py(contents: str) -> List[str]
│   ├── class MyClass
│   ├──     @staticmethod
│   │       def physical_element_aval(dtype) -> core.ShapedArray
│   ├──     def my_method(self)
│   ├──     @staticmethod
│   │       def my_typed_method(obj: dict) -> int
│   ├──     def my_multiline_signature_method(
│   │           self,
│   │           alice: str = None,
│   │           bob: int = None,
│   │       ) -> tuple
│   ├── @lru_cache(maxsize=None)
│   │   def my_multiline_signature_function(
│   │       tree: tuple = (),
│   │       plus: str = "+",
│   │   ) -> tuple
│   ├── class LogLevelEnum(str, Enum)
│   ├── class Algo(BaseModel)
│   ├── @dataclass
│   │   class TestDataclass
│   ├── A = TypeVar("A", str, bytes)
│   ├── def omega_yikes(file: str, expected: List[str]) -> bool
│   ├── def ice[T](args: Iterable[T] = ())
│   ├── class list[T]
│   ├──     def __getitem__(self, index: int, /) -> T
│   ├──     @classmethod
│   │       def from_code(cls, toolbox, code: bytes, score=None) -> "Algo"
│   ├──     @classmethod
│   │       def from_str(cls, toolbox, string: str, score=None) -> 'Algo'
│   └── class Router(hk.Module)
├── 📄 empty.py (0 tokens, 0 lines)
├── 📄 file.md (12 tokens, 2 lines)
│   └── # Hello, world!
├── 📄 file.py (19 tokens, 3 lines)
│   └── def hello_world()
├── 📄 file.txt (11 tokens, 2 lines)
└── 📄 version.py (19 tokens, 2 lines)
    └── __version__ = "1.2.3"
"""


def test_e2e_single_directory():
    result = tree_plus.from_seeds((test_directory,))
    assert isinstance(result, tree_plus.TreePlus)
    print("test_e2e_single_directory tree\n")
    result.render()
    result_str = result.into_str()
    print("test_e2e_single_directory result_str\n", result_str)
    assert unify_tree_symbols(result_str) == unify_tree_symbols(EXPECTATION_1)


def test_e2e_multiple_directories():
    test_directory2 = "tests/path_to_test"
    with tree_plus.debug_disabled():
        result = tree_plus.from_seeds((test_directory, test_directory2))
    assert isinstance(result, tree_plus.TreePlus)
    print("test_e2e_multiple_directories result")
    result.render()
    result_str = result.into_str()
    print("test_e2e_multiple_directories result_str\n", result_str)
    unified_tree_symbols = unify_tree_symbols(result_str)
    assert unified_tree_symbols == unify_tree_symbols(EXPECTATION_1)
    assert unified_tree_symbols.count("📁 path_to_test") == 1


def test_e2e_glob():
    result = tree_plus.from_seed("tests/more_languages", maybe_globs=("*.*s",))
    assert isinstance(result, tree_plus.TreePlus)
    result_str = result.into_str()
    print(result_str)
    assert ".rs" in result_str
    assert ".ts" in result_str
    assert ".js" in result_str
    assert ".hs" in result_str
    assert ".cs" in result_str
    assert ".exs" in result_str
    assert ".java" not in result_str
    assert "group2" not in result_str
    assert "group4" in result_str
    assert "group5" in result_str
    assert "group_todo" in result_str


# Test ignore parameter
def test_e2e_ignore_parameter_filetype():
    result = tree_plus.from_seed(
        "tests/more_languages/group1",
        maybe_ignore=("*.kt",),
    )
    assert isinstance(result, tree_plus.TreePlus)
    result.render()
    result_str = result.into_str()
    assert ".kt" not in result_str


def test_e2e_ignore_parameter_directory():
    result = tree_plus.from_seed(
        "tests/more_languages",
        maybe_ignore=("group2",),
    )
    assert isinstance(result, tree_plus.TreePlus)
    result_str = result.into_str()
    print(result_str)
    assert "group2" not in result_str


# BUG: test_e2e_root_rs_glob detects an issue with deeply nested glob matches
# [
#     '📁 tree_plus (999 tokens, 186 lines)',
#     '┗━━ 📁 tests (0 tokens, 0 lines)',
#     '    ┗━━ 📁 more_languages',
#     '        ┣━━ 📁 group4 (890 tokens, 159 lines)',
#     '        ┃   ┗━━ 📄 rust_test.rs (890 tokens, 159 lines)',
#     '        ┗━━ 📁 group5 (109 tokens, 27 lines)',
#     '            ┗━━ 📄 rust_todo_test.rs (109 tokens, 27 lines)'
# ]
# TokenLineCount(n_tokens=890, n_lines=159)
# TokenLineCount(n_tokens=109, n_lines=27)
# BUG: 2. test_e2e_root_rs_glob detects single-root trees unnecessarily nesting in Root
# 📁 Root (1 folder, 2 files)
# └── 📁  (1 folder, 2 files)
def test_e2e_root_rs_glob():
    # disable_debug()
    # with tree_plus.debug_disabled():
    result = tree_plus.from_seed(".", maybe_globs=("*.rs",))
    result.render()
    result_str = result.into_str()
    result_lines = result_str.splitlines()
    first_lines = result_lines[:5]
    # NOTE: cool idea here:
    rust_lines = [line for line in result_lines[5:] if "📁" in line or "📄" in line]
    relevant_lines = first_lines + rust_lines
    tree_plus.safe_print(relevant_lines)
    total = tree_plus.TokenLineCount()
    tests_line = ""
    more_languages_line = ""
    with tree_plus.debug_disabled():
        for line in relevant_lines:
            if ".rs (" in line:
                line_count = extract_tokens_lines(line)
                print("rs line count:", line_count)
                total = tree_plus.add_tokens_lines(total, line_count)
            elif "tests" in line:
                tests_line = line
            elif "more_languages" in line:
                more_languages_line = line
    print("total_count:", total)
    # expectation = f"({total.n_tokens} tokens, {total.n_lines} lines)"
    print(f"{tests_line=}")
    print(f"{more_languages_line=}")
    # visual defect
    # assert not all(
    #     (
    #         "📁 Root (1 folder, 2 files)" in result_str,
    #         "└── 📁  (1 folder, 2 files)" in result_str,
    #     )
    # )
    assert "📁 tree_plus (" in result_str
    # assert 0
    # assert expectation in more_languages_line
    # assert expectation in tests_line


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

    rich.print("test_e2e_subprocess_glob", result.stdout)

    # Check that the process exited successfully
    assert result.returncode == 0

    # Assertions to verify the output
    # TODO: add more assertions here
    # assert "Expected content" in result.stdout
