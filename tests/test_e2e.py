# tests/test_e2e.py
import pytest  # noqa: F401
import rich

from tree_plus_cli import tree_plus, tree_to_string

test_directory = "tests/path_to_test"

EXPECTATION_1 = """tests/path_to_test (153 tokens, 38 lines)
┗━━ 📁 path_to_test (153 tokens, 38 lines)
    ┣━━ 📄 file.py (11 tokens, 2 lines)
    ┃   ┗━━ def hello_world
    ┣━━ 📄 file.txt (11 tokens, 2 lines)
    ┣━━ 📄 class_function_type.ts (45 tokens, 12 lines)
    ┃   ┣━━ type MyType
    ┃   ┣━━ class TsClass
    ┃   ┗━━ function tsFunction
    ┣━━ 📄 file.js (14 tokens, 3 lines)
    ┃   ┗━━ function helloWorld
    ┣━━ 📄 empty.py (0 tokens, 0 lines)
    ┣━━ 📄 file.md (12 tokens, 2 lines)
    ┃   ┗━━ # Hello, world!
    ┣━━ 📄 class_function.js (33 tokens, 9 lines)
    ┃   ┣━━ class MyClass
    ┃   ┗━━ function myFunction
    ┗━━ 📄 class_method_type.py (27 tokens, 8 lines)
        ┣━━ MyType
        ┣━━ class MyClass
        ┗━━ class MyClass -> def my_method
"""


def test_e2e_single_directory():
    result = tree_plus(test_directory)
    assert isinstance(result, rich.tree.Tree)
    result_str = tree_to_string(result)
    print(result_str)
    assert tree_to_string(result) == EXPECTATION_1


# Test multiple directories
EXPECTATION_2 = """Multiple Directories:
┣━━ tests/path_to_test (153 tokens, 38 lines)
┃   ┗━━ 📁 path_to_test (153 tokens, 38 lines)
┃       ┣━━ 📄 file.py (11 tokens, 2 lines)
┃       ┃   ┗━━ def hello_world
┃       ┣━━ 📄 file.txt (11 tokens, 2 lines)
┃       ┣━━ 📄 class_function_type.ts (45 tokens, 12 lines)
┃       ┃   ┣━━ type MyType
┃       ┃   ┣━━ class TsClass
┃       ┃   ┗━━ function tsFunction
┃       ┣━━ 📄 file.js (14 tokens, 3 lines)
┃       ┃   ┗━━ function helloWorld
┃       ┣━━ 📄 empty.py (0 tokens, 0 lines)
┃       ┣━━ 📄 file.md (12 tokens, 2 lines)
┃       ┃   ┗━━ # Hello, world!
┃       ┣━━ 📄 class_function.js (33 tokens, 9 lines)
┃       ┃   ┣━━ class MyClass
┃       ┃   ┗━━ function myFunction
┃       ┗━━ 📄 class_method_type.py (27 tokens, 8 lines)
┃           ┣━━ MyType
┃           ┣━━ class MyClass
┃           ┗━━ class MyClass -> def my_method
┗━━ tests/path_to_test (153 tokens, 38 lines)
    ┗━━ 📁 path_to_test (153 tokens, 38 lines)
        ┣━━ 📄 file.py (11 tokens, 2 lines)
        ┃   ┗━━ def hello_world
        ┣━━ 📄 file.txt (11 tokens, 2 lines)
        ┣━━ 📄 class_function_type.ts (45 tokens, 12 lines)
        ┃   ┣━━ type MyType
        ┃   ┣━━ class TsClass
        ┃   ┗━━ function tsFunction
        ┣━━ 📄 file.js (14 tokens, 3 lines)
        ┃   ┗━━ function helloWorld
        ┣━━ 📄 empty.py (0 tokens, 0 lines)
        ┣━━ 📄 file.md (12 tokens, 2 lines)
        ┃   ┗━━ # Hello, world!
        ┣━━ 📄 class_function.js (33 tokens, 9 lines)
        ┃   ┣━━ class MyClass
        ┃   ┗━━ function myFunction
        ┗━━ 📄 class_method_type.py (27 tokens, 8 lines)
            ┣━━ MyType
            ┣━━ class MyClass
            ┗━━ class MyClass -> def my_method
"""


def test_e2e_multiple_directories():
    test_directory2 = "tests/path_to_test"
    result = tree_plus(f"{test_directory},{test_directory2}")
    assert isinstance(result, rich.tree.Tree)
    result_str = tree_to_string(result)
    print(result_str)
    assert result_str == EXPECTATION_2


# Test ignore parameter
def test_e2e_ignore_parameter_filetype():
    result = tree_plus("tests/more_languages/group1", ignore={"*.kt"})
    assert isinstance(result, rich.tree.Tree)
    result_str = tree_to_string(result)
    print(result_str)
    assert ".kt" not in result_str


def test_e2e_ignore_parameter_directory():
    result = tree_plus("tests/more_languages", ignore={"group2"})
    assert isinstance(result, rich.tree.Tree)
    result_str = tree_to_string(result)
    print(result_str)
    assert "group2" not in result_str
