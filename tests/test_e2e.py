# tests/test_e2e.py
import platform
import pytest  # noqa: F401
import rich

from rich import print

from tree_plus_cli import tree_plus, tree_to_string

test_directory = "tests/path_to_test"


def unify_tree_symbols(tree_string):
    if platform.system() == "Windows":
        # Map Windows tree symbols to Unix/MacOS tree symbols
        tree_string = tree_string.replace("\u2523", "\u251C")  # '┣' in Ubuntu/MacOS
        tree_string = tree_string.replace("\u2503", "\u2502")  # '┃' in Ubuntu/MacOS
        tree_string = tree_string.replace("\u2517", "\u2514")  # '┗' in Ubuntu/MacOS
        tree_string = tree_string.replace("\u2501", "\u2500")  # '━' in Ubuntu/MacOS
    return tree_string


EXPECTATION_1 = """📁 path_to_test (153 tokens, 38 lines)
┣━━ 📄 class_function.js (33 tokens, 9 lines)
┃   ┣━━ class MyClass
┃   ┗━━ function myFunction
┣━━ 📄 class_function_type.ts (45 tokens, 12 lines)
┃   ┣━━ type MyType
┃   ┣━━ class TsClass
┃   ┗━━ function tsFunction
┣━━ 📄 class_method_type.py (27 tokens, 8 lines)
┃   ┣━━ MyType
┃   ┣━━ class MyClass
┃   ┗━━ class MyClass -> def my_method
┣━━ 📄 empty.py (0 tokens, 0 lines)
┣━━ 📄 file.js (14 tokens, 3 lines)
┃   ┗━━ function helloWorld
┣━━ 📄 file.md (12 tokens, 2 lines)
┃   ┗━━ # Hello, world!
┣━━ 📄 file.py (11 tokens, 2 lines)
┃   ┗━━ def hello_world
┗━━ 📄 file.txt (11 tokens, 2 lines)
"""


def test_e2e_single_directory():
    result = tree_plus(test_directory)
    assert isinstance(result, rich.tree.Tree)
    result_str = tree_to_string(result)
    print(result_str)
    assert unify_tree_symbols(result_str) == unify_tree_symbols(EXPECTATION_1)


# Test multiple directories
EXPECTATION_2 = """Multiple Directories:
┣━━ 📁 path_to_test (153 tokens, 38 lines)
┃   ┣━━ 📄 class_function.js (33 tokens, 9 lines)
┃   ┃   ┣━━ class MyClass
┃   ┃   ┗━━ function myFunction
┃   ┣━━ 📄 class_function_type.ts (45 tokens, 12 lines)
┃   ┃   ┣━━ type MyType
┃   ┃   ┣━━ class TsClass
┃   ┃   ┗━━ function tsFunction
┃   ┣━━ 📄 class_method_type.py (27 tokens, 8 lines)
┃   ┃   ┣━━ MyType
┃   ┃   ┣━━ class MyClass
┃   ┃   ┗━━ class MyClass -> def my_method
┃   ┣━━ 📄 empty.py (0 tokens, 0 lines)
┃   ┣━━ 📄 file.js (14 tokens, 3 lines)
┃   ┃   ┗━━ function helloWorld
┃   ┣━━ 📄 file.md (12 tokens, 2 lines)
┃   ┃   ┗━━ # Hello, world!
┃   ┣━━ 📄 file.py (11 tokens, 2 lines)
┃   ┃   ┗━━ def hello_world
┃   ┗━━ 📄 file.txt (11 tokens, 2 lines)
┗━━ 📁 path_to_test (153 tokens, 38 lines)
    ┣━━ 📄 class_function.js (33 tokens, 9 lines)
    ┃   ┣━━ class MyClass
    ┃   ┗━━ function myFunction
    ┣━━ 📄 class_function_type.ts (45 tokens, 12 lines)
    ┃   ┣━━ type MyType
    ┃   ┣━━ class TsClass
    ┃   ┗━━ function tsFunction
    ┣━━ 📄 class_method_type.py (27 tokens, 8 lines)
    ┃   ┣━━ MyType
    ┃   ┣━━ class MyClass
    ┃   ┗━━ class MyClass -> def my_method
    ┣━━ 📄 empty.py (0 tokens, 0 lines)
    ┣━━ 📄 file.js (14 tokens, 3 lines)
    ┃   ┗━━ function helloWorld
    ┣━━ 📄 file.md (12 tokens, 2 lines)
    ┃   ┗━━ # Hello, world!
    ┣━━ 📄 file.py (11 tokens, 2 lines)
    ┃   ┗━━ def hello_world
    ┗━━ 📄 file.txt (11 tokens, 2 lines)
"""


def test_e2e_multiple_directories():
    test_directory2 = "tests/path_to_test"
    result = tree_plus(f"{test_directory},{test_directory2}")
    assert isinstance(result, rich.tree.Tree)
    result_str = tree_to_string(result)
    print(result_str)
    assert unify_tree_symbols(result_str) == unify_tree_symbols(EXPECTATION_2)


# def test_e2e_more_languages():
#     test_directory3 = "tests/more_languages"
#     result = tree_plus(test_directory3)
#     assert isinstance(result, rich.tree.Tree)
#     result_str = tree_to_string(result)
#     print(result_str)
#     assert result_str == ""


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
