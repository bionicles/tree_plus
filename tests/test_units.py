# tests/test_units.py
import pytest

from tree_plus_src import (
    traverse_directory,
    parse_file,
    parse_markers,
    TokenLineCount,
    count_tokens_lines,
    count_directory_tokens_lines,
)


# test traversal
def test_valid_directory():
    result = traverse_directory("tests/path_to_test")
    assert isinstance(result, list)


# def test_invalid_directory():
#     with pytest.raises(NotADirectoryError):
#         traverse_directory("tests/non/existent/directory")


def test_file_as_directory():
    result = traverse_directory("tests/path_to_test/file.txt")
    assert isinstance(result, list)


# test parsing
@pytest.mark.parametrize(
    "file,expected",
    [
        ("tests/path_to_test/file.py", ["def hello_world"]),
        ("tests/path_to_test/file.js", ["function helloWorld"]),
        ("tests/path_to_test/file.md", ["# Hello, world!"]),
        ("tests/path_to_test/file.txt", []),
        (
            "tests/path_to_test/class_method_type.py",
            ["MyType", "class MyClass", "class MyClass -> def my_method"],
        ),
        (
            "tests/path_to_test/class_function.js",
            ["class MyClass", "function myFunction"],
        ),
        (
            "tests/path_to_test/class_function_type.ts",
            ["type MyType", "class TsClass", "function tsFunction"],
        ),
    ],
)
def test_file_parsing(file, expected):
    result = parse_file(file)
    print(f"{expected=}")
    print(f"{result=}")
    assert result == expected


def test_parse_todo():
    content = open("tests/more_languages/group5/rust_todo_test.rs", "r").read()
    result = parse_markers(content)
    assert result == [
        "TODO (Line 23): This todo tests parse_todo",
    ]


bug_todo_note = (
    "BUG: This is a bug.\nTODO: Fix this soon.\nNOTE: Interesting observation."
)


def test_parse_markers():
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
        ("tests/path_to_test/file.py", TokenLineCount(n_tokens=11, n_lines=2)),
        ("tests/path_to_test/empty.py", TokenLineCount(n_tokens=0, n_lines=0)),
    ],
)
def test_token_counting(file, expected):
    result = count_tokens_lines(file)
    assert isinstance(result, TokenLineCount)
    assert result == expected


def test_directory_token_line_counting():
    dir_path = "tests/path_to_test/"
    expected = TokenLineCount(
        n_tokens=153, n_lines=38
    )  # Manually calculated total counts.
    result = count_directory_tokens_lines(dir_path)
    assert isinstance(result, TokenLineCount)
    assert result == expected
