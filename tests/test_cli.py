# tree_plus/tests/test_cli.py
import subprocess
import platform
import pytest  # noqa: F401
import os

from rich import print

# the exact import path might need to be adjusted based on your setup
from tree_plus_cli import main
from tree_plus_src import (
    count_tokens_lines,
    TokenLineCount,
    traverse_directory,
    parse_file,
)


def test_imports():
    assert callable(main)
    assert callable(count_tokens_lines)
    assert callable(TokenLineCount)
    assert callable(traverse_directory)
    assert callable(parse_file)


def test_tree_plus_help():
    result = subprocess.run(["tree_plus", "--help"], capture_output=True, text=True)
    assert result.returncode == 0
    assert "Usage: tree_plus" in result.stdout
    result = subprocess.run(["tree_plus", "-h"], capture_output=True, text=True)
    assert result.returncode == 0
    assert "Usage: tree_plus" in result.stdout


def test_tree_plus_display_version():
    from tree_plus_src import __version__

    result = subprocess.run(["tree_plus", "-v"], capture_output=True, text=True)
    assert result.returncode == 0
    assert __version__ in result.stdout
    result = subprocess.run(["tree_plus", "-V"], capture_output=True, text=True)
    assert result.returncode == 0
    assert __version__ in result.stdout
    result = subprocess.run(["tree_plus", "--version"], capture_output=True, text=True)
    assert result.returncode == 0
    assert __version__ in result.stdout


# def test_tree_plus_glob():
#     result = subprocess.run(
#         ["tree_plus", "-g !*.rs", "tests/more_language_units/group5"],
#         capture_output=True,
#     )
#     assert result.returncode == 0
#     assert result.stderr == ""
#     assert "group5" in result.stdout
#     assert ".rs" not in result.stdout
#     assert "trait" not in result.stdout


def test_cli_on_root():
    path_to_tests = os.path.dirname(os.path.abspath(__file__))
    root_path = os.path.join(path_to_tests, "..")
    result = subprocess.run(
        ["tree_plus", root_path],
        capture_output=True,
        text=True,
    )
    print(result.stdout)
    assert result.returncode == 0
    assert result.stderr == ""

    # example of what you could test
    assert " tree_plus" in result.stdout
    assert " .github" in result.stdout
    assert " workflows" in result.stdout
    assert " tests" in result.stdout
    assert " dot_dot" in result.stdout
    assert " nested_dir" in result.stdout
    assert " more_languages" in result.stdout
    assert " group1" in result.stdout
    assert " group2" in result.stdout
    assert " group_todo" in result.stdout
    assert " group4" in result.stdout
    assert " group5" in result.stdout
    assert " path_to_test" in result.stdout
    assert " tree_plus_src" in result.stdout
    assert " unix.yml" in result.stdout
    assert " microsoft.yml" in result.stdout
    assert "# Tree Plus" in result.stdout
    assert "tokens" in result.stdout
    assert "lines" in result.stdout
    assert "def" in result.stdout
    assert "fn" in result.stdout
    assert "function" in result.stdout
    assert "class" in result.stdout
    assert "trait" in result.stdout
    assert "struct" in result.stdout
    assert "type" in result.stdout
    assert "impl" in result.stdout
    assert "module" in result.stdout
    assert "let" in result.stdout
    assert "->" in result.stdout
    assert "::" in result.stdout
    assert "provider" in result.stdout
    assert "resource" in result.stdout
    assert "pub" in result.stdout
    assert "mod" in result.stdout
    assert "struct" in result.stdout
    assert "enum" in result.stdout
    assert '): (.*)")' not in result.stdout

    if platform.system() == "Windows":
        assert 0, "Windows always fails because of extensive visual defects"
