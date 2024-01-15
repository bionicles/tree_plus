# tree_plus/tests/test_cli.py
import subprocess
import platform
import pytest  # noqa: F401
import os

from rich import print as rich_print

# the exact import path might need to be adjusted based on your setup
from tree_plus_cli import main
import tree_plus_src as tree_plus


def test_imports():
    assert callable(main)
    assert callable(tree_plus.count_tokens_lines)
    assert callable(tree_plus.TokenLineCount)

    assert callable(tree_plus.parse_file)


def test_tree_plus_on_parent_directory():
    result = subprocess.run(
        [
            "cd",
            "tests/dot_dot/nested_dir",
            "&&",
            "pytest",
            "-k",
            "test_tree_plus_dotdot",
            "-vv",
        ],
        capture_output=True,
        shell=True,
        text=True,
    )
    assert result.returncode == 0


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


def test_cli_syntax_highlighting_flag():
    result = subprocess.run(
        ["tree_plus", "-d", "-s", "tests/path_to_test", "tests/*.py"],
        capture_output=True,
        text=True,
    )
    assert result.returncode == 0
    stdout = result.stdout
    rich_print(stdout)
    assert "syntax_highlighting=True" in stdout
    assert "_from_file SYNTAX HIGHLIGHTING " in stdout
    assert "_get_lexer chose lexer=" in stdout
    assert "_syntax_highlight succeeded " in stdout
    assert "component(s) from file_path=" in stdout
    assert " tests" in stdout
    assert " test_cli.py" in stdout
    assert " Root" in stdout
    assert " parse_file.py" not in stdout
    assert " nested_dir" not in stdout
    result = subprocess.run(
        ["tree_plus", "-d", "-S", "tests/path_to_test", "tests/*.py"],
        capture_output=True,
        text=True,
    )
    assert result.returncode == 0
    stdout = result.stdout
    assert "syntax_highlighting=True" in stdout
    assert "component(s) from file_path=" in stdout
    assert "_from_file SYNTAX HIGHLIGHTING " in stdout
    assert "_get_lexer chose lexer=" in stdout
    assert "_syntax_highlight succeeded " in stdout
    assert "component(s) from file_path=" in stdout
    assert " tests" in stdout
    assert " test_cli.py" in stdout
    assert " Root" in stdout
    assert " parse_file.py" not in stdout
    assert " nested_dir" not in stdout
    result = subprocess.run(
        ["tree_plus", "-d", "--syntax", "tests/path_to_test", "tests/*.py"],
        capture_output=True,
        text=True,
    )
    assert result.returncode == 0
    stdout = result.stdout
    assert "syntax_highlighting=True" in stdout
    assert "component(s) from file_path=" in stdout
    assert "_from_file SYNTAX HIGHLIGHTING " in stdout
    assert "_get_lexer chose lexer=" in stdout
    assert "_syntax_highlight succeeded " in stdout
    assert "component(s) from file_path=" in stdout
    assert " tests" in stdout
    assert " test_cli.py" in stdout
    assert " Root" in stdout
    assert " parse_file.py" not in stdout
    assert " nested_dir" not in stdout


def test_cli_override():
    result = subprocess.run(
        ["tree_plus", "-o", "-i", "*.ini", "tests/dot_dot"],
        capture_output=True,
        text=True,
    )
    assert result.returncode == 0
    stdout = result.stdout
    assert " dot_dot" in stdout
    assert " nested_dir" in stdout
    assert " pytest.ini" not in stdout
    assert " .hypothesis" in stdout
    assert " __pycache__" in stdout
    assert " test_tp_dotdot.py" in stdout
    # "-i", "*.ini", removed for normal override test
    result = subprocess.run(
        [
            "tree_plus",
            "-O",
            "tests/dot_dot",
        ],
        capture_output=True,
        text=True,
    )
    assert result.returncode == 0
    stdout = result.stdout
    assert " dot_dot" in stdout
    assert " nested_dir" in stdout
    assert " .hypothesis" in stdout
    assert " pytest.ini" in stdout
    assert " __pycache__" in stdout
    assert " test_tp_dotdot.py" in stdout
    result = subprocess.run(
        ["tree_plus", "--override", "-i", ".hypothesis", "tests/dot_dot"],
        capture_output=True,
        text=True,
    )
    assert result.returncode == 0
    stdout = result.stdout
    assert " dot_dot" in stdout
    assert " nested_dir" in stdout
    assert " .hypothesis" not in stdout
    assert " pytest.ini" in stdout
    assert " __pycache__" in stdout
    assert " test_tp_dotdot.py" in stdout


def test_cli_on_tests():
    path_to_tests = os.path.dirname(os.path.abspath(__file__))
    tests = os.path.join(path_to_tests)
    with tree_plus.debug_disabled():
        result = subprocess.run(
            ["tree_plus", "-i", "README.md", tests],
            capture_output=True,
            text=True,
        )
    print(result.stdout)
    assert result.returncode == 0
    assert result.stderr == ""

    stdout = result.stdout
    # example of what you could test
    assert "__pycache__" not in stdout
    assert ".hypothesis" not in stdout
    assert ".pytest_cache" not in stdout
    # assert " tree_plus" in stdout
    # assert " .github" in stdout
    # assert " workflows" in stdout
    assert " tests" in stdout
    assert " dot_dot" in stdout
    assert " nested_dir" in stdout
    assert " more_languages" in stdout
    assert " group1" in stdout
    assert " group2" in stdout
    assert " group_todo" in stdout
    assert " group4" in stdout
    assert " group5" in stdout
    assert " path_to_test" in stdout
    # assert " tree_plus_src" in stdout
    # assert " unix.yml" in stdout
    # assert " microsoft.yml" in stdout
    assert "# Hello, world!" in stdout
    assert "tokens" in stdout
    assert "lines" in stdout
    assert "def" in stdout
    assert "fn" in stdout
    assert "function" in stdout
    assert "class" in stdout
    assert "trait" in stdout
    assert "struct" in stdout
    assert "type" in stdout
    assert "impl" in stdout
    assert "module" in stdout
    assert "let" in stdout
    assert "->" in stdout
    assert "::" in stdout
    assert "provider" in stdout
    assert "resource" in stdout
    assert "pub" in stdout
    assert "mod" in stdout
    assert "struct" in stdout
    assert "enum" in stdout
    assert '): (.*)")' not in stdout

    if platform.system() == "Windows":
        assert 0, "Windows always fails because of extensive visual defects"
