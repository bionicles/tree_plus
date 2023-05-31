# tree_plus/tests/test_cli.py
import pytest
import subprocess

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


def test_main_entry_point():
    result = subprocess.run(["tree_plus", "--help"], capture_output=True)
    assert result.returncode == 0
