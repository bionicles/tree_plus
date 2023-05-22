import pytest
from src import tree_plus


def test_e2e_single_directory():
    result = tree_plus("/path/to/test/directory")
    assert "file.py" in result
    assert "file.js" in result
    assert "file.md" in result


def test_e2e_multiple_directories():
    result = tree_plus("/path/to/test/directory1,/path/to/test/directory2")
    assert "file.py" in result
    assert "file.js" in result
    assert "file.md" in result
