# tests/test_units.py
import pytest

from src import traverse_directory
from src import count_tokens
from src import parse_file


# test traversal
def test_valid_directory():
    result = traverse_directory("path_to_test")
    assert isinstance(result, list)


def test_invalid_directory():
    with pytest.raises(FileNotFoundError):
        traverse_directory("/non/existent/directory")


def test_file_as_directory():
    with pytest.raises(NotADirectoryError):
        traverse_directory("/path_to_test/file.txt")


# test parsing
@pytest.mark.parametrize(
    "file,expected",
    [
        ("/path_to_test/file.py", True),
        ("/path_to_test/file.js", True),
        ("/path_to_test/file.md", True),
        ("/path_to_test/file.txt", False),
    ],
)
def test_file_parsing(file, expected):
    result = parse_file(file)
    if expected:
        assert len(result) > 0
    else:
        assert len(result) == 0


# test counting
@pytest.mark.parametrize(
    "file,expected",
    [
        ("/path_to_test/file.py", (2, 0)),
        ("/path_to_test/empty.py", (0, 0)),
    ],
)
def test_token_counting(file, expected):
    result = count_tokens(file)
    assert result == expected
