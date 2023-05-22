import pytest

from src import traverse_directory


def test_valid_directory():
    result = traverse_directory("/path/to/test/directory")
    assert isinstance(result, list)


def test_invalid_directory():
    with pytest.raises(FileNotFoundError):
        traverse_directory("/non/existent/directory")


def test_file_as_directory():
    with pytest.raises(NotADirectoryError):
        traverse_directory("/path/to/test/file.txt")
