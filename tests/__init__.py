import pytest
from tree_plus import traverse_directory, parse_file, count_tokens

# File Traversal Module Tests


def test_valid_directory():
    result = traverse_directory("/path/to/test/directory")
    assert isinstance(result, list)


def test_invalid_directory():
    with pytest.raises(FileNotFoundError):
        traverse_directory("/non/existent/directory")


def test_file_as_directory():
    with pytest.raises(NotADirectoryError):
        traverse_directory("/path/to/test/file.txt")


# File Parsing Module Tests


@pytest.mark.parametrize(
    "file,expected",
    [
        ("/path/to/test/file.py", True),
        ("/path/to/test/file.js", True),
        ("/path/to/test/file.md", True),
        ("/path/to/test/file.txt", False),
    ],
)
def test_file_parsing(file, expected):
    result = parse_file(file)
    if expected:
        assert len(result) > 0
    else:
        assert len(result) == 0


# Token Counting Module Tests


@pytest.mark.parametrize(
    "file,expected",
    [
        ("/path/to/test/file.py", (100, 200)),
        ("/path/to/test/empty/file.py", (0, 0)),
        ("/path/to/test/file.txt", (0, 0)),
    ],
)
def test_token_counting(file, expected):
    result = count_tokens(file)
    assert result == expected
