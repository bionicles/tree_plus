import pytest
from tree_plus import parse_file


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
