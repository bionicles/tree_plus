import pytest
from tree_plus import count_tokens


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
