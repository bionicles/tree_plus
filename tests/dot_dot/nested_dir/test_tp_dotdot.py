import unicodedata
import subprocess
import pytest  # noqa f401
import re
import os

from rich import print

from tree_plus_cli import tree_plus, tree_to_string
from tree_plus_src import traverse_directory


BASE_PATH = os.path.dirname(os.path.abspath(__file__))
EXPECTATION_1 = """tests/dot_dot
┣━━ 📁 nested_dir
┃   ┗━━ 📄 test_tp_dotdot.py
┃       ┣━━ def ignore_tokens_lines_test
┃       ┣━━ def test_tree_plus_dotdot
┃       ┣━━ def show_invisible_chars
┃       ┗━━ def test_tree_plus_dotdot_traverse
┗━━ 📄 my_test_file.py
    ┗━━ def dot_dot_dot
"""


def ignore_tokens_lines_test(text: str) -> str:
    pattern = re.compile(r"\(\d+ tokens, \d+ lines\)")
    return re.sub(pattern, "", text)


def test_tree_plus_dotdot():
    dotdot_path = os.path.join(BASE_PATH, "..")
    tree = tree_plus(dotdot_path)
    tree_string = tree_to_string(tree)

    # Ignore tokens and lines count for tree_string
    tree_string = ignore_tokens_lines_test(tree_string)
    print("result")
    print(tree_string)
    # Do the same for EXPECTATION_1
    expectation = ignore_tokens_lines_test(EXPECTATION_1)
    print("expectation")
    print(expectation)

    print(
        subprocess.run(
            ["tree", "..", "-I", "__pycache__"], capture_output=True
        ).stdout.decode()
    )


def show_invisible_chars(s):
    return "".join(
        c if unicodedata.category(c) != "Cc" else unicodedata.name(c) for c in s
    )

    tree_string = tree_string.replace(" ", " ")
    expectation = expectation.replace(" ", " ")
    print(show_invisible_chars(tree_string))
    print(show_invisible_chars(expectation))
    assert tree_string == expectation


def test_tree_plus_dotdot_traverse():
    traversal = traverse_directory("..")
    print(traversal)
    assert traversal == ["../my_test_file.py", "../nested_dir/test_tp_dotdot.py"]
