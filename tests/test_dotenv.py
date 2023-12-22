# tests/test_dotenv.py
import pytest
import os

from tree_plus_src import debug_print


def test_dotenv():
    assert os.environ.get("DEBUG_TREE_PLUS") == "1"
    debug_print("bob")
    # assert 0
