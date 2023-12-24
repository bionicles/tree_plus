# tree_plus_src/debug.py
import os
from rich import print


def enable_debug():
    os.environ["DEBUG_TREE_PLUS"] = "1"


def disable_debug():
    os.environ["DEBUG_TREE_PLUS"] = "0"


def debug_enabled():
    return os.environ.get("DEBUG_TREE_PLUS") == "1"


def debug_print(*args, **kwargs):
    if debug_enabled():
        print(*args, **kwargs)
