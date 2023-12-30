# tree_plus_src/debug.py
from functools import lru_cache
import os
from rich import print


# def disable_debug():
#     os.environ["DEBUG_TREE_PLUS"] = "0"
#     debug_enabled.cache_clear()


@lru_cache
def debug_enabled():
    return os.environ.get("DEBUG_TREE_PLUS") == "1"


def debug_print(*args, **kwargs):
    if debug_enabled():
        print(*args, **kwargs)


def enable_debug():
    os.environ["DEBUG_TREE_PLUS"] = "1"
    debug_enabled.cache_clear()
