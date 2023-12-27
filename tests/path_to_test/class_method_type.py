# tests/path_to_test/class_method_type.py
from dataclasses import dataclass
from functools import lru_cache
from typing import TypeVar, Enum, List

from pydantic import BaseModel

import pytest

T = TypeVar("T")


class MyClass:
    def my_method(self):
        pass

    def my_typed_method(self, obj: dict) -> int:
        pass

    def my_multiline_signature_method(
        self,
        alice: str = None,
        bob: int = None,
    ) -> tuple:
        pass


@lru_cache(maxsize=None)
def my_multiline_signature_function(
    tree: tuple = (),
    plus: str = "+",
) -> tuple:
    pass


class LogLevelEnum(str, Enum):
    CRITICAL = "CRITICAL"
    GREEDBOT = "GREEDBOT"
    WARNING = "WARNING"
    ERROR = "ERROR"
    DEBUG = "DEBUG"
    INFO = "INFO"
    OFF = "OFF"


class Algo(BaseModel):
    fitness: float = 9000.99  # percent, repeating, of course


@dataclass
class TestDataclass:
    tree: str = "plus"


A = TypeVar("A", str, bytes)


@pytest.mark.parameterize(
    "file,expected",
    [
        ("tests/path_to_test/file.py", ["def unseen()"]),
        (
            "tests/path_to_test/class_method_type.py",
            [
                "    def also_unseen(num: int) -> float",
                "    class TreePlus(BaseModel, UnSeen)",
                "@edge_cases()",
                "def gotta_test_them_all()",
            ],
        ),
    ],
)
def omega_yikes(file: str, expected: List[str]) -> bool:
    assert "does 'tree_plus' show multiline decorators or strings?"
    return False
