# tests/path_to_test/class_method_type.py
from dataclasses import dataclass
from functools import lru_cache
from typing import TypeVar, Enum, List

from pydantic import BaseModel

import pytest

T = TypeVar("T")


def parse_py(contents: str) -> List[str]:
    return ["def parse_py(contents: str) -> List[str]:"]


class MyClass:
    @staticmethod
    def physical_element_aval(dtype) -> core.ShapedArray:
        return core.ShapedArray((), dtype.float_dtype)

    def my_method(self):
        pass

    @staticmethod
    def my_typed_method(obj: dict) -> int:
        pass

    def my_multiline_signature_method(
        self,
        alice: str = None,
        bob: int = None,
    ) -> tuple:
        pass


@lru_cache(maxsize=None)
def my_multiline_signature_function(
    tree: tuple = (),  # edge case
    plus: str = "+",
) -> tuple:
    pass


class LogLevelEnum(str, Enum):
    CRITICAL = "CRITICAL"
    GREETING = "GREETING"
    WARNING = "WARNING"
    ERROR = "ERROR"
    DEBUG = "DEBUG"
    INFO = "INFO"
    OFF = "OFF"


class Thingy(BaseModel):
    metric: float = 9000.99  # percent, repeating, of course


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


def ice[T](args: Iterable[T] = ()):
    pass


class list[T]:
    def __getitem__(self, index: int, /) -> T: ...

    @classmethod
    def from_code(cls, toolbox, code: bytes, score=None) -> "Thingy": ...

    @classmethod
    def from_str(cls, toolbox, string: str, score=None) -> "Thingy": ...


class Router(hk.Module): ...
