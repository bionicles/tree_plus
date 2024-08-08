from dataclasses import dataclass
from typing import runtime_checkable, Protocol
from enum import Enum


@runtime_checkable
class DataClass(Protocol):
    __dataclass_fields__: dict


class MyInteger(Enum):
    ONE = 1
    TWO = 2
    THREE = 42


class MyString(Enum):
    AAA1 = "aaa"
    BB_B = """edge
case"""


@dataclass(frozen=True, slots=True, kw_only=True)
class Tool:
    """
    name: str
    description: str
    input_model: DataClass
    output_model: DataClass

    def execute(self, *args, **kwargs): ...
    """

    name: str
    description: str
    input_model: DataClass
    output_model: DataClass

    def execute(self, *args, **kwargs): ...

    @property
    def edge_case(self) -> str:
        if not self.name and not self.description:
            return ""
        deeper_edge_case: int = 1
        return f"""Location: {self.name}

{self.description}
"""

    def should_still_see_me(self, x: bool = True) -> "Tool":
        print(f"{x=}")
        return self


@dataclass
class MyInput[T]:
    name: str
    rank: MyInteger
    serial_n: int


@dataclass
class Thingy:
    is_edge_case: bool = True


@dataclass
class MyOutput:
    orders: str


class MyTools(Enum):
    TOOL_A = Tool(
        name="complicated",
        description="edge case!",
        input_model=MyInput[Thingy],
        output_model=MyOutput,
    )
    TOOL_B = Tool(
        name="""super
complicated
""",
        description="edge case!",
        input_model=MyInput,
        output_model=MyOutput,
    )


@final
class dtype(Generic[_DTypeScalar_co]):
    names: None | tuple[builtins.str, ...]
