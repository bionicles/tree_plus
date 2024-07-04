from dataclasses import dataclass
from typing import Protocol, Any


type DataClass = Any  # not needed here


@dataclass(frozen=True, slots=True, kw_only=True)
class Tool(Protocol):
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
        return f"""Location: {self.config.name}

{self.config.description}
"""

    def should_still_see_me(self, x: bool = True) -> "Tool":
        pass
