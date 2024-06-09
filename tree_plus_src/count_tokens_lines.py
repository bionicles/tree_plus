# tree_plus_src/count_tokens_lines.py
from dataclasses import dataclass
from typing import Optional, Union
from pathlib import Path
import os

import tiktoken
from tree_plus_src.debug import debug_print
from tree_plus_src.parse_file import read_file

encoder = tiktoken.encoding_for_model("gpt-4")


# TODO: show off how well we parse_todo!
@dataclass(frozen=True)
class TokenLineCount:
    n_tokens: int = 0
    n_lines: int = 0


# Ignore binary files like images, executables, and databases
extensions_not_to_count = {
    ".stl",
    ".obj",
    ".gcode",
    ".3mf",
    ".amf",
    ".ply",
    ".f3d",
    ".iges",
    ".igs",
    "step",
    ".stp",
    ".vrml",
    ".wrl",
    ".7z",
    ".aac",
    ".ai",
    ".avi",
    ".bak",
    ".bin",
    ".bz2",
    ".chk",
    ".class",
    ".csv",
    ".d",
    ".dat",
    ".db",
    ".dll",
    ".doc",
    ".docx",
    ".dylib",
    ".ear",
    ".eps",
    ".exe",
    ".flac",
    ".flv",
    ".framework",
    ".gdoc",
    ".gif",
    ".gsheet",
    ".gz",
    ".img",
    ".ipa",
    ".iso",
    ".jar",
    ".jpg",
    ".jpeg",
    ".lock",
    ".log",
    ".mov",
    ".mp3",
    ".mp4",
    ".nib",
    ".node",
    ".o",
    ".obj",
    ".odg",
    ".pack",
    ".pdf",
    ".png",
    ".ppt",
    ".pptx",
    ".psd",
    ".pyc",
    ".pyo",
    ".pyd",
    ".rar",
    ".rlib",
    ".rmeta",
    ".so",
    ".sqlite",
    ".storyboardc",
    ".swp",
    ".tar",
    ".tml",
    ".wav",
    ".war",
    ".wmv",
    ".xcarchive",
    ".xlsx",
    ".xlsx",
    ".zip",
    ".zst",
}


def count_tokens_lines(file_path: Union[str, Path]) -> Optional[TokenLineCount]:
    """
    Count the number of lines and OpenAI tokens in a file.
    """
    if isinstance(file_path, Path):
        file_path = str(file_path)
    debug_print(f"count_tokens_lines {file_path=}")
    _prefix, file_extension = os.path.splitext(file_path)
    debug_print(f"count_tokens_lines {file_extension=}")
    # counting CSV is too slow
    # if file_extension == ".csv":
    #     with open(file_path, "rb") as csv_file:
    #         n_lines = sum(1 for _ in csv_file)
    #     return TokenLineCount(n_tokens=None, n_lines=n_lines)
    if os.path.isdir(file_path) or file_extension in extensions_not_to_count:
        debug_print(f"count_tokens_lines not counting {file_path=}")
        return None
    debug_print(f"count_tokens_lines counting {file_path=}")

    contents = read_file(file_path)
    # if not contents.strip():
    #     return TokenLineCount(n_tokens=0, n_lines=0)
    # n_tokens = len(encoder.encode(contents, disallowed_special=()))
    # n_lines = len(contents.splitlines())
    count = count_tokens_lines_from_contents(contents)
    debug_print(f"count_tokens_lines {count=}")
    return count


from rich.markdown import Markdown


def count_tokens_lines_from_contents(contents: Union[str, Markdown]) -> TokenLineCount:
    "Count OpenAI tokens and lines in a string."
    if not isinstance(contents, str) or not contents.strip():
        return TokenLineCount(n_tokens=0, n_lines=0)
    n_tokens = len(encoder.encode(contents, disallowed_special=()))
    n_lines = len(contents.splitlines())
    return TokenLineCount(n_tokens=n_tokens, n_lines=n_lines)


def add_tokens_lines(
    lhs_count: TokenLineCount, rhs_count: TokenLineCount
) -> TokenLineCount:
    total_tokens = lhs_count.n_tokens + rhs_count.n_tokens
    total_lines = lhs_count.n_lines + rhs_count.n_lines
    new_count = TokenLineCount(total_tokens, total_lines)
    debug_print(f"{lhs_count} + {rhs_count} = {new_count}")
    return new_count
