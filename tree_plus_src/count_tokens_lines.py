# tree_plus_src/count_tokens_lines.py
from dataclasses import dataclass
from pathlib import Path
from typing import Optional, Union
from enum import Enum

import subprocess
import os

from rich.markdown import Markdown

from tree_plus_src.debug import debug_print
from tree_plus_src.parse_file import read_file

tiktoken = None
encoder = None


# TODO: show off how well we parse_todo!
@dataclass(frozen=True)  # , slots=True) # slots added py310
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


class TokenizerName(Enum):
    WC = "wc"
    GPT4O = "gpt-4o"
    GPT4 = "gpt-4"


def count_tokens_lines(
    file_path: Union[str, Path],
    *,
    tokenizer_name: TokenizerName = TokenizerName.WC,
) -> Optional[TokenLineCount]:
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

    # TOO SOON! py38 failed on this
    # match tokenizer_name:
    #     case TokenizerName.GPT4O | TokenizerName.GPT4:
    #         contents = read_file(file_path)
    #         count = count_openai_tokens_lines_from_contents(
    #             contents,
    #             tokenizer_name=tokenizer_name,
    #         )
    #     case TokenizerName.WC:
    #         count = count_wc_tokens_lines_from_path(file_path)

    if tokenizer_name is TokenizerName.GPT4O or tokenizer_name is TokenizerName.GPT4:
        contents = read_file(file_path)
        count = count_openai_tokens_lines_from_contents(
            contents,
            tokenizer_name=tokenizer_name,
        )
    elif tokenizer_name == TokenizerName.WC:
        count = count_wc_tokens_lines_from_path(file_path)
    else:
        raise ValueError(f"unsupported {tokenizer_name=}")
    debug_print(f"count_tokens_lines {count=}")
    return count


def count_openai_tokens_lines_from_contents(
    contents: Union[str, "Markdown"],
    *,
    tokenizer_name: TokenizerName = TokenizerName.GPT4,
) -> TokenLineCount:
    "Count OpenAI tokens and lines in a string."
    assert (
        tokenizer_name is not TokenizerName.WC
    ), "can't use wc as a tiktoken tokenizer"
    # lazy import tiktoken and make the encoder only if needed
    global tiktoken
    global encoder
    if encoder is None:
        import tiktoken

        encoder = tiktoken.encoding_for_model(tokenizer_name.value)
    if not isinstance(contents, str) or not contents.strip():
        return TokenLineCount(n_tokens=0, n_lines=0)
    n_tokens = len(encoder.encode(contents, disallowed_special=()))
    n_lines = len(contents.splitlines())
    return TokenLineCount(n_tokens=n_tokens, n_lines=n_lines)


# source: @StasBekman on X
def count_wc_tokens_lines_from_path(file_path: str) -> TokenLineCount:
    """Count OpenAI tokens and lines in a file."""
    # Use the wc command to count the number of lines and characters
    result = subprocess.run(["wc", "-ml", file_path], stdout=subprocess.PIPE, text=True)
    output = result.stdout.strip()
    n_lines, n_characters = map(int, output.split()[:2])
    # 1 token ~= 4 chars in English
    n_tokens = n_characters // 4
    return TokenLineCount(n_tokens=n_tokens, n_lines=n_lines)


def add_tokens_lines(
    lhs_count: TokenLineCount, rhs_count: TokenLineCount
) -> TokenLineCount:
    total_tokens = lhs_count.n_tokens + rhs_count.n_tokens
    total_lines = lhs_count.n_lines + rhs_count.n_lines
    new_count = TokenLineCount(total_tokens, total_lines)
    debug_print(f"{lhs_count} + {rhs_count} = {new_count}")
    return new_count
