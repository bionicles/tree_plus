# tree_plus_src/count_tokens_lines.py
from dataclasses import dataclass
import tiktoken

from tree_plus_src.traverse_directory import traverse_directory
from tree_plus_src.debug import debug_print


encoder = tiktoken.encoding_for_model("gpt-4")


# TODO: show off how well we parse_todo!
@dataclass
class TokenLineCount:
    n_tokens: int = 0
    n_lines: int = 0


def add_tokens_lines(
    lhs_count: TokenLineCount, rhs_count: TokenLineCount
) -> TokenLineCount:
    total_tokens = lhs_count.n_tokens + rhs_count.n_tokens
    total_lines = lhs_count.n_lines + rhs_count.n_lines
    new_count = TokenLineCount(total_tokens, total_lines)
    debug_print(f"{lhs_count} + {rhs_count} = {new_count}")
    return new_count


def count_tokens_lines(file_path: str) -> TokenLineCount:
    """
    Count the number of lines and OpenAI tokens in a file.
    """
    # Ignore binary files like images, executables, and databases
    ignored_extensions = {
        ".db",
        ".png",
        ".jpg",
        ".exe",
        ".dll",
        ".so",
        ".o",
        ".pdf",
        ".pack",
        ".jar",
        ".odg",
        ".sqlite",
    }
    if any(file_path.endswith(ext) for ext in ignored_extensions):
        # if any(file_path.suffix == ext for ext in ignored_extensions):
        return TokenLineCount(n_tokens=0, n_lines=0)

    try:
        with open(file_path, "r", encoding="utf-8") as f:
            contents = f.read()
            # Handle empty files separately
            if not contents.strip():
                return TokenLineCount(n_tokens=0, n_lines=0)
            n_tokens = len(encoder.encode(contents, disallowed_special=()))
            n_lines = len(contents.splitlines())
            return TokenLineCount(n_tokens=n_tokens, n_lines=n_lines)
    except Exception as e:
        print(f"count_tokens_lines Error reading {file_path}: {e}")
        return TokenLineCount(n_tokens=0, n_lines=0)


def count_directory_tokens_lines(directory_path: str) -> TokenLineCount:
    """
    Traverse a directory, count lines and OpenAI tokens in each file, sum the results.
    """
    total_count = TokenLineCount(n_tokens=0, n_lines=0)

    # Get all file paths in the directory
    file_paths = traverse_directory(directory_path)

    for file_path in file_paths:
        # Count tokens and lines in each file and add them to the total count
        file_count = count_tokens_lines(file_path)
        print(f"{file_path}: {file_count}")  # Print counts for each file
        total_count.n_tokens += file_count.n_tokens
        total_count.n_lines += file_count.n_lines

    return total_count
