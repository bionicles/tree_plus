# src/count_tokens.py
import tiktoken

encoder = tiktoken.encoding_for_model("gpt-4")


def count_tokens(file_path):
    """
    Count the number of lines and OpenAI tokens in a file.
    """
    with open(file_path, "r") as f:
        contents = f.read()
        n_tokens = len(encoder.encode(contents))
        n_lines = len(contents.splitlines())
        return n_tokens, n_lines
