# file_parsing.py

import os


def parse_file(file_path):
    """
    Parse a file and return a list of its major components.
    Only supports Python, JavaScript, and Markdown files.
    """
    file_extension = os.path.splitext(file_path)[-1].lower()

    if file_extension not in [".py", ".js", ".md"]:
        return []

    # Add code here to parse the file and return its major components
    # This is a placeholder implementation
    return ["Placeholder"]
