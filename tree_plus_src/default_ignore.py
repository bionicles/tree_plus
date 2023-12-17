from typing import Optional, Union, Set, Tuple

DEFAULT_IGNORE = {
    "detritus",
    "__init__.py",
    "__pycache__",
    ".git",
    "*.egg-info",
    ".pytest_cache",
    "node_modules",
    ".hypothesis",
    "babel-webpack",
    ".angular",
    ".vscode",
    "dist",
    "build",
    "venv",
    "env",
    ".idea",
    ".DS_Store",
    ".ipynb_checkpoints",
    ".env*",
    "*.log",
    ".cache",
    "*.so",
    "*.dll",
    "*.dylib",
    "*.pyc",
    "*.swp",
    "*.swo",
    "*~",
    "._*",
    ".coverage",
    "*.png",
    "*.jpg",
    "*.jpeg",
    "*.gif",
    "*.ico",
    "*.woff",
    "*.eot",
    "*.ttf",
    "*.zip",
    "*.mp4",
    "*.mp3",
    "*.avi",
    "*.wav",
    "*.mov",
    "*.flv",
    "*.wmv",
    "*_memmap",
    "*.pdf",
    "*.jar",
    "*.pack",
    "*.odg",
    "Anaconda3*.sh",
    "Miniconda3*.sh",
}

IgnoreInput = Optional[Union[str, Set[str], Tuple[str]]]
Ignore = Set[str]


def make_ignore(ignore: IgnoreInput) -> Ignore:
    if ignore is None:
        ignore = set()
    elif isinstance(ignore, str):
        ignore = set(ignore.split(","))
    elif isinstance(ignore, tuple):
        ignore = set(ignore)
    elif isinstance(ignore, set):
        pass
    else:
        print(f"{ignore=} {type(ignore)=}")
        raise TypeError("tree_plus ignore arg must be a string, set or None")
    ignore |= DEFAULT_IGNORE
    return ignore


def is_binary_string(bytes):
    textchars = bytearray({7, 8, 9, 10, 12, 13, 27} | set(range(0x20, 0x100)) - {0x7F})
    return bool(bytes.translate(None, textchars))


def is_binary(file_path: str) -> bool:
    """
    Check if a file is binary or not.
    """
    try:
        # read the file in binary mode
        with open(file_path, "rb") as f:
            return is_binary_string(f.read(1024))
    except Exception as e:
        print(f"Error opening file {file_path}: {e}")
        return False
