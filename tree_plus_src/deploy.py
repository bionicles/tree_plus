# tree_plus_src/deploy.py
from typing import Optional, Tuple
import subprocess
import re
import os


def extract(path: Optional[str] = None) -> str:
    assert path is not None
    assert os.path.exists(path)
    try:
        with open(path, "r") as f:
            content = f.read()
        return content
    except Exception as e:
        print("extract Exception", e)
        raise e


def load(content: Optional[str] = None, path: Optional[str] = None):
    assert content is not None
    assert isinstance(content, str)
    assert path is not None
    # print(
    #     f"{type(path)=}",
    # )
    # print(
    #     f"{path=}",
    # )
    if not os.path.exists(path):
        # make directories
        try:
            # i think the issue is "./xyz.rs" has a dirname of "" which doesn't exist,
            # so we need to resolve the path and guard against the empty path
            if (parent_dir := os.path.dirname(path)) != "":
                os.makedirs(parent_dir, exist_ok=True)
        except Exception as e:  # and catch exceptions to be sure
            print(e)
            print("trying to continue...")
    try:
        with open(path, "w+") as f:
            f.write(content)
    except Exception as e:
        print("load Exception", e)
        raise e


def extract_version(source_path: Optional[str] = None) -> Tuple[int, int, int]:
    assert source_path is not None
    assert source_path.endswith(".py")
    assert os.path.exists(source_path)
    content = extract(source_path)
    if match := re.search('\n__version__ = "(.*)"', content):
        version = match.group(1).lstrip("\n")
        major, minor, patch = map(int, version.split("."))
        return major, minor, patch
    raise ValueError("Failed to match a version")


def increment_version(
    source_path: Optional[str] = None,
    sink_path: Optional[str] = None,
):
    assert source_path is not None
    assert sink_path is not None
    assert source_path.endswith(".py")
    assert os.path.exists(source_path)
    content = extract(source_path)
    content_lines = content.splitlines()
    print("increment_version content_lines", content_lines)
    new_content_lines = []
    for line in content_lines:
        if not line.startswith("__version__"):
            new_content_lines.append(line)
            continue
        match = re.search(r"\"(.*)\"", line)
        print("increment_version match:", match)
        if not match:
            raise ValueError("Failed to match a version")
        version = match.group(1)
        major, minor, patch = map(int, version.split("."))
        print(f"increment_version extracted {major=} {minor=} {patch=}")
        new_patch = patch + 1
        new_line = f'__version__ = "{major}.{minor}.{new_patch}"'
        print(f"increment_version new line:\n", new_line)
        new_content_lines.append(new_line)
    new_content = "\n".join(new_content_lines)
    load(new_content, sink_path)
    print("increment_version complete")


def run_command(command: Optional[str] = None, debug: bool = False):
    assert command is not None
    assert isinstance(debug, bool)
    if command.startswith("make"):
        command = command.replace("make", "make --no-print-directory")
    environ = None
    if not debug:
        environ = os.environ.copy()
        environ.update({"DEBUG_TREE_PLUS": "0"})
    return subprocess.check_output(command, shell=True, env=environ).decode()


def replace_readme_section(
    source_path: Optional[str] = None,
    sink_path: Optional[str] = None,
    marker: Optional[str] = None,
    command: Optional[str] = None,
):
    # paranoia
    assert source_path is not None
    assert source_path.endswith(".md")
    assert os.path.exists(source_path)
    assert sink_path is not None
    assert marker is not None
    assert marker.startswith("t")
    assert int(marker[1]) > 0
    assert int(marker[1]) < 6
    assert command is not None
    assert "make t" in command

    # define the section to replace
    start_marker = f"<!-- {marker}-start -->"
    end_marker = f"<!-- {marker}-end -->"
    pattern = re.compile(
        rf"{re.escape(start_marker)}.*?{re.escape(end_marker)}", re.DOTALL
    )

    # obtain new tree content to format into the readme
    new_tree_plus_output = run_command(command)
    # print("replace_section new_tree_plus_output:", new_tree_plus_output)
    assert "Entering directory" not in new_tree_plus_output, "make logs in output"
    assert "Leaving directory" not in new_tree_plus_output, "make logs in output"

    new_content = f"{start_marker}\n```sh\n{new_tree_plus_output}\n```\n{end_marker}"

    # get the current readme string
    with open(source_path, "r") as file:
        content = file.read()
    print(f"read from '{source_path}'")

    # update the readme string
    updated_content = re.sub(pattern, new_content, content)
    # print("replace_section updated_content", updated_content)

    # writing to file
    with open(sink_path, "w") as file:
        file.write(updated_content)
    print(f"wrote to '{sink_path}'")


def update_readme(source_path: Optional[str] = None, sink_path: Optional[str] = None):
    assert source_path is not None
    assert source_path.endswith(".md")
    assert os.path.exists(source_path)
    assert sink_path is not None
    assert sink_path.endswith("md")
    replace_readme_section(
        source_path=source_path, sink_path=sink_path, marker="t1", command="make t1"
    )
    print("update_readme handled t1")
    replace_readme_section(
        source_path=sink_path, sink_path=sink_path, marker="t2", command="make t2"
    )
    print("update_readme handled t2")
    replace_readme_section(
        source_path=sink_path, sink_path=sink_path, marker="t3", command="make t3"
    )
    print("update_readme handled t3")
    replace_readme_section(
        source_path=sink_path, sink_path=sink_path, marker="t4", command="make t4"
    )
    print("update_readme handled t4")
    replace_readme_section(
        source_path=sink_path, sink_path=sink_path, marker="t5", command="make t5"
    )
    print("update_readme handled t5")
    print("deploy::update_readme done")


MAIN_VERSION_PY_SOURCE_PATH = os.path.join("tree_plus_src", "version.py")
MAIN_README_SOURCE_PATH = "README.md"


def main():
    print("deploy::main begins")
    # if os.environ.get("RESET_TREE_PLUS_README") == "1":
    #     reset_readme(source_path="README.md", sink_path="README.md")
    # dry run: load new pyproject.toml into tests/version_increments
    main_version_py_sink_path = os.path.join(
        "tests", "version_increments", "dry_run_version.py"
    )
    # dry run: load new README.md into tests/readme_updates
    main_readme_sink_path = os.path.join("tests", "readme_updates", "dry_run_README.md")
    if os.environ.get("TREE_PLUS_DEPLOYMENT") == "GO":
        main_readme_sink_path = MAIN_README_SOURCE_PATH
        main_version_py_sink_path = MAIN_VERSION_PY_SOURCE_PATH
    else:
        # dry run: remove the sink files to keep tests honest
        if os.environ.get("TREE_PLUS_INCREMENT_VERSION") == "YES" and os.path.exists(
            main_version_py_sink_path
        ):
            os.remove(main_version_py_sink_path)
        if os.environ.get("TREE_PLUS_UPDATE_README") == "YES" and os.path.exists(
            main_readme_sink_path
        ):
            os.remove(main_readme_sink_path)
    # separate concerns since we need to increment, test, then update
    if os.environ.get("TREE_PLUS_INCREMENT_VERSION") == "YES":
        # 1. increment version first, because step 2 will show this version number
        increment_version(
            source_path=MAIN_VERSION_PY_SOURCE_PATH,
            sink_path=main_version_py_sink_path,
        )
    if os.environ.get("TREE_PLUS_UPDATE_README") == "YES":
        # 2. update README with new trees
        update_readme(
            source_path=MAIN_README_SOURCE_PATH, sink_path=main_readme_sink_path
        )
    print("deploy::main ends")


if __name__ == "__main__":
    main()
