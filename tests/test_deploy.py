import subprocess
import os

from rich import print as rich_print
import pytest

from tree_plus_src import deploy


def test_deploy_run_command():
    t1_output = deploy.run_command("make t1")
    rich_print(t1_output)
    # detect correct avoidance of debug
    assert "match=<re.Match" not in t1_output
    assert "ignore={" not in t1_output
    assert "tree_plus main received paths" not in t1_output
    # detect make command doesn't show up
    assert "make t1" not in t1_output
    # detect a tree is produced
    assert "📁 tree_plus" in t1_output
    # assert 0, "manual inspection"


def test_deploy_replace_section():
    # Path to the source and sink README files
    source_readme_name = "mini_README_source.md"
    source_readme_path = os.path.join("tests", "readme_updates", source_readme_name)
    sink_readme_name = "mini_README_sink.md"
    sink_readme_path = os.path.join("tests", "readme_updates", sink_readme_name)
    if os.path.exists(sink_readme_path):
        os.remove(sink_readme_path)
    initial = deploy.extract(source_readme_path)
    assert "# Mini Readme" in initial
    assert "<!-- t1-start -->\n```sh" in initial
    assert "```\n<!-- t1-end -->" in initial
    assert "this text should be removed" in initial
    assert "this text should remain" in initial

    deploy.replace_readme_section(
        source_path=source_readme_path,
        sink_path=sink_readme_path,
        marker="t1",
        command="make t1",
    )

    final = deploy.extract(sink_readme_path)
    assert "# Mini Readme" in final
    assert "<!-- t1-start -->\n```sh" in final
    assert "```\n<!-- t1-end -->" in final
    assert "this text should be removed" not in final
    assert "📁 tree_plus" in final
    assert "this text should remain" in final


def test_deploy_update_readme():
    # Path to the source and sink README files
    source_readme_name = "medium_README_source.md"
    source_readme_path = os.path.join("tests", "readme_updates", source_readme_name)
    sink_readme_name = "medium_README_sink.md"
    sink_readme_path = os.path.join("tests", "readme_updates", sink_readme_name)
    if os.path.exists(sink_readme_path):
        os.remove(sink_readme_path)
    initial = deploy.extract(source_readme_path)
    assert "# Medium Readme" in initial
    # t1
    assert "<!-- t1-start -->\n```sh" in initial
    assert "```\n<!-- t1-end -->" in initial
    assert "this 1st text should be removed" in initial
    assert "📁 tree_plus_src" not in initial
    assert "1st remaining text" in initial
    # t2
    assert "<!-- t2-start -->\n```sh" in initial
    assert "```\n<!-- t2-end -->" in initial
    assert "this 2nd text should be removed" in initial
    assert "📁 more_languages" not in initial
    assert "2nd remaining text" in initial
    # t3
    assert "<!-- t3-start -->\n```sh" in initial
    assert "```\n<!-- t3-end -->" in initial
    assert "this 3rd text should be removed" in initial
    assert "*.*s" not in initial
    assert "3rd remaining text" in initial
    # t4
    assert "<!-- t4-start -->\n```sh" in initial
    assert "```\n<!-- t4-end -->" in initial
    assert "this 4th text should be removed" in initial
    assert "group_todo" not in initial
    assert "4th remaining text" in initial

    deploy.update_readme(
        source_path=source_readme_path,
        sink_path=sink_readme_path,
    )

    final = deploy.extract(sink_readme_path)
    assert "# Medium Readme" in final
    # t1
    assert "<!-- t1-start -->\n```sh" in final
    assert "```\n<!-- t1-end -->" in final
    assert "this 1st text should be removed" not in final
    assert "📁 tree_plus_src" in final
    assert "1st remaining text" in final
    # t2
    assert "<!-- t2-start -->\n```sh" in final
    assert "```\n<!-- t2-end -->" in final
    assert "this 2nd text should be removed" not in final
    assert "📁 more_languages" in final
    assert "2nd remaining text" in final
    # t3
    assert "<!-- t3-start -->\n```sh" in final
    assert "```\n<!-- t3-end -->" in final
    assert "this 3rd text should be removed" not in final
    assert "*.*s" in final
    assert "3rd remaining text" in final
    # t4
    assert "<!-- t4-start -->\n```sh" in final
    assert "```\n<!-- t4-end -->" in final
    assert "this 4th text should be removed" not in final
    assert "group_todo" in final
    assert "4th remaining text" in final
    # t5
    assert "<!-- t5-start -->\n```sh" in final
    assert "```\n<!-- t5-end -->" in final
    assert "this 5th text should be removed" not in final
    assert "Wrap patterns in quotes:" in final
    # assert "within tests/more_languages" in final # not working b/c need to build new version
    assert "5th remaining text" in final


def test_deploy_increment_version():
    # Path to the source and sink toml files
    source_version_py_name = "test_source_version.py"
    source_version_py_path = os.path.join(
        "tests", "version_increments", source_version_py_name
    )
    assert os.path.exists(source_version_py_path), "no version.py to test"
    og_major, og_minor, og_patch = deploy.extract_version(source_version_py_path)
    sink_version_py_name = "test_sink_version.py"
    sink_version_py_path = os.path.join(
        "tests", "version_increments", sink_version_py_name
    )
    if os.path.exists(sink_version_py_path):
        os.remove(sink_version_py_path)
    deploy.increment_version(
        source_path=source_version_py_path, sink_path=sink_version_py_path
    )
    assert os.path.exists(sink_version_py_path)
    source_major, source_minor, source_patch = deploy.extract_version(
        source_version_py_path
    )
    assert source_major == og_major, "mutated source major version"
    assert source_minor == og_minor, "mutated source minor version"
    assert source_patch == og_patch, "mutated source patch version"
    sink_major, sink_minor, sink_patch = deploy.extract_version(sink_version_py_path)
    assert sink_major == og_major, "mutated major version"
    assert sink_minor == og_minor, "mutated minor version"
    assert sink_patch == (og_patch + 1), "failed to increment patch version"


def run_deploy_script_with_env(env_vars):
    env = os.environ.copy()
    env.update(env_vars)
    subprocess.check_output(["python", "tree_plus_src/deploy.py"], env=env)


MAIN_VERSION_PY_SINK_PATH = os.path.join(
    "tests", "version_increments", "dry_run_version.py"
)
MAIN_README_SINK_PATH = os.path.join("tests", "readme_updates", "dry_run_README.md")

# rename files so they stick around to inspect
DRY_RUN_VERSION_PY_SINK_RENAMED_PATH = os.path.join(
    "tests", "version_increments", "renamed_dry_run_version.py"
)
DRY_RUN_README_SINK_RENAMED_PATH = os.path.join(
    "tests", "readme_updates", "renamed_dry_run_README.md"
)


def cleanup():
    if os.path.exists(MAIN_VERSION_PY_SINK_PATH):
        os.remove(MAIN_VERSION_PY_SINK_PATH)
    if os.path.exists(MAIN_README_SINK_PATH):
        os.remove(MAIN_README_SINK_PATH)


@pytest.mark.order(1)
def test_deploy_main_increment_version():
    cleanup()
    run_deploy_script_with_env({"TREE_PLUS_INCREMENT_VERSION": "YES"})

    assert os.path.exists(MAIN_VERSION_PY_SINK_PATH)
    assert not os.path.exists(MAIN_README_SINK_PATH)

    og_major, og_minor, og_patch = deploy.extract_version(
        source_path=deploy.MAIN_VERSION_PY_SOURCE_PATH
    )
    major, minor, patch = deploy.extract_version(
        source_path=str(MAIN_VERSION_PY_SINK_PATH)
    )

    assert major == og_major
    assert minor == og_minor
    assert patch == (og_patch + 1)

    os.rename(MAIN_VERSION_PY_SINK_PATH, DRY_RUN_VERSION_PY_SINK_RENAMED_PATH)


@pytest.mark.order(2)
def test_deploy_main_update_readme():
    cleanup()

    run_deploy_script_with_env({"TREE_PLUS_UPDATE_README": "YES"})

    assert not os.path.exists(MAIN_VERSION_PY_SINK_PATH)
    assert os.path.exists(MAIN_README_SINK_PATH)

    readme_content = deploy.extract(MAIN_README_SINK_PATH)

    assert "util enhanced with tokens, lines, and components" in readme_content
    assert "both also work" in readme_content
    # assert "within tests/more_languages" in readme_content

    os.rename(MAIN_README_SINK_PATH, DRY_RUN_README_SINK_RENAMED_PATH)
