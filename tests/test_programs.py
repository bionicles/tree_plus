import subprocess
import os

import rich

import tree_plus_src as tp


def test_programs_hello_tree_plus():
    command = ["python", "tree_plus_programs/hello_tree_plus.py"]
    result = subprocess.run(command, capture_output=True, text=True)
    rich.print(result.stdout)
    assert result.returncode == 0
    assert 'The "tree_plus_src" library' in result.stdout


STUB_TESTS_EXPECTATION = """# TODO: fix this path
# from tree_plus_programs.stub_tests import stub_tests
# TODO: fill in these stubs:
def test_remove_decorators():
	assert 0

def test_make_import_path():
	assert 0

def test_stub_tests():
	assert 0

def test_main():
	assert 0

def test_class_vehicle():
	assert 0

def test_class_car():
	assert 0
"""


def test_programs_stub_tests():
    input_path = "tree_plus_programs/stub_tests.py"
    output_path = "tree_plus_programs/test_stub_tests.py"
    if not os.path.exists(output_path):
        tp.load(
            content="# not empty",
            path=output_path,
        )
    if not os.path.exists(output_path):
        rich.print("no flaky tests allowed")
        return
    command = [
        "python",
        input_path,
        "-i",
        input_path,
    ]
    result = subprocess.run(
        command,
        capture_output=True,
        text=True,
    )
    rich.print("stderr:")
    rich.print(result.stderr)
    rich.print("stdout:")
    rich.print(result.stdout)
    # whoops, it didn't work!
    assert result.returncode != 0
    # whoops, we "forgot" -r
    command.append("-r")
    result = subprocess.run(
        command,
        capture_output=True,
        text=True,
    )
    rich.print("stderr:")
    rich.print(result.stderr)
    rich.print("stdout:")
    rich.print(result.stdout)
    # it worked!
    assert result.returncode == 0
    assert os.path.exists(output_path)
    assert "tests: List =" in result.stdout
    content = tp.extract(
        path=output_path,
    )
    rich.print(f"content from {output_path}:")
    rich.print(content)
    assert content == STUB_TESTS_EXPECTATION
