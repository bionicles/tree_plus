import subprocess

import rich


def test_programs_hello_tree_plus():
    command = ["python", "tree_plus_programs/hello_tree_plus.py"]
    result = subprocess.run(command, capture_output=True, text=True)
    rich.print(result.stdout)
    assert result.returncode == 0
    assert 'The "tree_plus_src" library v(1' in result.stdout
