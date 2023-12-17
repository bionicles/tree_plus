# tests/test_externals.py
import subprocess


# test on our specific directory
def test_tree_plus_specific_case():
    # Path to a directory with known token and line counts
    test_directory = "~/hax/aris/promo/promo-client"
    # Run the function
    result = subprocess.run(
        ["tree_plus", test_directory],
        capture_output=True,
        text=True,
    )
    print(dir(result))
    if result.args:
        print(result.args)
    if result.stderr:
        print(result.stderr)
    if result.stdout:
        print(result.stdout)
    assert result.returncode == 0

    # print(result.stdout)
    # assert 0
