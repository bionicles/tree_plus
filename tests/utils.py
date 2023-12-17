from difflib import ndiff
from rich import print


def pretty_print_diff(*, result, expected):
    diff = ndiff(result, expected)
    diff_list = list(diff)
    for line in diff_list:
        if line.startswith("-"):
            print("[red]" + line + "[/red]")  # Deleted items in red
        elif line.startswith("+"):
            print("[green]" + line + "[/green]")  # Added items in green
        else:
            print(line)  # No change
