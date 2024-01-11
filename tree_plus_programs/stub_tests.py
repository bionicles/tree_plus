from functools import lru_cache
from pathlib import Path
from typing import Optional, Tuple, List

from rich.console import Console
from rich.theme import Theme
import click

import tree_plus_src as tp

console = Console(
    style="#FFB000 on black",
    # this avoids a bug with '::' in code
    theme=Theme({"repr.ipv6": "default"}),
    width=128,
    soft_wrap=False,
)


# I put a decorator on the remove_decorators function so it can test itself
@lru_cache
def remove_decorators(component: str) -> str:
    console.print("remove_decorators")
    component_lines = component.splitlines()
    filtered_lines = [line for line in component_lines if not line.startswith("@")]
    return "\n".join(filtered_lines)


def make_import_path(path: Path) -> str:
    console.print("make_import_path")
    no_suffix = path.with_suffix("")
    console.print("make_import_path removed suffix", no_suffix)
    periods = str(no_suffix).replace("/", ".").replace("\\", ".")
    console.print("make_import_path replaced slashes", periods)
    return periods


def stub_tests(
    input_path: str,
    output_path: Optional[str],
    rewrite_ok: bool,
) -> Tuple[List[str], str, bool]:
    if not input_path.endswith(".py"):
        raise ValueError("only python for now!")

    category = tp.engine.categorize(input_path)
    if category is not tp.engine.Category.FILE:
        raise ValueError("only files for now!")

    input_path = Path(input_path)

    if output_path:
        output_path = Path(output_path)
    else:
        new_stem = "test_" + input_path.stem
        output_path = Path(input_path).with_stem(new_stem)
        console.print(f"gonna put this test at {output_path=}")

    if output_path.exists() and not rewrite_ok:
        raise ValueError(
            "not gonna clobber existing tests! try a name that doesn't exist"
        )

    try:
        components = tp.parse_file(file_path=input_path)
    except Exception as e:
        console.log(f"Exception parsing file: {e}")
        raise e

    if not components:
        console.log(f"No components found at {input_path}")

    tests = []
    for component in components:
        console.print("component", component)
        component = component.lstrip()
        console.print("stripped", component)
        if component.startswith("@"):
            component = remove_decorators(component)
        if component.startswith("def"):
            console.print("looks like a function")
            first_open_paren_index = component.index("(")
            console.print(f"function signature start={first_open_paren_index}")
            component = component[:first_open_paren_index]
            console.print("removed signature", component)
            component = component.replace("def ", "def test_")
            console.print("added 'test_'", component)
            component = component + "():\n\tassert 0\n"
            console.print("added signature and assertion", component)
            tests.append(component)
        elif component.startswith("class"):
            console.print("looks like a class")
            first_open_paren_index = None
            try:
                first_open_paren_index = component.index("(")
                component = component[:first_open_paren_index]
                console.print("removed signature", component)
            except Exception as e:
                console.print(f"Exception finding first open paren {e}")
            component = component.replace(" ", "_")
            console.print("replaced whitespace", component)
            component = component.lower()
            console.print("lowercased", component)
            component = "def test_" + component
            console.print("added 'def test_'", component)
            component = component + "():\n\tassert 0\n"
            console.print("added signature and assertion", component)
            tests.append(component)
        else:
            console.print("dunno how to test that, moving right along...")
            continue

        console.print("tests: List[str] =")
        console.print_json(data=tests)

        target_module = Path(input_path).stem
        import_path = make_import_path(input_path)

        test_imports = [
            "# TODO: fix this path",
            f"# from {import_path} import {target_module}",
            "# TODO: fill in these stubs:",
        ]

    tests = test_imports + tests

    test_file_content = "\n".join(tests)

    console.print(f"load the new test(s) into {output_path=}")
    tp.deploy.load(content=test_file_content, path=output_path)


@click.command()
@click.option(
    "--input-path",
    "-i",
    help="path to the input file (REQUIRED)",
    type=str,
)
@click.option(
    "--output-path",
    "-o",
    help="path to the output file (default will be 'test_{input}')",
    default=None,
    type=Optional[str],
)
@click.option(
    "--rewrite-ok",
    "-r",
    help="should we overwrite the test file if it exists? y/n (default n)",
    is_flag=True,
    default=False,
    type=bool,
)
def main(
    input_path: str,
    output_path: Optional[str],
    rewrite_ok: bool,
):
    "stub pytests for input_path at output_path"
    stub_tests(
        input_path=input_path,
        output_path=output_path,
        rewrite_ok=rewrite_ok,
    )


class Vehicle:
    pass


class Car(Vehicle):
    pass


if __name__ == "__main__":
    main()
