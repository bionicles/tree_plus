# Tree Plus

**A `tree` util enhanced with tokens, lines, and components. Why? To see the big picture!**

[![License](https://img.shields.io/badge/license-MIT%2FApache--2.0-brightgreen)](https://choosealicense.com/licenses/)
[![Docs](https://img.shields.io/badge/docs-README-blue)](https://github.com/bionicles/tree_plus/blob/main/README.md)
[![Linux & MacOS](https://github.com/bionicles/tree_plus/actions/workflows/unix.yml/badge.svg)](https://github.com/bionicles/tree_plus/actions/workflows/unix.yml)
[![Microsoft](https://github.com/bionicles/tree_plus/actions/workflows/microsoft.yml/badge.svg)](https://github.com/bionicles/tree_plus/actions/workflows/microsoft.yml)

**Supported Python Versions:**

![Python 3.8](https://img.shields.io/badge/Python-3.8-blue)
![Python 3.9](https://img.shields.io/badge/Python-3.9-blue)
![Python 3.10](https://img.shields.io/badge/Python-3.10-blue)
![Python 3.11](https://img.shields.io/badge/Python-3.11-blue)
![Python 3.12](https://img.shields.io/badge/Python-3.12-blue)

**Supported Operating Systems:**

![Ubuntu](https://img.shields.io/badge/Supports-Ubuntu-orange)
![MacOS](https://img.shields.io/badge/Supports-MacOS-orange)
![Windows](https://img.shields.io/badge/Supports-Windows-orange)

**Support Free, Open-Source Software:**

[![Support Tree Plus](https://img.shields.io/badge/Support%20Tree%20Plus-8A2BE2)](https://www.buymeacoffee.com/bionicles)


`pip install -U tree_plus`

## Example Output:
- [ ] Demonstrate Parsed Checkboxes
<!-- t1-start -->
```sh
tree_plus -i tests
paths=('.',)
📁 tree_plus (57964 tokens, 5753 lines)
┣━━ 📁 .github (107 tokens, 11 lines)
┃   ┣━━ 📁 workflows (1050 tokens, 128 lines)
┃   ┃   ┣━━ 📄 microsoft.yml (323 tokens, 40 lines)
┃   ┃   ┃   ┣━━ Microsoft
┃   ┃   ┃   ┣━━   job: build
┃   ┃   ┃   ┣━━     - Set up Python ${{ matrix.python-version }}
┃   ┃   ┃   ┣━━     - Install tree_plus
┃   ┃   ┃   ┣━━     - Create .env file
┃   ┃   ┃   ┣━━     - Set PYTHONUTF8 for Windows
┃   ┃   ┃   ┣━━     - Run generic tests
┃   ┃   ┃   ┗━━     - Run specific test
┃   ┃   ┗━━ 📄 unix.yml (727 tokens, 88 lines)
┃   ┃       ┣━━ Linux & MacOS
┃   ┃       ┣━━   job: test
┃   ┃       ┣━━     - Set up Python ${{ matrix.python-version }}
┃   ┃       ┣━━     - Install tree_plus
┃   ┃       ┣━━     - Create .env file
┃   ┃       ┣━━     - Run generic tests
┃   ┃       ┣━━     - Run specific test
┃   ┃       ┣━━   job: deploy
┃   ┃       ┣━━     - Set up Python
┃   ┃       ┣━━     - Install pypa/build
┃   ┃       ┣━━     - Increment Version
┃   ┃       ┣━━     - Build
┃   ┃       ┣━━     - Install
┃   ┃       ┣━━     - Run generic tests
┃   ┃       ┣━━     - Run specific test
┃   ┃       ┣━━     - Update README
┃   ┃       ┣━━     - Build Again
┃   ┃       ┣━━     - Commit Updates
┃   ┃       ┗━━     - Publish to PyPI
┃   ┗━━ 📄 dependabot.yml (107 tokens, 11 lines)
┃       ┗━━ Unsupported YAML Category
┣━━ 📁 tree_plus_src (22324 tokens, 2706 lines)
┃   ┣━━ 📁 scripts (3515 tokens, 450 lines)
┃   ┃   ┣━━ 📄 alias_tree_plus.sh (277 tokens, 31 lines)
┃   ┃   ┃   ┣━━ add_alias()
┃   ┃   ┃   ┗━━ create_conda_env()
┃   ┃   ┗━━ 📄 Microsoft.PowerShell_profile.ps1 (3238 tokens, 419 lines)
┃   ┃       ┣━━ function Log($message)
┃   ┃       ┗━━ function Show-Error($err)
┃   ┣━━ 📄 count_tokens_lines.py (951 tokens, 159 lines)
┃   ┃   ┣━━ TODO (Line 14): show off how well we parse_todo!
┃   ┃   ┣━━ @dataclass(frozen=True)
┃   ┃   ┃   class TokenLineCount
┃   ┃   ┣━━ @lru_cache
┃   ┃   ┃   def add_tokens_lines(
┃   ┃   ┃       lhs_count: TokenLineCount, rhs_count: TokenLineCount
┃   ┃   ┃   ) -> TokenLineCount
┃   ┃   ┣━━ def count_tokens_lines(file_path: str) -> TokenLineCount
┃   ┃   ┗━━ def count_directory_tokens_lines(directory_path: str) -> TokenLineCount
┃   ┣━━ 📄 debug.py (87 tokens, 20 lines)
┃   ┃   ┣━━ def disable_debug()
┃   ┃   ┣━━ def debug_enabled()
┃   ┃   ┣━━ def debug_print(*args, **kwargs)
┃   ┃   ┗━━ def enable_debug()
┃   ┣━━ 📄 deploy.py (2093 tokens, 240 lines)
┃   ┃   ┣━━ TODO (Line 167): test this reset readme command so we can clean out the code blocks
┃   ┃   ┣━━ def extract(path: str = None) -> str
┃   ┃   ┣━━ def load(content: str = None, path: str = None)
┃   ┃   ┣━━ def extract_version(source_path: str = None) -> Tuple[int, int, int]
┃   ┃   ┣━━ def increment_version(
┃   ┃   ┃       source_path: str = None,
┃   ┃   ┃       sink_path: str = None,
┃   ┃   ┃   )
┃   ┃   ┣━━ def run_command(command: str = None, debug: bool = False)
┃   ┃   ┣━━ def replace_readme_section(
┃   ┃   ┃       source_path: str = None,
┃   ┃   ┃       sink_path: str = None,
┃   ┃   ┃       marker: str = None,
┃   ┃   ┃       command: str = None,
┃   ┃   ┃   )
┃   ┃   ┣━━ def update_readme(source_path: str = None, sink_path: str = None)
┃   ┃   ┗━━ def main()
┃   ┣━━ 📄 ignore.py (1483 tokens, 227 lines)
┃   ┃   ┣━━ @lru_cache
┃   ┃   ┃   def make_ignore(ignore: IgnoreInput) -> Ignore
┃   ┃   ┣━━ @lru_cache
┃   ┃   ┃   def make_globs(globs: IgnoreInput) -> FrozenSet
┃   ┃   ┣━━ @lru_cache()
┃   ┃   ┃   def is_binary_string(data: bytes) -> bool
┃   ┃   ┣━━ @lru_cache()
┃   ┃   ┃   def is_binary(file_path: str) -> bool
┃   ┃   ┗━━ @lru_cache(maxsize=None)
┃   ┃       def should_ignore(path: str, ignore: Ignore, globs: Optional[Ignore] = None) -> bool
┃   ┣━━ 📄 parse_file.py (17263 tokens, 2000 lines)
┃   ┃   ┣━━ def extract_groups(match: re.Match) -> dict
┃   ┃   ┣━━ def parse_file(file_path: str) -> List[str]
┃   ┃   ┣━━ def parse_fsharp(contents: str) -> List[str]
┃   ┃   ┣━━ def parse_tcl(contents: str) -> List[str]
┃   ┃   ┣━━ def parse_erl(contents: str) -> List[str]
┃   ┃   ┣━━ def parse_rs(contents: str) -> List[str]
┃   ┃   ┣━━ def parse_csv(contents: str, max_leaves=11) -> List[str]
┃   ┃   ┣━━ def parse_mathematica(contents: str) -> List[str]
┃   ┃   ┣━━ def parse_r(contents: str) -> List[str]
┃   ┃   ┣━━ def parse_zig(contents: str) -> List[str]
┃   ┃   ┣━━ def parse_hs(contents: str) -> List[str]
┃   ┃   ┣━━ def parse_lisp(content: str) -> List[str]
┃   ┃   ┣━━ def parse_capnp(contents: str) -> List[str]
┃   ┃   ┣━━ def parse_grpc(contents: str) -> List[str]
┃   ┃   ┣━━ def parse_openrpc_json(contents: str) -> List[str]
┃   ┃   ┣━━ def parse_json_rpc(contents: str) -> List[str]
┃   ┃   ┣━━ def parse_graphql(contents: str) -> List[str]
┃   ┃   ┣━━ def format_dependency(name, details)
┃   ┃   ┣━━ def parse_cargo_toml(contents: str) -> List[str]
┃   ┃   ┣━━ def parse_pyproject_toml(contents: str) -> List[str]
┃   ┃   ┣━━ def parse_lean(lean_content: str) -> List[str]
┃   ┃   ┣━━ def parse_cs(contents: str) -> List[str]
┃   ┃   ┣━━ def parse_tex(tex_content: str) -> List[str]
┃   ┃   ┣━━ def parse_rb(contents) -> List[str]
┃   ┃   ┣━━ def remove_c_comments(multiline_string)
┃   ┃   ┣━━ def parse_cpp(contents) -> List[str]
┃   ┃   ┣━━ def parse_c(contents) -> List[str]
┃   ┃   ┣━━ def parse_go(contents) -> List[str]
┃   ┃   ┣━━ def parse_swift(contents: str) -> List[str]
┃   ┃   ┣━━ def parse_bash(contents) -> List[str]
┃   ┃   ┣━━ def parse_d_dot_ts(contents) -> List[str]
┃   ┃   ┣━━ def parse_angular_app_module(contents) -> List[str]
┃   ┃   ┣━━ def parse_angular_routes(content) -> List[str]
┃   ┃   ┣━━ def parse_angular_spec(content) -> List[str]
┃   ┃   ┣━━ def parse_environment_ts(contents) -> List[str]
┃   ┃   ┣━━ def parse_dot_env(contents) -> List[str]
┃   ┃   ┣━━ def parse_requirements_txt(contents) -> List[str]
┃   ┃   ┣━━ def parse_json_schema(contents) -> List[str]
┃   ┃   ┣━━ def parse_package_json(contents) -> List[str]
┃   ┃   ┣━━ def remove_ts_comments(contents: str) -> str
┃   ┃   ┣━━ def parse_ts(contents: str) -> List[str]
┃   ┃   ┣━━ def parse_makefile(contents: str) -> List[str]
┃   ┃   ┣━━ def parse_sql(contents: str) -> List[str]
┃   ┃   ┣━━ def is_openapi_yml(ymls: Tuple[dict]) -> bool
┃   ┃   ┣━━ def is_k8s_yml(ymls: Tuple[dict]) -> bool
┃   ┃   ┣━━ def is_ansible_yml(ymls: Tuple[dict]) -> bool
┃   ┃   ┣━━ def is_github_yml(ymls: Tuple[dict]) -> bool
┃   ┃   ┣━━ def parse_github_yml(ymls: Tuple[dict]) -> List[str]
┃   ┃   ┣━━ def parse_k8s(ymls: Tuple[dict]) -> List[str]
┃   ┃   ┣━━ def parse_ansible(ymls: Tuple[dict]) -> List[str]
┃   ┃   ┣━━ def parse_openapi_yml(ymls: Tuple[dict]) -> List[str]
┃   ┃   ┣━━ def parse_yml(contents: str) -> List[str]
┃   ┃   ┣━━ def extract_nodes(node, node_type, parent=None)
┃   ┃   ┣━━ def is_typing_construct(node)
┃   ┃   ┣━━ def is_builtin_type(node, parent)
┃   ┃   ┣━━ def parse_py(contents: str) -> List[str]
┃   ┃   ┣━━ def parse_db(db_path: str) -> List[str]
┃   ┃   ┣━━ def parse_cobol(content: str) -> List[str]
┃   ┃   ┣━━ def parse_java(contents: str) -> List[str]
┃   ┃   ┣━━ def parse_julia(content: str) -> List[str]
┃   ┃   ┣━━ def parse_kt(contents: str) -> List[str]
┃   ┃   ┣━━ def parse_lua(content: str) -> List[str]
┃   ┃   ┣━━ def parse_objective_c(content: str) -> List[str]
┃   ┃   ┣━━ def parse_ocaml(content: str) -> List[str]
┃   ┃   ┣━━ def parse_apl(content: str) -> List[str]
┃   ┃   ┣━━ def parse_perl(content: str) -> List[str]
┃   ┃   ┣━━ def parse_php(content: str) -> List[str]
┃   ┃   ┣━━ def parse_powershell(contents: str) -> List[str]
┃   ┃   ┣━━ def parse_matlab(content: str) -> List[str]
┃   ┃   ┣━━ def parse_scala(contents: str) -> List[str]
┃   ┃   ┣━━ def parse_tf(contents: str) -> List[str]
┃   ┃   ┣━━ def parse_md(content: str) -> List[str]
┃   ┃   ┣━━ def parse_txt(content: str) -> List[str]
┃   ┃   ┗━━ def parse_markers(content: str) -> List[str]
┃   ┣━━ 📄 traverse_directory.py (429 tokens, 58 lines)
┃   ┃   ┗━━ def traverse_directory(
┃   ┃           directory_path: str, ignore: IgnoreInput = None, globs: IgnoreInput = None
┃   ┃       ) -> List[str]
┃   ┗━━ 📄 version.py (18 tokens, 2 lines)
┃       ┗━━ __version__ = "1.0.14"
┣━━ 📄 .gitignore (210 tokens, 50 lines)
┣━━ 📄 LICENSE (2123 tokens, 81 lines)
┣━━ 📄 Makefile (383 tokens, 73 lines)
┃   ┣━━ SHELL := /bin/bash
┃   ┣━━ cli
┃   ┣━━ debug
┃   ┣━━ .PHONY: debug_command
┃   ┣━━ debug_command: test test_cli
┃   ┣━━ test: test_normally test_e2e test_tp_dotdot test_cli test_deploy
┃   ┣━━ test_normally
┃   ┣━━ test_e2e
┃   ┣━━ test_tp_dotdot
┃   ┣━━ test_cli: cli
┃   ┣━━ test_deploy
┃   ┣━━ test_dotenv
┃   ┣━━ build: install-build-tool clean-dist
┃   ┣━━ install-wheel
┃   ┣━━ install-build-tool
┃   ┣━━ test-publish: install-twine
┃   ┣━━ install-twine
┃   ┣━━ publish: install-twine
┃   ┣━━ clean-dist
┃   ┣━━ t1
┃   ┣━━ t2
┃   ┣━━ t3
┃   ┣━━ t4
┃   ┗━━ t5
┣━━ 📄 nodemon.json (124 tokens, 23 lines)
┣━━ 📄 pyproject.toml (327 tokens, 41 lines)
┃   ┣━━ name: tree_plus
┃   ┣━━ version: N/A
┃   ┣━━ description: A `tree` util enhanced with tokens, lines, and components.
┃   ┣━━ License :: OSI Approved :: Apache Software License
┃   ┣━━ License :: OSI Approved :: MIT License
┃   ┣━━ dependencies:
┃   ┣━━     tiktoken
┃   ┣━━     PyYAML
┃   ┣━━     click
┃   ┣━━     rich
┃   ┗━━     tomli
┣━━ 📄 pytest.ini (11 tokens, 3 lines)
┣━━ 📄 README.md (24293 tokens, 1739 lines)
┃   ┣━━ # Tree Plus
┃   ┣━━ ## Example Output:
┃   ┣━━ - [ ] Demonstrate Parsed Checkboxes
┃   ┣━━ ## Start Quick!
┃   ┣━━ ### Prerequisites
┃   ┣━━ ### Install Tree Plus
┃   ┣━━ #### PyPI
┃   ┣━━ #### Local Hackable Install
┃   ┣━━ ### Alias Usage
┃   ┣━━ ## Moar Languages
┃   ┣━━ ## Got Globs?
┃   ┣━━ ## Languages Todo:
┃   ┣━━ ## Oppose Unfair Business Practices
┃   ┗━━ ## License
┗━━ 📄 tree_plus_cli.py (3497 tokens, 448 lines)
    ┣━━ NOTE (Line 408): parent_count unused, is that ok?
    ┣━━ def tree_to_string(tree: Tree) -> str
    ┣━━ def clean_string(input_str)
    ┣━━ def safe_print(tree)
    ┣━━ def main(
    ┃       glob: IgnoreInput,
    ┃       paths: PathsInput,
    ┃       ignore: IgnoreInput,
    ┃       debug: bool,
    ┃       version: bool,
    ┃   )
    ┣━━ def subtree(label: str) -> Tree
    ┣━━ def clean_tree(input_tree: Tree, root_node: bool = False) -> Optional[Tree]
    ┣━━ def tree_plus(
    ┃       path_or_paths: Union[str, Tuple[str]],
    ┃       ignore: IgnoreInput = None,
    ┃       globs: IgnoreInput = None,
    ┃   ) -> Tree
    ┣━━ def _parse_paths(path_or_paths: Union[str, Tuple[str]]) -> Tuple[str]
    ┣━━ def flatten_to_str(collection: Collection)
    ┣━━ def _handle_paths(paths: Tuple[str], ignore: Ignore, globs: Ignore) -> Tree
    ┗━━ def _handle_path(
            path: str, ignore: Ignore, globs: Ignore, paths_to_trees: dict
        ) -> Tuple[Tree, TokenLineCount]

```
<!-- t1-end -->
- [x] Demonstrate Parsed Checkboxes

Here's how `tree_plus --help` looks (`-h` and `-H` both also work) 
<!-- t5-start -->
```sh
tree_plus -h
Usage: tree_plus [OPTIONS] [PATHS]...

  A `tree` util enhanced with tokens, lines, and components.

  Wrap glob patterns in quotes: -i "*.py" / -g "*.rs"

  Examples:

          Show tree_plus_src and tests simultaneously
              > tree_plus tree_plus_src tests

          Show files matching "*.*s" within tests/more_languages
              > tree_plus -g "*.*s" tests/more_languages

          Ignore Java files
              > tree_plus tests -i "*.java"

Options:
  -i, -I, --ignore TEXT  Patterns to ignore, in quotes: -i "*.java"
  -g, -G, --glob TEXT    Patterns to find, in quotes: -g "*.rs"
  -v, -V, --version      Print the version and exit.
  -d, -D, --debug        Enables $DEBUG_TREE_PLUS.
  -H, -h, --help         Show this message and exit.

  (v1.0.14) --- https://github.com/bionicles/tree_plus

```
<!-- t5-end -->

## Start Quick!

### Prerequisites

Ensure you have the following tools installed:

- [Miniconda3 (conda)](https://docs.conda.io/en/latest/miniconda.html)
- [GitHub CLI (gh)](https://cli.github.com/manual/installation)

### Install Tree Plus

#### PyPI

```bash
pip install tree_plus
```

#### Local Hackable Install

1. Clone the repository and cd into the tree_plus directory
```bash
gh repo clone https://github.com/bionicles/tree_plus.git && cd tree_plus
```

2. Install tree_plus using Makefile
```bash
make cli
```

Alternatively, if `make` is not installed, use `pip`:
```bash
pip install -e .[dev]
```

Now, you can use the `tree_plus` command.

3. Add slick aliases like `tp` to your RC file

Typical RC_FILE values `~/.bash_profile`, `~/.bashrc`, or `~/.zshrc`

Use our **idempotent aliasing script**: [alias_tree_plus.sh](https://github.com/bionicles/tree_plus/blob/main/tree_plus_src/scripts/alias_tree_plus.sh)
```bash
RC_FILE=~/.bash_profile source alias_tree_plus.sh
```
```bash
RC_FILE=~/.bashrc source alias_tree_plus.sh
```
```zsh
RC_FILE=~/.zshrc source alias_tree_plus.sh
```

### Alias Usage

Run `tree_plus` with a shorter alias:

```sh
tp
```

Reinstall `tree_plus` quickly:

```sh
tpcli
```

`cd` (change directory) to the `TREE_PLUS_PATH`:

```sh
cdtp
```

Run "debug mode" (test runner)

Ensure your rig has a global install of [nodemon](https://www.npmjs.com/package/nodemon):
```sh
npm install -g nodemon
```

Watch for changes to auto re-test `tree_plus`:
```sh
make debug
```

## Moar Languages

<!-- t2-start -->
```sh
tree_plus -i group_todo tests/more_languages
paths=('tests/more_languages',)
count_tokens_lines Error reading /home/runner/work/tree_plus/tree_plus/tests/more_languages/group3/test.sqlite: 'utf-8' codec can't decode byte 0xe9 in position 99: invalid continuation byte
📁 more_languages (27625 tokens, 4096 lines)
┣━━ 📁 group1 (3228 tokens, 622 lines)
┃   ┣━━ 📄 COBOL_TEST.CBL (57 tokens, 11 lines)
┃   ┃   ┣━━ IDENTIFICATION DIVISION -> PROGRAM-ID. HELLO
┃   ┃   ┣━━ DATA DIVISION -> 01 GREETING
┃   ┃   ┗━━ PROCEDURE DIVISION
┃   ┣━━ 📄 JavaTest.java (467 tokens, 87 lines)
┃   ┃   ┣━━ abstract class LivingBeing
┃   ┃   ┣━━     abstract void breathe()
┃   ┃   ┣━━ interface Communicator
┃   ┃   ┣━━     String communicate()
┃   ┃   ┣━━ @Log
┃   ┃   ┣━━ @Getter
┃   ┃   ┣━━ @Setter
┃   ┃   ┣━━ class Person extends LivingBeing implements Communicator
┃   ┃   ┣━━     Person(String name, int age)
┃   ┃   ┣━━     @Override
┃   ┃   ┣━━     void breathe()
┃   ┃   ┣━━     @Override
┃   ┃   ┣━━     public String communicate()
┃   ┃   ┣━━     void greet()
┃   ┃   ┣━━     String personalizedGreeting(String greeting, Optional<Boolean> includeAge)
┃   ┃   ┣━━ @Singleton
┃   ┃   ┣━━ @RestController
┃   ┃   ┣━━ @SpringBootApplication
┃   ┃   ┣━━ public class Example
┃   ┃   ┣━━     @Inject
┃   ┃   ┣━━     public Example(Person person)
┃   ┃   ┣━━     @RequestMapping("/greet")
┃   ┃   ┣━━     String home(@RequestParam(value = "name", defaultValue = "World") String name,
┃   ┃   ┃                   @RequestParam(value = "age", defaultValue = "30") int age)
┃   ┃   ┗━━     public static void main(String[] args)
┃   ┣━━ 📄 JuliaTest.jl (42 tokens, 12 lines)
┃   ┃   ┣━━ module JuliaTest
┃   ┃   ┣━━ module JuliaTest -> struct Person
┃   ┃   ┗━━ module JuliaTest -> greet(p::Person)
┃   ┣━━ 📄 KotlinTest.kt (998 tokens, 172 lines)
┃   ┃   ┣━━ data class Person(val name: String)
┃   ┃   ┣━━ fun greet(person: Person)
┃   ┃   ┣━━ fun <T> processItems(items: List<T>, processor: (T) -> Unit)
┃   ┃   ┣━━ interface Source<out T>
┃   ┃   ┣━━     fun nextT(): T
┃   ┃   ┣━━ fun MutableList<Int>.swap(index1: Int, index2: Int)
┃   ┃   ┣━━ fun Any?.toString(): String
┃   ┃   ┣━━ tailrec fun findFixPoint(x: Double = 1.0): Double
┃   ┃   ┣━━ class GenericRepository<T>
┃   ┃   ┣━━     fun getItem(id: Int): T?
┃   ┃   ┣━━ sealed interface Error
┃   ┃   ┣━━ sealed class IOError(): Error
┃   ┃   ┣━━ object Runner
┃   ┃   ┣━━     inline fun <reified S: SomeClass<T>, T> run() : T
┃   ┃   ┣━━ infix fun Int.shl(x: Int): Int
┃   ┃   ┣━━ class MyStringCollection
┃   ┃   ┣━━     infix fun add(s: String)
┃   ┃   ┣━━     fun build()
┃   ┃   ┣━━ open class Base(p: Int)
┃   ┃   ┣━━ class Derived(p: Int) : Base(p)
┃   ┃   ┣━━ open class Shape
┃   ┃   ┣━━     open fun draw()
┃   ┃   ┣━━     fun fill()
┃   ┃   ┣━━     open fun edge(case: Int)
┃   ┃   ┣━━ interface Thingy
┃   ┃   ┣━━     fun edge()
┃   ┃   ┣━━ class Circle() : Shape(), Thingy
┃   ┃   ┣━━     override fun draw()
┃   ┃   ┣━━     final override fun edge(case: Int)
┃   ┃   ┣━━ interface Base
┃   ┃   ┣━━     fun print()
┃   ┃   ┣━━ class BaseImpl(val x: Int) : Base
┃   ┃   ┣━━     override fun print()
┃   ┃   ┣━━ internal class Derived(b: Base) : Base by b
┃   ┃   ┣━━ class Person constructor(firstName: String)
┃   ┃   ┣━━ class People(
┃   ┃   ┃       firstNames: Array<String>,
┃   ┃   ┃       ages: Array<Int>(42),
┃   ┃   ┃   )
┃   ┃   ┣━━     fun edgeCases(): Boolean
┃   ┃   ┣━━ class Alien public @Inject constructor(
┃   ┃   ┃       val firstName: String,
┃   ┃   ┃       val lastName: String,
┃   ┃   ┃       var age: Int,
┃   ┃   ┃       val pets: MutableList<Pet> = mutableListOf(),
┃   ┃   ┃   )
┃   ┃   ┣━━     fun objectOriented(): String
┃   ┃   ┣━━  enum class IntArithmetics : BinaryOperator<Int>, IntBinaryOperator
┃   ┃   ┣━━     PLUS {
┃   ┃   ┃           override fun apply(t: Int, u: Int): Int
┃   ┃   ┣━━     TIMES {
┃   ┃   ┃           override fun apply(t: Int, u: Int): Int
┃   ┃   ┣━━     override fun applyAsInt(t: Int, u: Int)
┃   ┃   ┣━━ fun reformat(
┃   ┃   ┃       str: String,
┃   ┃   ┃       normalizeCase: Boolean = true,
┃   ┃   ┃       upperCaseFirstLetter: Boolean = true,
┃   ┃   ┃       divideByCamelHumps: Boolean = false,
┃   ┃   ┃       wordSeparator: Char = ' ',
┃   ┃   ┃   )
┃   ┃   ┣━━ operator fun Point.unaryMinus()
┃   ┃   ┣━━ abstract class Polygon
┃   ┃   ┗━━     abstract fun draw()
┃   ┣━━ 📄 LuaTest.lua (84 tokens, 16 lines)
┃   ┃   ┣━━ function HelloWorld.new
┃   ┃   ┣━━ function HelloWorld.greet
┃   ┃   ┗━━ function say_hello
┃   ┣━━ 📄 ObjectiveCTest.m (59 tokens, 16 lines)
┃   ┃   ┣━━ @interface HelloWorld
┃   ┃   ┣━━ @interface HelloWorld -> (void) sayHello
┃   ┃   ┣━━ @implementation HelloWorld
┃   ┃   ┣━━ @implementation HelloWorld -> (void) sayHello
┃   ┃   ┗━━ void sayHelloWorld()
┃   ┣━━ 📄 OcamlTest.ml (53 tokens, 12 lines)
┃   ┃   ┣━━ type color
┃   ┃   ┣━━ class hello
┃   ┃   ┣━━ class hello -> method say_hello
┃   ┃   ┗━━ let main ()
┃   ┣━━ 📄 test.js (755 tokens, 154 lines)
┃   ┃   ┣━━ class MyClass
┃   ┃   ┣━━   myMethod()
┃   ┃   ┣━━   async asyncMethod(a, b)
┃   ┃   ┣━━   methodWithDefaultParameters(a = 5, b = 10)
┃   ┃   ┣━━   multilineMethod(
┃   ┃   ┃       c,
┃   ┃   ┃       d
┃   ┃   ┃     )
┃   ┃   ┣━━   multilineMethodWithDefaults(
┃   ┃   ┃       t = "tree",
┃   ┃   ┃       p = "plus"
┃   ┃   ┃     )
┃   ┃   ┣━━ function myFunction(param1, param2)
┃   ┃   ┣━━ function multilineFunction(
┃   ┃   ┃     param1,
┃   ┃   ┃     param2
┃   ┃   ┃   )
┃   ┃   ┣━━ const arrowFunction = () =>
┃   ┃   ┣━━ const parametricArrow = (a, b) =>
┃   ┃   ┣━━ function ()
┃   ┃   ┣━━ function outerFunction(outerParam)
┃   ┃   ┣━━   function innerFunction(innerParam)
┃   ┃   ┣━━ const myObject = {
┃   ┃   ┣━━   myMethod: function (stuff)
┃   ┃   ┣━━ let myArrowObject = {
┃   ┃   ┣━━   myArrow: ({
┃   ┃   ┃       a,
┃   ┃   ┃       b,
┃   ┃   ┃       c,
┃   ┃   ┃     }) =>
┃   ┃   ┣━━ const myAsyncArrowFunction = async () =>
┃   ┃   ┣━━ function functionWithRestParameters(...args)
┃   ┃   ┣━━ const namedFunctionExpression = function myNamedFunction()
┃   ┃   ┣━━ const multilineArrowFunction = (
┃   ┃   ┃     a,
┃   ┃   ┃     b
┃   ┃   ┃   ) =>
┃   ┃   ┣━━ function functionReturningFunction()
┃   ┃   ┣━━   return function ()
┃   ┃   ┣━━ function destructuringOnMultipleLines({
┃   ┃   ┃     a,
┃   ┃   ┃     b,
┃   ┃   ┃   })
┃   ┃   ┣━━ const arrowFunctionWithDestructuring = ({ a, b }) =>
┃   ┃   ┣━━ const multilineDestructuringArrow = ({
┃   ┃   ┃     a,
┃   ┃   ┃     b,
┃   ┃   ┃   }) =>
┃   ┃   ┣━━ async function asyncFunctionWithErrorHandling()
┃   ┃   ┣━━ class Car
┃   ┃   ┣━━   constructor(brand)
┃   ┃   ┣━━   present()
┃   ┃   ┣━━ class Model extends Car
┃   ┃   ┣━━   constructor(brand, mod)
┃   ┃   ┗━━   show()
┃   ┗━━ 📄 test.ts (713 tokens, 142 lines)
┃       ┣━━ type MyType
┃       ┣━━ interface MyInterface
┃       ┣━━ class TsClass
┃       ┣━━   myMethod()
┃       ┣━━   myMethodWithArgs(param1: string, param2: number): void
┃       ┣━━   static myStaticMethod<T>(param: T): T
┃       ┣━━   multilineMethod(
┃       ┃       c: number,
┃       ┃       d: number
┃       ┃     ): number
┃       ┣━━   multilineMethodWithDefaults(
┃       ┃       t: string = "tree",
┃       ┃       p: string = "plus"
┃       ┃     ): string
┃       ┣━━ export class AdvancedComponent implements MyInterface
┃       ┣━━   async myAsyncMethod(
┃       ┃       a: string,
┃       ┃       b: number,
┃       ┃       c: string
┃       ┃     ): Promise<void>
┃       ┣━━   genericMethod<T, U>(
┃       ┃       arg1: T,
┃       ┃       arg2: U
┃       ┃     ): [T, U]
┃       ┣━━ export class TicketsComponent implements MyInterface
┃       ┣━━   async myAsyncMethod({ a, b, c }: { a: String; b: Number; c: String })
┃       ┣━━ function tsFunction()
┃       ┣━━ function tsFunctionSigned(
┃       ┃     param1: number,
┃       ┃     param2: number
┃       ┃   ): void
┃       ┣━━ const tsArrowFunctionSigned = ({
┃       ┃     a,
┃       ┃     b,
┃       ┃   }: {
┃       ┃     a: number;
┃       ┃     b: string;
┃       ┃   }) =>
┃       ┣━━ const arrowFunction = () =>
┃       ┣━━ const arrow = (a: String, b: Number) =>
┃       ┣━━ const asyncArrowFunction = async () =>
┃       ┣━━ const asyncArrow = async (a: String, b: Number) =>
┃       ┣━━ let weirdArrow = () =>
┃       ┣━━ const asyncPromiseArrow = async (): Promise<void> =>
┃       ┣━━ let myWeirdArrowSigned = (x: number): number =>
┃       ┣━━ class Person
┃       ┣━━   constructor(private firstName: string, private lastName: string)
┃       ┣━━   getFullName(): string
┃       ┣━━   describe(): string
┃       ┣━━ class Employee extends Person
┃       ┣━━   constructor(
┃       ┃       firstName: string,
┃       ┃       lastName: string,
┃       ┃       private jobTitle: string
┃       ┃     )
┃       ┣━━   describe(): string
┃       ┣━━ interface Shape
┃       ┗━━ interface Square extends Shape
┣━━ 📁 group2 (1714 tokens, 305 lines)
┃   ┣━━ 📄 apl_test.apl (44 tokens, 5 lines)
┃   ┃   ┣━━ :Namespace HelloWorld
┃   ┃   ┣━━ :Namespace HelloWorld -> hello ← 'Hello, World!'
┃   ┃   ┗━━ :Namespace HelloWorld -> plus ← {⍺+⍵}
┃   ┣━━ 📄 c_test.c (886 tokens, 141 lines)
┃   ┃   ┣━━ struct Point
┃   ┃   ┣━━ struct Point getOrigin()
┃   ┃   ┣━━ float mul_two_floats(float x1, float x2)
┃   ┃   ┣━━ enum days
┃   ┃   ┣━━ long add_two_longs(long x1, long x2)
┃   ┃   ┣━━ double multiplyByTwo(double num)
┃   ┃   ┣━━ char getFirstCharacter(char *str)
┃   ┃   ┣━━ void greet(Person p)
┃   ┃   ┣━━ typedef struct Person
┃   ┃   ┣━━ int main()
┃   ┃   ┣━━ int* getArrayStart(int arr[], int size)
┃   ┃   ┣━━ long complexFunctionWithMultipleArguments(
┃   ┃   ┃       int param1,
┃   ┃   ┃       double param2,
┃   ┃   ┃       char *param3,
┃   ┃   ┃       struct Point point
┃   ┃   ┃   )
┃   ┃   ┣━━ keyPattern *ACLKeyPatternCreate(sds pattern, int flags)
┃   ┃   ┣━━ sds sdsCatPatternString(sds base, keyPattern *pat)
┃   ┃   ┣━━ static int ACLCheckChannelAgainstList(list *reference, const char *channel, int channellen, int is_pattern)
┃   ┃   ┗━━ static struct config
┃   ┣━━ 📄 go_test.go (169 tokens, 46 lines)
┃   ┃   ┣━━ type Greeting struct
┃   ┃   ┣━━ func (g Greeting) sayHello()
┃   ┃   ┣━━ func createGreeting(m string) Greeting
┃   ┃   ┣━━ type SomethingLong struct
┃   ┃   ┣━━ func (s *SomethingLong) WithAReasonableName(
┃   ┃   ┃     ctx context.Context,
┃   ┃   ┃     param1 string,
┃   ┃   ┃     param2 int,
┃   ┃   ┃     param3 mapinterface{},
┃   ┃   ┃     callback func(int) error,
┃   ┃   ┃   ) (resultType, error)
┃   ┃   ┣━━ type resultType struct
┃   ┃   ┗━━ func main()
┃   ┣━━ 📄 PerlTest.pl (75 tokens, 20 lines)
┃   ┃   ┣━━ package PerlTest
┃   ┃   ┣━━ package PerlTest -> sub new
┃   ┃   ┣━━ package PerlTest -> sub hello
┃   ┃   ┗━━ package PerlTest -> sub say_hello
┃   ┣━━ 📄 PhpTest.php (74 tokens, 19 lines)
┃   ┃   ┣━━ class HelloWorld
┃   ┃   ┣━━ class HelloWorld -> function sayHello
┃   ┃   ┣━━ function greet
┃   ┃   ┣━━ class Person
┃   ┃   ┗━━ class Person -> function __construct
┃   ┣━━ 📄 PowershellTest.ps1 (169 tokens, 27 lines)
┃   ┃   ┣━━ function Test-Ordering($foo)
┃   ┃   ┣━━ class Person
┃   ┃   ┣━━ class Person -> Person($name)
┃   ┃   ┣━━ class Person -> Greet()
┃   ┃   ┣━━ class Person -> GreetMany($times)
┃   ┃   ┣━━ class Person -> NoReturn($times)
┃   ┃   ┣━━ class Person -> NoReturnNoArgs()
┃   ┃   ┗━━ function Say-Hello([Person]$person)
┃   ┣━━ 📄 ScalaTest.scala (217 tokens, 41 lines)
┃   ┃   ┣━━ def sumOfSquares(x: Int, y: Int): Int
┃   ┃   ┣━━ trait Bark
┃   ┃   ┣━━   def bark: String
┃   ┃   ┣━━ case class Person(name: String)
┃   ┃   ┣━━ class GenericClass[T](
┃   ┃   ┃       val data: T,
┃   ┃   ┃       val count: Int
┃   ┃   ┃   )
┃   ┃   ┣━━   def getData: T
┃   ┃   ┣━━ object HelloWorld
┃   ┃   ┣━━   def greet(person: Person): Unit
┃   ┃   ┣━━   def main(args: Array[String]): Unit
┃   ┃   ┣━━ def complexFunction(
┃   ┃   ┃       a: Int,
┃   ┃   ┃       b: String,
┃   ┃   ┃       c: Float
┃   ┃   ┃   ): (Int, String) Option
┃   ┃   ┗━━ def sumOfSquaresShort(x: Int, y: Int): Int
┃   ┗━━ 📄 test.csv (80 tokens, 6 lines)
┃       ┣━━ Name
┃       ┣━━ Age
┃       ┣━━ Country
┃       ┣━━ City
┃       ┗━━ Email
┣━━ 📁 group3 (6898 tokens, 1033 lines)
┃   ┣━━ 📄 bash_test.sh (154 tokens, 23 lines)
┃   ┃   ┣━━ echo_hello_world()
┃   ┃   ┣━━ function fun_echo_hello_world()
┃   ┃   ┣━━ export SECRET
┃   ┃   ┣━━ alias md='make debug'
┃   ┃   ┣━━ add_alias()
┃   ┃   ┗━━ create_conda_env()
┃   ┣━━ 📄 cpp_test.cpp (1737 tokens, 259 lines)
┃   ┃   ┣━━ class Person
┃   ┃   ┣━━ void globalGreet()
┃   ┃   ┣━━ int main()
┃   ┃   ┣━━ void printMessage(const std :: string &message)
┃   ┃   ┣━━ template<typename T>
┃   ┃   ┃   void printVector(const std :: vector<T>& vec)
┃   ┃   ┣━━ struct Point
┃   ┃   ┣━━ class Animal
┃   ┃   ┣━━ class Dog : public Animal
┃   ┃   ┣━━ class Cat : public Animal
┃   ┃   ┣━━ nb :: bytes BuildRnnDescriptor(int input_size, int hidden_size, int num_layers,
┃   ┃   ┃                                int batch_size, int max_seq_length, float dropout,
┃   ┃   ┃                                bool bidirectional, bool cudnn_allow_tf32,
┃   ┃   ┃              int workspace_size, int reserve_space_size)
┃   ┃   ┣━━ int main()
┃   ┃   ┣━━ enum ECarTypes
┃   ┃   ┣━━ ECarTypes GetPreferredCarType()
┃   ┃   ┣━━ enum ECarTypes : uint8_t
┃   ┃   ┣━━ enum class ECarTypes : uint8_t
┃   ┃   ┣━━ void myFunction(string fname, int age)
┃   ┃   ┣━━ template <typename T> T cos(T)
┃   ┃   ┣━━ template <typename T> T sin(T)
┃   ┃   ┣━━ template <typename T> T sqrt(T)
┃   ┃   ┣━━ template<typename T> struct VLEN
┃   ┃   ┣━━ template<typename T> class arr
┃   ┃   ┣━━ class Buffer
┃   ┃   ┗━━ std :: tuple<array, array, array> quantize(
┃   ┃           const array& w,
┃   ┃           int group_size,
┃   ┃           int bits,
┃   ┃           StreamOrDevice s)
┃   ┣━━ 📄 csharp_test.cs (850 tokens, 147 lines)
┃   ┃   ┣━━ public interface IExcelTemplate
┃   ┃   ┣━━     void LoadTemplate(string templateFilePath)
┃   ┃   ┣━━     void LoadData(Dictionary<string, string> data)
┃   ┃   ┣━━     void ModifyCell(string cellName, string value)
┃   ┃   ┣━━     void SaveToFile(string filePath)
┃   ┃   ┣━━ public interface IGreet
┃   ┃   ┣━━     void Greet()
┃   ┃   ┣━━ public enum WeekDays
┃   ┃   ┣━━ public delegate void DisplayMessage(string message)
┃   ┃   ┣━━ public struct Address
┃   ┃   ┣━━ public static class HelperFunctions
┃   ┃   ┣━━     public static void PrintMessage(string message)
┃   ┃   ┣━━     public static int AddNumbers(int a, int b)
┃   ┃   ┣━━ namespace HelloWorldApp
┃   ┃   ┣━━     class Person : IGreet
┃   ┃   ┣━━         public Person(string name, int age)
┃   ┃   ┣━━         public void Greet()
┃   ┃   ┣━━     class HelloWorld
┃   ┃   ┣━━         static void Main(string[] args)
┃   ┃   ┣━━ namespace TemplateToExcelServer.Template
┃   ┃   ┣━━     public interface ITemplateObject
┃   ┃   ┣━━         string[,] GetContent()
┃   ┃   ┣━━         string[] GetContentArray()
┃   ┃   ┣━━         string[] GetFormat()
┃   ┃   ┣━━         int? GetFormatLength()
┃   ┃   ┣━━         TemplateObject SetContent(string[,] Content)
┃   ┃   ┣━━         TemplateObject SetContentArray(string[] value)
┃   ┃   ┣━━         TemplateObject SetFormat(string[] Header)
┃   ┃   ┣━━         TemplateObject SetNameOfReport(
┃   ┃   ┃               ReadOnlyMemory<byte> ReportName,
┃   ┃   ┃               int[] EdgeCase)
┃   ┃   ┣━━         TemplateObject SetSheetName(ReadOnlyMemory<byte> SheetName)
┃   ┃   ┣━━ public class BankAccount(string accountID, string owner)
┃   ┃   ┣━━     public override string ToString() =>
┃   ┃   ┣━━ var IncrementBy = (int source, int increment = 1) =>
┃   ┃   ┣━━ Func<int, int, int> add = (x, y) =>
┃   ┃   ┣━━ button.Click += (sender, args) =>
┃   ┃   ┣━━ public Func<int, int> GetMultiplier(int factor)
┃   ┃   ┣━━ public void Method(
┃   ┃   ┃           int param1,
┃   ┃   ┃           int param2,
┃   ┃   ┃           int param3,
┃   ┃   ┃           int param4,
┃   ┃   ┃           int param5,
┃   ┃   ┃           int param6,
┃   ┃   ┃       )
┃   ┃   ┣━━ System.Net.ServicePointManager.ServerCertificateValidationCallback +=
┃   ┃   ┃       (se, cert, chain, sslerror) =>
┃   ┃   ┣━━ class ServerCertificateValidation
┃   ┃   ┣━━     public bool OnRemoteCertificateValidation(
┃   ┃   ┃           object se,
┃   ┃   ┃           X509Certificate cert,
┃   ┃   ┃           X509Chain chain,
┃   ┃   ┃           SslPolicyErrors sslerror
┃   ┃   ┃       )
┃   ┃   ┣━━ s_downloadButton.Clicked += async (o, e) =>
┃   ┃   ┣━━ [HttpGet, Route("DotNetCount")]
┃   ┃   ┗━━ static public async Task<int> GetDotNetCount(string URL)
┃   ┣━━ 📄 hallucination.tex (1465 tokens, 127 lines)
┃   ┃   ┣━━ Harnessing the Master Algorithm: Strategies for AI LLMs to Mitigate Hallucinations
┃   ┃   ┣━━ Hallucinated Pedro Domingos et al.
┃   ┃   ┣━━ Christmas Eve 2023
┃   ┃   ┣━━ 1 Introduction
┃   ┃   ┣━━ 2 Representation in LLMs
┃   ┃   ┣━━   2.1 Current Representational Models
┃   ┃   ┣━━   2.2 Incorporating Cognitive Structures
┃   ┃   ┣━━   2.3 Conceptual Diagrams of Advanced Representational Models
┃   ┃   ┣━━ 3 Evaluation Strategies
┃   ┃   ┣━━   3.1 Existing Evaluation Metrics for LLMs
┃   ┃   ┣━━   3.2 Integrating Contextual and Ethical Considerations
┃   ┃   ┣━━   3.3 Case Studies: Evaluation in Practice
┃   ┃   ┣━━ 4 Optimization Techniques
┃   ┃   ┣━━   4.1 Continuous Learning Models
┃   ┃   ┣━━   4.2 Adaptive Algorithms for Real-time Adjustments
┃   ┃   ┣━━   4.3 Performance Metrics Pre- and Post-Optimization
┃   ┃   ┣━━ 5 Interdisciplinary Insights
┃   ┃   ┣━━   5.1 Cognitive Science and AI: A Symbiotic Relationship
┃   ┃   ┣━━   5.2 Learning from Human Cognitive Processes
┃   ┃   ┣━━ 6 Challenges and Future Directions
┃   ┃   ┣━━   6.1 Addressing Current Limitations
┃   ┃   ┣━━   6.2 The Road Ahead: Ethical and Practical Considerations
┃   ┃   ┣━━ 7 Conclusion
┃   ┃   ┣━━   7.1 Summarizing Key Findings
┃   ┃   ┗━━   7.2 The Next Steps in AI Development
┃   ┣━━ 📄 ruby_test.rb (152 tokens, 37 lines)
┃   ┃   ┣━━ module Greeter
┃   ┃   ┣━━   def self.say_hello
┃   ┃   ┣━━ class HelloWorld
┃   ┃   ┣━━   def say_hello
┃   ┃   ┣━━ class Human
┃   ┃   ┣━━   def self.bar
┃   ┃   ┣━━   def self.bar=(value)
┃   ┃   ┣━━ class Doctor < Human
┃   ┃   ┗━━   def brachial_plexus(
┃   ┃             roots,
┃   ┃             trunks,
┃   ┃             divisions: true,
┃   ┃             cords: [],
┃   ┃             branches: Time.now
┃   ┃           )
┃   ┣━━ 📄 swift_test.swift (525 tokens, 110 lines)
┃   ┃   ┣━━ class Person
┃   ┃   ┣━━     init(name: String)
┃   ┃   ┣━━     func greet()
┃   ┃   ┣━━     func yEdgeCase(
┃   ┃   ┃           fname: String, 
┃   ┃   ┃           lname: String, 
┃   ┃   ┃           age: Int, 
┃   ┃   ┃           address: String, 
┃   ┃   ┃           phoneNumber: String
┃   ┃   ┃       )
┃   ┃   ┣━━ func globalGreet()
┃   ┃   ┣━━ struct Point
┃   ┃   ┣━━ protocol Animal
┃   ┃   ┣━━     func speak()
┃   ┃   ┣━━ struct Dog: Animal
┃   ┃   ┣━━ class Cat: Animal
┃   ┃   ┣━━     init(name: String)
┃   ┃   ┣━━     func speak()
┃   ┃   ┣━━ enum CarType
┃   ┃   ┣━━ func getPreferredCarType() -> CarType
┃   ┃   ┣━━ enum CarType: UInt8
┃   ┃   ┣━━ enum class CarType: UInt8
┃   ┃   ┣━━ func myFunction(fname: String, age: Int)
┃   ┃   ┗━━ func myFunctionWithMultipleParameters(
┃   ┃           fname: String, 
┃   ┃           lname: String, 
┃   ┃           age: Int, 
┃   ┃           address: String, 
┃   ┃           phoneNumber: String
┃   ┃       )
┃   ┣━━ 📄 test.capnp (143 tokens, 31 lines)
┃   ┃   ┣━━ struct Employee
┃   ┃   ┣━━   id @0 :Int32
┃   ┃   ┣━━   name @1 :Text
┃   ┃   ┣━━   role @2 :Text
┃   ┃   ┣━━   skills @3 :List(Skill)
┃   ┃   ┣━━   struct Skill
┃   ┃   ┣━━     name @0 :Text
┃   ┃   ┣━━     level @1 :Level
┃   ┃   ┣━━     enum Level
┃   ┃   ┣━━       beginner @0
┃   ┃   ┣━━       intermediate @1
┃   ┃   ┣━━       expert @2
┃   ┃   ┣━━   status :union
┃   ┃   ┣━━     active @4 :Void
┃   ┃   ┣━━     onLeave @5 :Void
┃   ┃   ┣━━     retired @6 :Void
┃   ┃   ┣━━ struct Company
┃   ┃   ┗━━   employees @0 :List(Employee)
┃   ┣━━ 📄 test.graphql (83 tokens, 21 lines)
┃   ┃   ┣━━ type Query
┃   ┃   ┣━━     getBooks: [Book]
┃   ┃   ┣━━     getAuthors: [Author]
┃   ┃   ┣━━ type Mutation
┃   ┃   ┣━━     addBook(title: String, author: String): Book
┃   ┃   ┣━━     removeBook(id: ID): Book
┃   ┃   ┣━━ type Book
┃   ┃   ┣━━     id: ID
┃   ┃   ┣━━     title: String
┃   ┃   ┣━━     author: Author
┃   ┃   ┣━━ type Author
┃   ┃   ┣━━     id: ID
┃   ┃   ┣━━     name: String
┃   ┃   ┗━━     books: [Book]
┃   ┣━━ 📄 test.lean (384 tokens, 43 lines)
┃   ┃   ┣━━ # Advanced Topics in Group Theory
┃   ┃   ┣━━ section GroupDynamics
┃   ┃   ┣━━ lemma group_stability (G : Type*) [Group G] (H : Subgroup G)
┃   ┃   ┣━━ theorem subgroup_closure {G : Type*} [Group G] (S : Set G)
┃   ┃   ┣━━ axiom group_homomorphism_preservation {G H : Type*} [Group G] [Group H] (f : G → H)
┃   ┃   ┣━━ end GroupDynamics
┃   ┃   ┣━━ section ConstructiveApproach
┃   ┃   ┣━━ lemma finite_group_order (G : Type*) [Group G] [Fintype G]
┃   ┃   ┣━━ lemma complex_lemma {X Y : Type*} [SomeClass X] [AnotherClass Y]
┃   ┃   ┃     (f : X → Y) (g : Y → X)
┃   ┃   ┗━━ end ConstructiveApproach
┃   ┣━━ 📄 test.proto (150 tokens, 34 lines)
┃   ┃   ┣━━ syntax = "proto3"
┃   ┃   ┣━━ service EmployeeService
┃   ┃   ┣━━     rpc GetEmployee(EmployeeId) returns (EmployeeInfo)
┃   ┃   ┣━━     rpc AddEmployee(EmployeeData) returns (EmployeeInfo)
┃   ┃   ┣━━     rpc UpdateEmployee(EmployeeUpdate) returns (EmployeeInfo)
┃   ┃   ┣━━ message EmployeeId
┃   ┃   ┣━━     int32 id = 1
┃   ┃   ┣━━ message EmployeeInfo
┃   ┃   ┣━━     int32 id = 1
┃   ┃   ┣━━     string name = 2
┃   ┃   ┣━━     string role = 3
┃   ┃   ┣━━ message EmployeeData
┃   ┃   ┣━━     string name = 1
┃   ┃   ┣━━     string role = 2
┃   ┃   ┣━━ message EmployeeUpdate
┃   ┃   ┣━━     int32 id = 1
┃   ┃   ┣━━     string name = 2
┃   ┃   ┗━━     string role = 3
┃   ┣━━ 📄 test.sqlite
┃   ┃   ┣━━ students table:
┃   ┃   ┣━━    id integer primary key
┃   ┃   ┣━━    name text not null
┃   ┃   ┣━━    age integer not null
┃   ┃   ┣━━ courses table:
┃   ┃   ┣━━    id integer primary key
┃   ┃   ┣━━    title text not null
┃   ┃   ┗━━    credits integer not null
┃   ┣━━ 📄 test_Cargo.toml (165 tokens, 19 lines)
┃   ┃   ┣━━ name: test_cargo
┃   ┃   ┣━━ version: 0.1.0
┃   ┃   ┣━━ description: A test Cargo.toml
┃   ┃   ┣━━ license: MIT OR Apache-2.0
┃   ┃   ┣━━ dependencies:
┃   ┃   ┣━━   clap 4.4
┃   ┃   ┗━━   sqlx 0.7 (features: runtime-tokio, tls-rustls)
┃   ┣━━ 📄 test_json_rpc_2_0.json (48 tokens, 6 lines)
┃   ┃   ┣━━ jsonrpc: 2.0
┃   ┃   ┣━━ method: subtract
┃   ┃   ┣━━ params:
┃   ┃   ┣━━     minuend: 42
┃   ┃   ┣━━     subtrahend: 23
┃   ┃   ┗━━ id: 1
┃   ┣━━ 📄 test_openapi.yaml (507 tokens, 93 lines)
┃   ┃   ┣━━ openapi: 3.0.1
┃   ┃   ┣━━     title: TODO Plugin
┃   ┃   ┣━━     description: A plugin to create and manage TODO lists using ChatGPT.
┃   ┃   ┣━━     version: v1
┃   ┃   ┣━━ servers:
┃   ┃   ┣━━     - url: PLUGIN_HOSTNAME
┃   ┃   ┣━━ paths:
┃   ┃   ┣━━     '/todos/{username}':
┃   ┃   ┣━━         GET (getTodos): Get the list of todos
┃   ┃   ┣━━         POST (addTodo): Add a todo to the list
┃   ┃   ┗━━         DELETE (deleteTodo): Delete a todo from the list
┃   ┣━━ 📄 test_openrpc.json (224 tokens, 44 lines)
┃   ┃   ┣━━ openrpc: 1.2.1
┃   ┃   ┣━━ info:
┃   ┃   ┣━━     title: Demo Petstore
┃   ┃   ┣━━     version: 1.0.0
┃   ┃   ┣━━ methods:
┃   ┃   ┣━━     listPets: List all pets
┃   ┃   ┣━━         params:
┃   ┃   ┣━━             - limit: integer
┃   ┃   ┗━━         result: pets = An array of pets
┃   ┗━━ 📄 test_pyproject.toml (311 tokens, 39 lines)
┃       ┣━━ name: tree_plus
┃       ┣━━ version: 1.0.8
┃       ┣━━ description: A `tree` util enhanced with tokens, lines, and components.
┃       ┣━━ License :: OSI Approved :: Apache Software License
┃       ┣━━ License :: OSI Approved :: MIT License
┃       ┣━━ dependencies:
┃       ┣━━     tiktoken
┃       ┣━━     PyYAML
┃       ┣━━     click
┃       ┣━━     rich
┃       ┗━━     tomli
┣━━ 📁 group4 (3230 tokens, 493 lines)
┃   ┣━━ 📄 erl_test.erl (512 tokens, 69 lines)
┃   ┃   ┣━━ -module(erl_test).
┃   ┃   ┣━━ -record(person).
┃   ┃   ┣━━ -type ra_peer_status().
┃   ┃   ┣━━ -type ra_membership().
┃   ┃   ┣━━ -opaque my_opaq_type().
┃   ┃   ┣━━ -type orddict(Key, Val).
┃   ┃   ┣━━ -type edge(
┃   ┃   ┃           Cases,
┃   ┃   ┃           Pwn,
┃   ┃   ┃       ).
┃   ┃   ┣━━ -spec guarded(X) -> X when X :: tuple().
┃   ┃   ┣━━ -spec edge_case(
┃   ┃   ┃           {integer(), any()} | 
┃   ┃   ┃       ) -> processed, integer(), any()} | [{item, any()}].
┃   ┃   ┣━━ -spec complex_function({integer(), any()} | ) -> 
┃   ┃   ┃       {processed, integer(), any()} | [{item, any()}].
┃   ┃   ┣━━ -spec list_manipulation() -> .
┃   ┃   ┣━━ -spec overload(T1, T2) -> T3
┃   ┃   ┃           ; (T4, T5) -> T6.
┃   ┃   ┣━━ -spec multiguard({X, integer()}) -> X when X :: atom()
┃   ┃   ┃           ; ([Y]) -> Y when Y :: number().
┃   ┃   ┣━━ -record(multiline).
┃   ┃   ┗━━ -record(maybe_undefined).
┃   ┣━━ 📄 haskell_test.hs (373 tokens, 41 lines)
┃   ┃   ┣━━ data Person
┃   ┃   ┣━━ greet :: Person -> String
┃   ┃   ┗━━ resolveVariables ::
┃   ┃         forall m fragments.
┃   ┃         (MonadError QErr m, Traversable fragments) =>
┃   ┃         Options.BackwardsCompatibleNullInNonNullableVariables ->
┃   ┃         [G.VariableDefinition] ->
┃   ┃         GH.VariableValues ->
┃   ┃         [G.Directive G.Name] ->
┃   ┃         G.SelectionSet fragments G.Name ->
┃   ┃         m
┃   ┃           ( [G.Directive Variable],
┃   ┃             G.SelectionSet fragments Variable
┃   ┃           )
┃   ┣━━ 📄 mathematica_test.nb (132 tokens, 21 lines)
┃   ┃   ┣━━ person
┃   ┃   ┣━━ sayHello[]
┃   ┃   ┗━━ sumList
┃   ┣━━ 📄 matlab_test.m (45 tokens, 12 lines)
┃   ┃   ┣━━ classdef HelloWorld -> function greet
┃   ┃   ┗━━ function loneFun
┃   ┣━━ 📄 RTest.R (367 tokens, 47 lines)
┃   ┃   ┣━━ class(person)
┃   ┃   ┣━━ greet.Person <- function
┃   ┃   ┣━━ ensure_between = function
┃   ┃   ┗━━ run_intermediate_annealing_process = function
┃   ┣━━ 📄 rust_test.rs (888 tokens, 159 lines)
┃   ┃   ┣━━ enum Days
┃   ┃   ┣━━ struct Point
┃   ┃   ┣━━ impl Point
┃   ┃   ┣━━     fn get_origin() -> Point
┃   ┃   ┣━━ struct Person
┃   ┃   ┣━━ impl Person
┃   ┃   ┣━━     fn greet(&self)
┃   ┃   ┣━━ fn add_two_longs(x1: i64, x2: i64) -> i64
┃   ┃   ┣━━ fn add_two_longs_longer(
┃   ┃   ┃       x1: i64,
┃   ┃   ┃       x2: i64,
┃   ┃   ┃   ) -> i64
┃   ┃   ┣━━ fn multiply_by_two(num: f64) -> f64
┃   ┃   ┣━━ fn get_first_character(s: &str) -> Option<char>
┃   ┃   ┣━━ trait Drawable
┃   ┃   ┣━━     fn draw(&self)
┃   ┃   ┣━━ impl Drawable for Point
┃   ┃   ┣━━     fn draw(&self)
┃   ┃   ┣━━ fn main()
┃   ┃   ┣━━ pub struct VisibleStruct
┃   ┃   ┣━━ mod my_module
┃   ┃   ┣━━     pub struct AlsoVisibleStruct<T>(T, T)
┃   ┃   ┣━━ macro_rules! say_hello
┃   ┃   ┣━━ #[macro_export]
┃   ┃   ┃   macro_rules! hello_tree_plus
┃   ┃   ┣━━ pub mod lib
┃   ┃   ┣━━     pub mod interfaces
┃   ┃   ┣━━     mod engine
┃   ┃   ┣━━ pub fn flow<S1, S2, S3, S4, E, T, L>(
┃   ┃   ┃       source: S1, 
┃   ┃   ┃       extractor: E, 
┃   ┃   ┃       inbox: S2, 
┃   ┃   ┃       transformer: T, 
┃   ┃   ┃       outbox: S3, 
┃   ┃   ┃       loader: L, 
┃   ┃   ┃       sink: &mut S4,
┃   ┃   ┃   ) -> Result<(), Box<dyn Error>>
┃   ┃   ┃   where
┃   ┃   ┃       S1: Extractable,
┃   ┃   ┃       S2: Extractable + Loadable,
┃   ┃   ┃       S3: Extractable + Loadable,
┃   ┃   ┃       S4: Loadable,
┃   ┃   ┃       E: Extractor<S1, S2>,
┃   ┃   ┃       T: Transformer<S2, S3>,
┃   ┃   ┃       L: Loader<S3, S4>,
┃   ┃   ┣━━ trait Container
┃   ┃   ┣━━     fn items(&self) -> impl Iterator<Item = Widget>
┃   ┃   ┣━━ trait HttpService
┃   ┃   ┣━━     async fn fetch(&self, url: Url) -> HtmlBody
┃   ┃   ┣━━ struct Pair<T, U>
┃   ┃   ┣━━ trait Transformer<T>
┃   ┃   ┣━━     fn transform(&self, input: T) -> T
┃   ┃   ┣━━ impl<T: std::ops::Add<Output = T> + Copy> Transformer<T> for Pair<T, T>
┃   ┃   ┣━━     fn transform(&self, input: T) -> T
┃   ┃   ┗━━ fn main()
┃   ┣━━ 📄 test.zig (432 tokens, 61 lines)
┃   ┃   ┣━━ pub fn add(a: i32, b: i32) i32
┃   ┃   ┣━━ test "add function"
┃   ┃   ┣━━ const BunBuildOptions = struct
┃   ┃   ┣━━     pub fn updateRuntime(this: *BunBuildOptions) anyerror!void
┃   ┃   ┣━━     pub fn step(this: BunBuildOptions, b: anytype) *std.build.OptionsStep
┃   ┃   ┗━━ pub fn sgemv(
┃   ┃           order: Order,
┃   ┃           trans: Trans,
┃   ┃           m: usize,
┃   ┃           n: usize,
┃   ┃           alpha: f32,
┃   ┃           a: []const f32,
┃   ┃           lda: usize,
┃   ┃           x: []const f32,
┃   ┃           x_add: usize,
┃   ┃           beta: f32,
┃   ┃           y: []f32,
┃   ┃           y_add: usize,
┃   ┃       ) void
┃   ┣━━ 📄 test_fsharp.fs (127 tokens, 27 lines)
┃   ┃   ┣━━ module TestFSharp
┃   ┃   ┣━━ type Person = {
┃   ┃   ┣━━ let add x y =
┃   ┃   ┣━━ let multiply 
┃   ┃   ┃       (x: int) 
┃   ┃   ┃       (y: int): int =
┃   ┃   ┣━━ let complexFunction
┃   ┃   ┃       (a: int)
┃   ┃   ┃       (b: string)
┃   ┃   ┃       (c: float)
┃   ┃   ┃       : (int * string) option =
┃   ┃   ┗━━ type Result<'T> =
┃   ┣━━ 📄 test_tcl_tk.tcl (66 tokens, 17 lines)
┃   ┃   ┣━━ proc sayHello {}
┃   ┃   ┣━━ proc arrg { input }
┃   ┃   ┗━━ proc multiLine {
┃   ┃           x,
┃   ┃           y
┃   ┃       }
┃   ┗━━ 📄 tf_test.tf (288 tokens, 39 lines)
┃       ┣━━ provider "aws"
┃       ┣━━ resource "aws_instance" "example"
┃       ┣━━ data "aws_ami" "ubuntu"
┃       ┣━━ variable "instance_type"
┃       ┣━━ output "instance_public_ip"
┃       ┣━━ locals
┃       ┗━━ module "vpc"
┣━━ 📁 group5 (11392 tokens, 1504 lines)
┃   ┣━━ 📄 ansible_test.yml (60 tokens, 15 lines)
┃   ┃   ┣━━ Install package
┃   ┃   ┣━━ Start service
┃   ┃   ┗━━ Create user
┃   ┣━━ 📄 app-routing.module.ts (242 tokens, 28 lines)
┃   ┃   ┣━━ const routes: Routes = [
┃   ┃   ┃       { path: '', redirectTo: 'login', pathMatch: 'full' },
┃   ┃   ┃       { path: '*', redirectTo: 'login' },
┃   ┃   ┃       { path: 'home', component: HomeComponent },
┃   ┃   ┃       { path: 'login', component: LoginComponent },
┃   ┃   ┃       { path: 'register', component: RegisterComponent },
┃   ┃   ┃       { path: 'events', component: EventsComponent },
┃   ┃   ┃       { path: 'invites', component: InvitesComponent },
┃   ┃   ┃       { path: 'rewards', component: RewardsComponent },
┃   ┃   ┃       { path: 'profile', component: ProfileComponent },
┃   ┃   ┃   ];
┃   ┃   ┗━━ export class AppRoutingModule
┃   ┣━━ 📄 app.component.spec.ts (307 tokens, 48 lines)
┃   ┃   ┣━━ describe 'AppComponent'
┃   ┃   ┣━━     it should create the app
┃   ┃   ┣━━     it should welcome the user
┃   ┃   ┣━━     it should welcome 'Jimbo'
┃   ┃   ┗━━     it should request login if not logged in
┃   ┣━━ 📄 app.component.ts (243 tokens, 45 lines)
┃   ┃   ┣━━ export class AppComponent
┃   ┃   ┣━━   constructor(private loginService: LoginService)
┃   ┃   ┣━━   checkSession()
┃   ┃   ┣━━   async goToEvent(event_id: string)
┃   ┃   ┗━━   valInvitedBy(event: any, event_id: string)
┃   ┣━━ 📄 app.module.ts (269 tokens, 43 lines)
┃   ┃   ┣━━ @NgModule({
┃   ┃   ┃       declarations: [
┃   ┃   ┃           AppComponent,
┃   ┃   ┃           HomeComponent,
┃   ┃   ┃           LoginComponent,
┃   ┃   ┃           RegisterComponent,
┃   ┃   ┃           EventsComponent,
┃   ┃   ┃           InvitesComponent,
┃   ┃   ┃           RewardsComponent,
┃   ┃   ┃           ProfileComponent
┃   ┃   ┗━━ export class AppModule
┃   ┣━━ 📄 checkbox_test.md (263 tokens, 22 lines)
┃   ┃   ┣━━ # My Checkbox Test
┃   ┃   ┣━━ ## My No Parens Test
┃   ┃   ┣━━ ## My Empty href Test
┃   ┃   ┣━━ ## My other url Test [Q&A]
┃   ┃   ┣━━ ## My other other url Test [Q&A]
┃   ┃   ┣━━ ## My 2nd other url Test [Q&A]
┃   ┃   ┣━━ ## My 3rd other url Test [Q&A]
┃   ┃   ┣━━ - [ ] Task 1
┃   ┃   ┣━━     - [ ] No Space Task 1.1
┃   ┃   ┣━━     - [ ] Two Spaces Task 1.2
┃   ┃   ┣━━         - [ ] Subtask 1.2.1
┃   ┃   ┣━━ - [ ] Task 2
┃   ┃   ┣━━ -  Task 3
┃   ┃   ┣━━     - [ ] Subtask 3.1
┃   ┃   ┣━━ -  Task 6
┃   ┃   ┣━━     -  Subtask 6.1
┃   ┃   ┗━━         - [ ] Handle edge cases
┃   ┣━━ 📄 checkbox_test.txt (276 tokens, 33 lines)
┃   ┃   ┣━━ - [ ] fix phone number format +1
┃   ┃   ┣━━ - [ ] add forgot password
┃   ┃   ┣━━ - [ ] ? add email verification
┃   ┃   ┣━━ - [ ] store token the right way
┃   ┃   ┣━━ - [ ] test nesting of checkboxes
┃   ┃   ┣━━ - [ ] user can use option to buy ticket at 2-referred price
┃   ┃   ┣━━ - [ ] CTA refer 2 people to get instant lower price
┃   ┃   ┗━━ - [ ] form to send referrals
┃   ┣━━ 📄 environment.test.ts (193 tokens, 19 lines)
┃   ┃   ┣━━ environment:
┃   ┃   ┣━━    production
┃   ┃   ┣━━    cognitoUserPoolId
┃   ┃   ┣━━    cognitoAppClientId
┃   ┃   ┗━━    apiurl
┃   ┣━━ 📄 k8s_test.yaml (171 tokens, 38 lines)
┃   ┃   ┣━━ apps/v1.Deployment -> my-app
┃   ┃   ┣━━ v1.Service -> my-service
┃   ┃   ┗━━ v1.ConfigMap -> my-config
┃   ┣━━ 📄 Makefile (882 tokens, 85 lines)
┃   ┃   ┣━━ include dotenv/dev.env
┃   ┃   ┣━━ .PHONY: dev
┃   ┃   ┣━━ dev
┃   ┃   ┣━━ services-down
┃   ┃   ┣━━ services-stop: services-down
┃   ┃   ┣━━ define CHECK_POSTGRES
┃   ┃   ┣━━ damage-report
┃   ┃   ┣━━ tail-logs
┃   ┃   ┗━━ cloud
┃   ┣━━ 📄 requirements_test.txt (40 tokens, 10 lines)
┃   ┃   ┣━━ psycopg2-binary
┃   ┃   ┣━━ pytest
┃   ┃   ┣━━ coverage
┃   ┃   ┣━━ flask
┃   ┃   ┣━━ flask_cors
┃   ┃   ┣━━ stripe
┃   ┃   ┣━━ pyjwt
┃   ┃   ┣━━ cognitojwt
┃   ┃   ┗━━ flask-lambda
┃   ┣━━ 📄 rust_todo_test.rs (109 tokens, 27 lines)
┃   ┃   ┣━━ TODO (Line 23): This todo tests parse_todo
┃   ┃   ┣━━ enum Color
┃   ┃   ┣━━ struct Point
┃   ┃   ┣━━ trait Drawable
┃   ┃   ┣━━     fn draw(&self)
┃   ┃   ┣━━ impl Drawable for Point
┃   ┃   ┣━━     fn draw(&self)
┃   ┃   ┗━━ fn main()
┃   ┣━━ 📄 sql_test.sql (300 tokens, 52 lines)
┃   ┃   ┣━━ CREATE TABLE promoters
┃   ┃   ┣━━    user_id serial PRIMARY KEY,
┃   ┃   ┣━━    type varchar(20) NOT NULL,
┃   ┃   ┣━━    username varchar(20) NOT NULL,
┃   ┃   ┣━━    password varchar(20) NOT NULL,
┃   ┃   ┣━━    email varchar(30) NOT NULL,
┃   ┃   ┣━━    phone varchar(20) NOT NULL,
┃   ┃   ┣━━    promocode varchar(20),
┃   ┃   ┣━━    info json,
┃   ┃   ┣━━    going text[],
┃   ┃   ┣━━    invites text[],
┃   ┃   ┣━━    balance integer NOT NULL,
┃   ┃   ┣━━    rewards text[],
┃   ┃   ┣━━    created timestamp
┃   ┃   ┣━━ CREATE TABLE events
┃   ┃   ┣━━    event_id serial PRIMARY KEY,
┃   ┃   ┣━━    name varchar(64) NOT NULL,
┃   ┃   ┣━━    date varchar(64) NOT NULL,
┃   ┃   ┣━━    location varchar(64) NOT NULL,
┃   ┃   ┣━━    performer varchar(64) NOT NULL,
┃   ┃   ┣━━    rewards json,
┃   ┃   ┗━━    created timestamp
┃   ┣━━ 📄 standard-app-routing.module.ts (93 tokens, 17 lines)
┃   ┃   ┗━━ const routes: Routes = [
┃   ┃         { path: '', component: HomeComponent },
┃   ┃         {
┃   ┃           path: 'heroes',
┃   ┃           component: HeroesListComponent,
┃   ┃           children: [
┃   ┃             { path: ':id', component: HeroDetailComponent },
┃   ┃             { path: 'new', component: HeroFormComponent },
┃   ┃           ],
┃   ┃         },
┃   ┃         { path: '**', component: PageNotFoundComponent },
┃   ┃       ];
┃   ┣━━ 📄 test.env (242 tokens, 26 lines)
┃   ┃   ┣━━ PROMO_PATH
┃   ┃   ┣━━ PRODUCTION
┃   ┃   ┣━━ SQL_SCHEMA_PATH
┃   ┃   ┣━━ DB_LOGS
┃   ┃   ┣━━ DB_LOG
┃   ┃   ┣━━ PGPASSWORD
┃   ┃   ┣━━ PGDATABASE
┃   ┃   ┣━━ PGHOST
┃   ┃   ┣━━ PGPORT
┃   ┃   ┣━━ PGUSER
┃   ┃   ┣━━ SERVER_LOG
┃   ┃   ┣━━ SERVER_LOGS
┃   ┃   ┣━━ API_URL
┃   ┃   ┣━━ APP_LOGS
┃   ┃   ┣━━ APP_LOG
┃   ┃   ┣━━ APP_URL
┃   ┃   ┣━━ COGNITO_USER_POOL_ID
┃   ┃   ┣━━ COGNITO_APP_CLIENT_ID
┃   ┃   ┣━━ AWS_REGION
┃   ┃   ┗━━ STRIPE_SECRET_KEY
┃   ┣━━ 📄 testJsonSchema.json (294 tokens, 49 lines)
┃   ┃   ┣━━ $schema: http://json-schema.org/draft-07/schema#
┃   ┃   ┣━━ type: object
┃   ┃   ┣━━ title: random_test
┃   ┃   ┗━━ description: A promoter's activites related to events
┃   ┣━━ 📄 testPackage.json (458 tokens, 44 lines)
┃   ┃   ┣━━ name: 'promo-app'
┃   ┃   ┣━━ version: 0.0.0
┃   ┃   ┣━━ scripts:
┃   ┃   ┣━━     ng: 'ng'
┃   ┃   ┣━━     start: 'ng serve'
┃   ┃   ┣━━     build: 'ng build'
┃   ┃   ┣━━     watch: 'ng build --watch --configuration development'
┃   ┃   ┗━━     test: 'ng test'
┃   ┗━━ 📄 tickets.component.ts (6950 tokens, 903 lines)
┃       ┣━━ interface EnrichedTicket extends Ticket
┃       ┣━━ interface SpinConfig
┃       ┣━━ interface RotationState
┃       ┣━━ interface SpeakInput
┃       ┣━━ const formatSpeakInput = (input: SpeakInput): string =>
┃       ┣━━ function hourToSpeech(hour: number, minute: number, period: string): string
┃       ┣━━ export class TicketsComponent implements AfterViewInit
┃       ┣━━   speak(input: SpeakInput)
┃       ┣━━   speakEvent(ticket: EnrichedTicket): void
┃       ┣━━   formatEvent(ticket: EnrichedTicket): string
┃       ┣━━   speakVenue(ticket: EnrichedTicket): void
┃       ┣━━   formatDate(date: Date, oneLiner: boolean = false): string
┃       ┣━━   formatDateForSpeech(date: Date): string
┃       ┣━━   async spinQRCode(
┃       ┃       event: PointerEvent,
┃       ┃       config: SpinConfig = DEFAULT_SPIN_CONFIG
┃       ┃     )
┃       ┣━━   private animateRotation(
┃       ┃       imgElement: HTMLElement,
┃       ┃       targetRotation: number,
┃       ┃       config: SpinConfig,
┃       ┃       cleanup: () => void
┃       ┃     )
┃       ┣━━     const animate = (currentTime: number) =>
┃       ┣━━   private getNext90Degree(currentRotation: number): number
┃       ┣━━   private getCurrentRotation(matrix: string): number
┃       ┣━━   ngAfterViewInit()
┃       ┣━━       const mouseEnterListener = () =>
┃       ┣━━       const mouseLeaveListener = () =>
┃       ┣━━   ngOnDestroy()
┃       ┣━━   toggleColumn(event: MatOptionSelectionChange, column: string)
┃       ┣━━           (col) =>
┃       ┣━━   adjustColumns(event?: Event)
┃       ┣━━   onResize(event: Event)
┃       ┣━━   async ngOnInit()
┃       ┣━━   async loadTickets(): Promise<void>
┃       ┣━━   onDateRangeChange(
┃       ┃       type: "start" | "end",
┃       ┃       event: MatDatepickerInputEvent<Date>
┃       ┃     )
┃       ┣━━   applyFilter(column: string): void
┃       ┣━━   formatDateForComparison(date: Date): string
┃       ┣━━   constructor(private renderer: Renderer2)
┃       ┣━━   onFilterChange(event: Event, column: string)
┃       ┣━━   onLatitudeChange(event: Event)
┃       ┣━━   onLongitudeChange(event: Event)
┃       ┣━━   onRadiusChange(event: Event)
┃       ┣━━   sortData(sort: Sort): void
┃       ┣━━   onRowClick(event: Event, row: any)
┃       ┣━━ function isDate(value: Date | undefined | null): value is Date
┃       ┣━━ function isNonNullNumber(value: number | null): value is number
┃       ┣━━ function hasLocation(
┃       ┃     ticket: any
┃       ┃   ): ticket is
┃       ┣━━ const create_faker_ticket = async () =>
┃       ┣━━ function compare(a: number | string, b: number | string, isAsc: boolean)
┃       ┣━━ function compare_dates(a: Date, b: Date, isAsc: boolean)
┃       ┣━━ async function mockMoreTickets(): Promise<Ticket[]>
┃       ┣━━ const mockTickets = async () =>
┃       ┗━━ const renderQRCode = async (text: String): Promise<string> =>
┗━━ 📁 group_lisp (1163 tokens, 139 lines)
    ┣━━ 📄 clojure_test.clj (726 tokens, 86 lines)
    ┃   ┣━━ defprotocol P
    ┃   ┣━━ defrecord Person
    ┃   ┣━━ defn -main
    ┃   ┣━━ ns bion.likes_trees
    ┃   ┣━━ def repo-url
    ┃   ┣━━ defn config
    ┃   ┣━━ defmacro with-os
    ┃   ┗━━ defrecord SetFullElement
    ┣━━ 📄 LispTest.lisp (31 tokens, 6 lines)
    ┃   ┣━━ defstruct person
    ┃   ┗━━ defun greet
    ┣━━ 📄 racket_struct.rkt (17 tokens, 2 lines)
    ┃   ┗━━ struct point
    ┗━━ 📄 test_scheme.scm (389 tokens, 45 lines)
        ┣━━ define topological-sort
        ┣━━   define table
        ┣━━   define queue
        ┣━━   define result
        ┣━━   define set-up
        ┗━━   define traverse

```
<!-- t2-end -->
## Got Globs?

<!-- t3-start -->
```sh
tree_plus -g "*.*s" -i group_todo tests/more_languages
paths=('tests/more_languages',)
📁 more_languages (12112 tokens, 1800 lines)
┣━━ 📁 group1 (1468 tokens, 296 lines)
┃   ┣━━ 📄 test.js (755 tokens, 154 lines)
┃   ┃   ┣━━ class MyClass
┃   ┃   ┣━━   myMethod()
┃   ┃   ┣━━   async asyncMethod(a, b)
┃   ┃   ┣━━   methodWithDefaultParameters(a = 5, b = 10)
┃   ┃   ┣━━   multilineMethod(
┃   ┃   ┃       c,
┃   ┃   ┃       d
┃   ┃   ┃     )
┃   ┃   ┣━━   multilineMethodWithDefaults(
┃   ┃   ┃       t = "tree",
┃   ┃   ┃       p = "plus"
┃   ┃   ┃     )
┃   ┃   ┣━━ function myFunction(param1, param2)
┃   ┃   ┣━━ function multilineFunction(
┃   ┃   ┃     param1,
┃   ┃   ┃     param2
┃   ┃   ┃   )
┃   ┃   ┣━━ const arrowFunction = () =>
┃   ┃   ┣━━ const parametricArrow = (a, b) =>
┃   ┃   ┣━━ function ()
┃   ┃   ┣━━ function outerFunction(outerParam)
┃   ┃   ┣━━   function innerFunction(innerParam)
┃   ┃   ┣━━ const myObject = {
┃   ┃   ┣━━   myMethod: function (stuff)
┃   ┃   ┣━━ let myArrowObject = {
┃   ┃   ┣━━   myArrow: ({
┃   ┃   ┃       a,
┃   ┃   ┃       b,
┃   ┃   ┃       c,
┃   ┃   ┃     }) =>
┃   ┃   ┣━━ const myAsyncArrowFunction = async () =>
┃   ┃   ┣━━ function functionWithRestParameters(...args)
┃   ┃   ┣━━ const namedFunctionExpression = function myNamedFunction()
┃   ┃   ┣━━ const multilineArrowFunction = (
┃   ┃   ┃     a,
┃   ┃   ┃     b
┃   ┃   ┃   ) =>
┃   ┃   ┣━━ function functionReturningFunction()
┃   ┃   ┣━━   return function ()
┃   ┃   ┣━━ function destructuringOnMultipleLines({
┃   ┃   ┃     a,
┃   ┃   ┃     b,
┃   ┃   ┃   })
┃   ┃   ┣━━ const arrowFunctionWithDestructuring = ({ a, b }) =>
┃   ┃   ┣━━ const multilineDestructuringArrow = ({
┃   ┃   ┃     a,
┃   ┃   ┃     b,
┃   ┃   ┃   }) =>
┃   ┃   ┣━━ async function asyncFunctionWithErrorHandling()
┃   ┃   ┣━━ class Car
┃   ┃   ┣━━   constructor(brand)
┃   ┃   ┣━━   present()
┃   ┃   ┣━━ class Model extends Car
┃   ┃   ┣━━   constructor(brand, mod)
┃   ┃   ┗━━   show()
┃   ┗━━ 📄 test.ts (713 tokens, 142 lines)
┃       ┣━━ type MyType
┃       ┣━━ interface MyInterface
┃       ┣━━ class TsClass
┃       ┣━━   myMethod()
┃       ┣━━   myMethodWithArgs(param1: string, param2: number): void
┃       ┣━━   static myStaticMethod<T>(param: T): T
┃       ┣━━   multilineMethod(
┃       ┃       c: number,
┃       ┃       d: number
┃       ┃     ): number
┃       ┣━━   multilineMethodWithDefaults(
┃       ┃       t: string = "tree",
┃       ┃       p: string = "plus"
┃       ┃     ): string
┃       ┣━━ export class AdvancedComponent implements MyInterface
┃       ┣━━   async myAsyncMethod(
┃       ┃       a: string,
┃       ┃       b: number,
┃       ┃       c: string
┃       ┃     ): Promise<void>
┃       ┣━━   genericMethod<T, U>(
┃       ┃       arg1: T,
┃       ┃       arg2: U
┃       ┃     ): [T, U]
┃       ┣━━ export class TicketsComponent implements MyInterface
┃       ┣━━   async myAsyncMethod({ a, b, c }: { a: String; b: Number; c: String })
┃       ┣━━ function tsFunction()
┃       ┣━━ function tsFunctionSigned(
┃       ┃     param1: number,
┃       ┃     param2: number
┃       ┃   ): void
┃       ┣━━ const tsArrowFunctionSigned = ({
┃       ┃     a,
┃       ┃     b,
┃       ┃   }: {
┃       ┃     a: number;
┃       ┃     b: string;
┃       ┃   }) =>
┃       ┣━━ const arrowFunction = () =>
┃       ┣━━ const arrow = (a: String, b: Number) =>
┃       ┣━━ const asyncArrowFunction = async () =>
┃       ┣━━ const asyncArrow = async (a: String, b: Number) =>
┃       ┣━━ let weirdArrow = () =>
┃       ┣━━ const asyncPromiseArrow = async (): Promise<void> =>
┃       ┣━━ let myWeirdArrowSigned = (x: number): number =>
┃       ┣━━ class Person
┃       ┣━━   constructor(private firstName: string, private lastName: string)
┃       ┣━━   getFullName(): string
┃       ┣━━   describe(): string
┃       ┣━━ class Employee extends Person
┃       ┣━━   constructor(
┃       ┃       firstName: string,
┃       ┃       lastName: string,
┃       ┃       private jobTitle: string
┃       ┃     )
┃       ┣━━   describe(): string
┃       ┣━━ interface Shape
┃       ┗━━ interface Square extends Shape
┣━━ 📁 group3 (850 tokens, 147 lines)
┃   ┗━━ 📄 csharp_test.cs (850 tokens, 147 lines)
┃       ┣━━ public interface IExcelTemplate
┃       ┣━━     void LoadTemplate(string templateFilePath)
┃       ┣━━     void LoadData(Dictionary<string, string> data)
┃       ┣━━     void ModifyCell(string cellName, string value)
┃       ┣━━     void SaveToFile(string filePath)
┃       ┣━━ public interface IGreet
┃       ┣━━     void Greet()
┃       ┣━━ public enum WeekDays
┃       ┣━━ public delegate void DisplayMessage(string message)
┃       ┣━━ public struct Address
┃       ┣━━ public static class HelperFunctions
┃       ┣━━     public static void PrintMessage(string message)
┃       ┣━━     public static int AddNumbers(int a, int b)
┃       ┣━━ namespace HelloWorldApp
┃       ┣━━     class Person : IGreet
┃       ┣━━         public Person(string name, int age)
┃       ┣━━         public void Greet()
┃       ┣━━     class HelloWorld
┃       ┣━━         static void Main(string[] args)
┃       ┣━━ namespace TemplateToExcelServer.Template
┃       ┣━━     public interface ITemplateObject
┃       ┣━━         string[,] GetContent()
┃       ┣━━         string[] GetContentArray()
┃       ┣━━         string[] GetFormat()
┃       ┣━━         int? GetFormatLength()
┃       ┣━━         TemplateObject SetContent(string[,] Content)
┃       ┣━━         TemplateObject SetContentArray(string[] value)
┃       ┣━━         TemplateObject SetFormat(string[] Header)
┃       ┣━━         TemplateObject SetNameOfReport(
┃       ┃               ReadOnlyMemory<byte> ReportName,
┃       ┃               int[] EdgeCase)
┃       ┣━━         TemplateObject SetSheetName(ReadOnlyMemory<byte> SheetName)
┃       ┣━━ public class BankAccount(string accountID, string owner)
┃       ┣━━     public override string ToString() =>
┃       ┣━━ var IncrementBy = (int source, int increment = 1) =>
┃       ┣━━ Func<int, int, int> add = (x, y) =>
┃       ┣━━ button.Click += (sender, args) =>
┃       ┣━━ public Func<int, int> GetMultiplier(int factor)
┃       ┣━━ public void Method(
┃       ┃           int param1,
┃       ┃           int param2,
┃       ┃           int param3,
┃       ┃           int param4,
┃       ┃           int param5,
┃       ┃           int param6,
┃       ┃       )
┃       ┣━━ System.Net.ServicePointManager.ServerCertificateValidationCallback +=
┃       ┃       (se, cert, chain, sslerror) =>
┃       ┣━━ class ServerCertificateValidation
┃       ┣━━     public bool OnRemoteCertificateValidation(
┃       ┃           object se,
┃       ┃           X509Certificate cert,
┃       ┃           X509Chain chain,
┃       ┃           SslPolicyErrors sslerror
┃       ┃       )
┃       ┣━━ s_downloadButton.Clicked += async (o, e) =>
┃       ┣━━ [HttpGet, Route("DotNetCount")]
┃       ┗━━ static public async Task<int> GetDotNetCount(string URL)
┣━━ 📁 group4 (1388 tokens, 227 lines)
┃   ┣━━ 📄 haskell_test.hs (373 tokens, 41 lines)
┃   ┃   ┣━━ data Person
┃   ┃   ┣━━ greet :: Person -> String
┃   ┃   ┗━━ resolveVariables ::
┃   ┃         forall m fragments.
┃   ┃         (MonadError QErr m, Traversable fragments) =>
┃   ┃         Options.BackwardsCompatibleNullInNonNullableVariables ->
┃   ┃         [G.VariableDefinition] ->
┃   ┃         GH.VariableValues ->
┃   ┃         [G.Directive G.Name] ->
┃   ┃         G.SelectionSet fragments G.Name ->
┃   ┃         m
┃   ┃           ( [G.Directive Variable],
┃   ┃             G.SelectionSet fragments Variable
┃   ┃           )
┃   ┣━━ 📄 rust_test.rs (888 tokens, 159 lines)
┃   ┃   ┣━━ enum Days
┃   ┃   ┣━━ struct Point
┃   ┃   ┣━━ impl Point
┃   ┃   ┣━━     fn get_origin() -> Point
┃   ┃   ┣━━ struct Person
┃   ┃   ┣━━ impl Person
┃   ┃   ┣━━     fn greet(&self)
┃   ┃   ┣━━ fn add_two_longs(x1: i64, x2: i64) -> i64
┃   ┃   ┣━━ fn add_two_longs_longer(
┃   ┃   ┃       x1: i64,
┃   ┃   ┃       x2: i64,
┃   ┃   ┃   ) -> i64
┃   ┃   ┣━━ fn multiply_by_two(num: f64) -> f64
┃   ┃   ┣━━ fn get_first_character(s: &str) -> Option<char>
┃   ┃   ┣━━ trait Drawable
┃   ┃   ┣━━     fn draw(&self)
┃   ┃   ┣━━ impl Drawable for Point
┃   ┃   ┣━━     fn draw(&self)
┃   ┃   ┣━━ fn main()
┃   ┃   ┣━━ pub struct VisibleStruct
┃   ┃   ┣━━ mod my_module
┃   ┃   ┣━━     pub struct AlsoVisibleStruct<T>(T, T)
┃   ┃   ┣━━ macro_rules! say_hello
┃   ┃   ┣━━ #[macro_export]
┃   ┃   ┃   macro_rules! hello_tree_plus
┃   ┃   ┣━━ pub mod lib
┃   ┃   ┣━━     pub mod interfaces
┃   ┃   ┣━━     mod engine
┃   ┃   ┣━━ pub fn flow<S1, S2, S3, S4, E, T, L>(
┃   ┃   ┃       source: S1, 
┃   ┃   ┃       extractor: E, 
┃   ┃   ┃       inbox: S2, 
┃   ┃   ┃       transformer: T, 
┃   ┃   ┃       outbox: S3, 
┃   ┃   ┃       loader: L, 
┃   ┃   ┃       sink: &mut S4,
┃   ┃   ┃   ) -> Result<(), Box<dyn Error>>
┃   ┃   ┃   where
┃   ┃   ┃       S1: Extractable,
┃   ┃   ┃       S2: Extractable + Loadable,
┃   ┃   ┃       S3: Extractable + Loadable,
┃   ┃   ┃       S4: Loadable,
┃   ┃   ┃       E: Extractor<S1, S2>,
┃   ┃   ┃       T: Transformer<S2, S3>,
┃   ┃   ┃       L: Loader<S3, S4>,
┃   ┃   ┣━━ trait Container
┃   ┃   ┣━━     fn items(&self) -> impl Iterator<Item = Widget>
┃   ┃   ┣━━ trait HttpService
┃   ┃   ┣━━     async fn fetch(&self, url: Url) -> HtmlBody
┃   ┃   ┣━━ struct Pair<T, U>
┃   ┃   ┣━━ trait Transformer<T>
┃   ┃   ┣━━     fn transform(&self, input: T) -> T
┃   ┃   ┣━━ impl<T: std::ops::Add<Output = T> + Copy> Transformer<T> for Pair<T, T>
┃   ┃   ┣━━     fn transform(&self, input: T) -> T
┃   ┃   ┗━━ fn main()
┃   ┗━━ 📄 test_fsharp.fs (127 tokens, 27 lines)
┃       ┣━━ module TestFSharp
┃       ┣━━ type Person = {
┃       ┣━━ let add x y =
┃       ┣━━ let multiply 
┃       ┃       (x: int) 
┃       ┃       (y: int): int =
┃       ┣━━ let complexFunction
┃       ┃       (a: int)
┃       ┃       (b: string)
┃       ┃       (c: float)
┃       ┃       : (int * string) option =
┃       ┗━━ type Result<'T> =
┗━━ 📁 group5 (8406 tokens, 1130 lines)
    ┣━━ 📄 app-routing.module.ts (242 tokens, 28 lines)
    ┃   ┣━━ const routes: Routes = [
    ┃   ┃       { path: '', redirectTo: 'login', pathMatch: 'full' },
    ┃   ┃       { path: '*', redirectTo: 'login' },
    ┃   ┃       { path: 'home', component: HomeComponent },
    ┃   ┃       { path: 'login', component: LoginComponent },
    ┃   ┃       { path: 'register', component: RegisterComponent },
    ┃   ┃       { path: 'events', component: EventsComponent },
    ┃   ┃       { path: 'invites', component: InvitesComponent },
    ┃   ┃       { path: 'rewards', component: RewardsComponent },
    ┃   ┃       { path: 'profile', component: ProfileComponent },
    ┃   ┃   ];
    ┃   ┗━━ export class AppRoutingModule
    ┣━━ 📄 app.component.spec.ts (307 tokens, 48 lines)
    ┃   ┣━━ describe 'AppComponent'
    ┃   ┣━━     it should create the app
    ┃   ┣━━     it should welcome the user
    ┃   ┣━━     it should welcome 'Jimbo'
    ┃   ┗━━     it should request login if not logged in
    ┣━━ 📄 app.component.ts (243 tokens, 45 lines)
    ┃   ┣━━ export class AppComponent
    ┃   ┣━━   constructor(private loginService: LoginService)
    ┃   ┣━━   checkSession()
    ┃   ┣━━   async goToEvent(event_id: string)
    ┃   ┗━━   valInvitedBy(event: any, event_id: string)
    ┣━━ 📄 app.module.ts (269 tokens, 43 lines)
    ┃   ┣━━ @NgModule({
    ┃   ┃       declarations: [
    ┃   ┃           AppComponent,
    ┃   ┃           HomeComponent,
    ┃   ┃           LoginComponent,
    ┃   ┃           RegisterComponent,
    ┃   ┃           EventsComponent,
    ┃   ┃           InvitesComponent,
    ┃   ┃           RewardsComponent,
    ┃   ┃           ProfileComponent
    ┃   ┗━━ export class AppModule
    ┣━━ 📄 environment.test.ts (193 tokens, 19 lines)
    ┃   ┣━━ environment:
    ┃   ┣━━    production
    ┃   ┣━━    cognitoUserPoolId
    ┃   ┣━━    cognitoAppClientId
    ┃   ┗━━    apiurl
    ┣━━ 📄 rust_todo_test.rs (109 tokens, 27 lines)
    ┃   ┣━━ TODO (Line 23): This todo tests parse_todo
    ┃   ┣━━ enum Color
    ┃   ┣━━ struct Point
    ┃   ┣━━ trait Drawable
    ┃   ┣━━     fn draw(&self)
    ┃   ┣━━ impl Drawable for Point
    ┃   ┣━━     fn draw(&self)
    ┃   ┗━━ fn main()
    ┣━━ 📄 standard-app-routing.module.ts (93 tokens, 17 lines)
    ┃   ┗━━ const routes: Routes = [
    ┃         { path: '', component: HomeComponent },
    ┃         {
    ┃           path: 'heroes',
    ┃           component: HeroesListComponent,
    ┃           children: [
    ┃             { path: ':id', component: HeroDetailComponent },
    ┃             { path: 'new', component: HeroFormComponent },
    ┃           ],
    ┃         },
    ┃         { path: '**', component: PageNotFoundComponent },
    ┃       ];
    ┗━━ 📄 tickets.component.ts (6950 tokens, 903 lines)
        ┣━━ interface EnrichedTicket extends Ticket
        ┣━━ interface SpinConfig
        ┣━━ interface RotationState
        ┣━━ interface SpeakInput
        ┣━━ const formatSpeakInput = (input: SpeakInput): string =>
        ┣━━ function hourToSpeech(hour: number, minute: number, period: string): string
        ┣━━ export class TicketsComponent implements AfterViewInit
        ┣━━   speak(input: SpeakInput)
        ┣━━   speakEvent(ticket: EnrichedTicket): void
        ┣━━   formatEvent(ticket: EnrichedTicket): string
        ┣━━   speakVenue(ticket: EnrichedTicket): void
        ┣━━   formatDate(date: Date, oneLiner: boolean = false): string
        ┣━━   formatDateForSpeech(date: Date): string
        ┣━━   async spinQRCode(
        ┃       event: PointerEvent,
        ┃       config: SpinConfig = DEFAULT_SPIN_CONFIG
        ┃     )
        ┣━━   private animateRotation(
        ┃       imgElement: HTMLElement,
        ┃       targetRotation: number,
        ┃       config: SpinConfig,
        ┃       cleanup: () => void
        ┃     )
        ┣━━     const animate = (currentTime: number) =>
        ┣━━   private getNext90Degree(currentRotation: number): number
        ┣━━   private getCurrentRotation(matrix: string): number
        ┣━━   ngAfterViewInit()
        ┣━━       const mouseEnterListener = () =>
        ┣━━       const mouseLeaveListener = () =>
        ┣━━   ngOnDestroy()
        ┣━━   toggleColumn(event: MatOptionSelectionChange, column: string)
        ┣━━           (col) =>
        ┣━━   adjustColumns(event?: Event)
        ┣━━   onResize(event: Event)
        ┣━━   async ngOnInit()
        ┣━━   async loadTickets(): Promise<void>
        ┣━━   onDateRangeChange(
        ┃       type: "start" | "end",
        ┃       event: MatDatepickerInputEvent<Date>
        ┃     )
        ┣━━   applyFilter(column: string): void
        ┣━━   formatDateForComparison(date: Date): string
        ┣━━   constructor(private renderer: Renderer2)
        ┣━━   onFilterChange(event: Event, column: string)
        ┣━━   onLatitudeChange(event: Event)
        ┣━━   onLongitudeChange(event: Event)
        ┣━━   onRadiusChange(event: Event)
        ┣━━   sortData(sort: Sort): void
        ┣━━   onRowClick(event: Event, row: any)
        ┣━━ function isDate(value: Date | undefined | null): value is Date
        ┣━━ function isNonNullNumber(value: number | null): value is number
        ┣━━ function hasLocation(
        ┃     ticket: any
        ┃   ): ticket is
        ┣━━ const create_faker_ticket = async () =>
        ┣━━ function compare(a: number | string, b: number | string, isAsc: boolean)
        ┣━━ function compare_dates(a: Date, b: Date, isAsc: boolean)
        ┣━━ async function mockMoreTickets(): Promise<Ticket[]>
        ┣━━ const mockTickets = async () =>
        ┗━━ const renderQRCode = async (text: String): Promise<string> =>

```
<!-- t3-end -->

## Languages Todo:

Help me **add to** and **priorize** this list of languages to support!

- [Open a `tree_plus` GitHub Issue](https://github.com/bionicles/tree_plus/issues/new), or
- [Contact Bion on X](https://twitter.com/flesheatingemu)

<!-- t4-start -->
```sh
tree_plus tests/more_languages/group_todo
paths=('tests/more_languages/group_todo',)
📁 group_todo (888 tokens, 176 lines)
┣━━ 📄 crystal_test.cr (56 tokens, 15 lines)
┣━━ 📄 dart_test.dart (106 tokens, 24 lines)
┣━━ 📄 elixir_test.exs (49 tokens, 10 lines)
┣━━ 📄 fortran_test.f90 (114 tokens, 21 lines)
┣━━ 📄 nodemon.json (120 tokens, 21 lines)
┣━━ 📄 sas_test.sas (104 tokens, 22 lines)
┣━━ 📄 test_setup_py.test (118 tokens, 24 lines)
┣━━ 📄 testTypings.d.ts (149 tokens, 23 lines)
┗━━ 📄 vba_test.bas (72 tokens, 16 lines)

```
<!-- t4-end -->

## Oppose Unfair Business Practices

Please consider contacting the authorities to report the issue described in this document:

[California OpenAI Complaint - Customer Noncompete Clause](https://www.tinyurl.com/cali-openai-complaint)

_Remember_: **Your Voice Matters!**

## License

MIT or Apache 2.0, at your option.