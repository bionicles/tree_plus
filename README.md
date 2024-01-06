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
📁 tree_plus (4 folders, 22 files) 
┣━━ 📄 .env.test (5 tokens, 1 line)
┃   ┗━━ DEBUG_TREE_PLUS
┣━━ 📁 .github (2 folders, 3 files) 
┃   ┣━━ 📄 dependabot.yml (107 tokens, 11 lines)
┃   ┃   ┗━━ Unsupported YAML Category
┃   ┗━━ 📁 workflows (1 folder, 2 files) 
┃       ┣━━ 📄 microsoft.yml (323 tokens, 40 lines)
┃       ┃   ┣━━ Microsoft
┃       ┃   ┣━━   job: build
┃       ┃   ┣━━     - Set up Python ${{ matrix.python-version }}
┃       ┃   ┣━━     - Install tree_plus
┃       ┃   ┣━━     - Create .env file
┃       ┃   ┣━━     - Set PYTHONUTF8 for Windows
┃       ┃   ┣━━     - Run generic tests
┃       ┃   ┗━━     - Run specific test
┃       ┗━━ 📄 unix.yml (715 tokens, 88 lines)
┃           ┣━━ Linux & MacOS
┃           ┣━━   job: test
┃           ┣━━     - Set up Python ${{ matrix.python-version }}
┃           ┣━━     - Install tree_plus
┃           ┣━━     - Create .env file
┃           ┣━━     - Run generic tests
┃           ┣━━     - Run specific test
┃           ┣━━   job: deploy
┃           ┣━━     - Set up Python
┃           ┣━━     - Install dependencies
┃           ┣━━     - Increment Version
┃           ┣━━     - Build
┃           ┣━━     - Install
┃           ┣━━     - Test
┃           ┣━━     - Update README
┃           ┣━━     - Build Again
┃           ┣━━     - Commit Updates
┃           ┗━━     - Publish to PyPI
┣━━ 📄 .gitignore (210 tokens, 50 lines)
┣━━ 📄 LICENSE (2123 tokens, 81 lines)
┣━━ 📄 Makefile (562 tokens, 96 lines)
┃   ┣━━ SHELL := /bin/bash
┃   ┣━━ cli
┃   ┣━━ library_demo
┃   ┣━━ debug
┃   ┣━━ .PHONY: debug_command
┃   ┣━━ debug_command: test_parallel test_tp_dotdot test_e2e test_deploy test_cli
┃   ┣━━ test_sequential
┃   ┣━━ test_parallel
┃   ┣━━ test_units
┃   ┣━━ test_more_languages
┃   ┣━━ test: test_normally test_tp_dotdot test_e2e test_cli test_deploy
┃   ┣━━ test_engine
┃   ┣━━ test_normally
┃   ┣━━ test_tp_dotdot
┃   ┣━━ test_e2e
┃   ┣━━ test_cli: cli
┃   ┣━━ test_deploy
┃   ┣━━ vulture: install_vulture
┃   ┣━━ install_vulture
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
┣━━ 📄 nodemon.json (129 tokens, 24 lines)
┣━━ 📄 pyproject.toml (347 tokens, 41 lines)
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
┃   ┣━━     tomli
┃   ┗━━     natsort>=7.1
┣━━ 📄 pytest.ini (21 tokens, 5 lines)
┣━━ 📄 README.md (37200 tokens, 2517 lines)
┃   ┣━━ # Tree Plus
┃   ┣━━ ## Example Output:
┃   ┣━━ - [ ] Demonstrate Parsed Checkboxes
┃   ┣━━ ## Start Quick!
┃   ┣━━ ### Prerequisites
┃   ┣━━ ### Install Tree Plus
┃   ┣━━ #### PyPI
┃   ┣━━ #### Local Hackable Install
┃   ┣━━ ### Alias Usage
┃   ┣━━ ## Library Usage:
┃   ┣━━ ## Moar Languages
┃   ┣━━ ## Got Globs?
┃   ┣━━ ## Languages Todo:
┃   ┣━━ ## Oppose Unfair Business Practices
┃   ┗━━ ## License
┣━━ 📄 tree_plus_cli.py (973 tokens, 161 lines)
┃   ┗━━ def main(
┃           glob: Optional[Tuple[str]],
┃           paths: Optional[Union[str, Tuple[str]]],
┃           ignore: Tuple[str],
┃           override: bool,
┃           debug: bool,
┃           version: bool,
┃           syntax: bool,
┃           concise: bool,
┃       )
┣━━ 📁 tree_plus_programs (1 folder, 1 file) 
┃   ┗━━ 📄 hello_tree_plus.py (528 tokens, 80 lines)
┃       ┣━━ class ItHappened
┃       ┣━━ def main()
┃       ┗━━ def trees(
┃               or_: bool,
┃               it: int,
┃               didnt: str,
┃               happen: tuple,
┃           ) -> ItHappened
┗━━ 📁 tree_plus_src (2 folders, 9 files) 
    ┣━━ 📄 count_tokens_lines.py (805 tokens, 139 lines)
    ┃   ┣━━ TODO (Line 14): show off how well we parse_todo!
    ┃   ┣━━ @dataclass(frozen=True)
    ┃   ┃   class TokenLineCount
    ┃   ┣━━ def count_tokens_lines(file_path: Union[str, Path]) -> Optional[TokenLineCount]
    ┃   ┗━━ def add_tokens_lines(
    ┃           lhs_count: TokenLineCount, rhs_count: TokenLineCount
    ┃       ) -> TokenLineCount
    ┣━━ 📄 debug.py (167 tokens, 39 lines)
    ┃   ┣━━ def disable_debug()
    ┃   ┣━━ @lru_cache
    ┃   ┃   def debug_enabled()
    ┃   ┣━━ def debug_print(*args, **kwargs)
    ┃   ┣━━ def enable_debug()
    ┃   ┗━━ @contextmanager
    ┃       def debug_disabled()
    ┣━━ 📄 deploy.py (1771 tokens, 210 lines)
    ┃   ┣━━ def extract(path: str = None) -> str
    ┃   ┣━━ def load(content: str = None, path: str = None)
    ┃   ┣━━ def extract_version(source_path: str = None) -> Tuple[int, int, int]
    ┃   ┣━━ def increment_version(
    ┃   ┃       source_path: str = None,
    ┃   ┃       sink_path: str = None,
    ┃   ┃   )
    ┃   ┣━━ def run_command(command: str = None, debug: bool = False)
    ┃   ┣━━ def replace_readme_section(
    ┃   ┃       source_path: str = None,
    ┃   ┃       sink_path: str = None,
    ┃   ┃       marker: str = None,
    ┃   ┃       command: str = None,
    ┃   ┃   )
    ┃   ┣━━ def update_readme(source_path: str = None, sink_path: str = None)
    ┃   ┗━━ def main()
    ┣━━ 📄 engine.py (6024 tokens, 718 lines)
    ┃   ┣━━ TODO (Line 40): MOVE TIMEOUT_SECONDS TO ENV VAR & CLI INPUT
    ┃   ┣━━ TODO (Line 388): research & decide about globs as paths instead of as filters
    ┃   ┣━━ NOTE (Line 422): here we add directly input file_paths to the amortized glob matches
    ┃   ┣━━ TODO (Line 495): decide if we apply glob patterns to glob paths (currently NO)
    ┃   ┣━━ TODO (Line 534): decide between glob and rglob in _from_glob
    ┃   ┣━━ TODO (Line 541): decide if we need to re-amortize the globs in the glob seed
    ┃   ┣━━ TODO (Line 543): clarify ignore in glob seed context, skipping for now
    ┃   ┣━━ class Category(Enum)
    ┃   ┣━━ @dataclass
    ┃   ┃   class TreePlus
    ┃   ┣━━     def is_root(self) -> bool
    ┃   ┣━━     def is_folder(self) -> bool
    ┃   ┣━━     def is_file(self) -> bool
    ┃   ┣━━     def is_glob(self) -> bool
    ┃   ┣━━     def is_component(self) -> bool
    ┃   ┣━━     def into_rich_tree(self) -> Tree
    ┃   ┣━━     def into_str(self) -> str
    ┃   ┣━━     def render(self)
    ┃   ┣━━     def stats(self) -> str
    ┃   ┣━━ @lru_cache
    ┃   ┃   def remove_trailing_space(x: str) -> str
    ┃   ┣━━ def tree_to_string(tree: Tree) -> str
    ┃   ┣━━ def clean_string(input_str: str) -> str
    ┃   ┣━━ def safe_print(
    ┃   ┃       tree: Tree,
    ┃   ┃       style: Optional[str] = None,
    ┃   ┃       highlight: bool = True,
    ┃   ┃       markup: bool = False,
    ┃   ┃   )
    ┃   ┣━━ def _make_rich_tree(label: str) -> Tree
    ┃   ┣━━ def into_rich_tree(*, root: TreePlus = None) -> Tree
    ┃   ┣━━ @lru_cache
    ┃   ┃   def categorize(
    ┃   ┃       x: Union[Path, Tuple[str], str],
    ┃   ┃       check_strs_globs: bool = True,
    ┃   ┃       check_strs_paths: bool = True,
    ┃   ┃       raise_if_component: bool = True,
    ┃   ┃   ) -> Category
    ┃   ┣━━ def from_seed(
    ┃   ┃       maybe_seed_str=None,
    ┃   ┃       *,
    ┃   ┃       maybe_ignore: Optional[Tuple[str]] = DEFAULT_IGNORE,
    ┃   ┃       maybe_globs: Optional[Tuple[str]] = None,
    ┃   ┃       syntax_highlighting: bool = False,
    ┃   ┃       override_ignore: bool = False,
    ┃   ┃       concise: bool = False,
    ┃   ┃   ) -> TreePlus
    ┃   ┣━━ def from_seeds(
    ┃   ┃       maybe_seed_strs: Optional[Tuple[str]] = None,
    ┃   ┃       *,
    ┃   ┃       maybe_ignore: Optional[Tuple[str]] = DEFAULT_IGNORE,
    ┃   ┃       maybe_globs: Optional[Tuple[str]] = None,
    ┃   ┃       syntax_highlighting: bool = False,
    ┃   ┃       override_ignore: bool = False,
    ┃   ┃       concise: bool = False,
    ┃   ┃   ) -> TreePlus
    ┃   ┣━━ def _reduce_forest(*, forest: Tuple[TreePlus] = None) -> TreePlus
    ┃   ┣━━ def _map_seeds(
    ┃   ┃       *,
    ┃   ┃       seeds: Tuple[str] = None,
    ┃   ┃       maybe_ignore: Optional[Tuple[str]] = DEFAULT_IGNORE,
    ┃   ┃       maybe_globs: Optional[Tuple[str]] = None,
    ┃   ┃       syntax_highlighting: bool = False,
    ┃   ┃       concise: bool = False,
    ┃   ┃   ) -> Tuple[TreePlus]
    ┃   ┣━━ def _from_seed(
    ┃   ┃       *,
    ┃   ┃       seed_path: Optional[Path] = None,
    ┃   ┃       maybe_ignore: Optional[Tuple[str]] = DEFAULT_IGNORE,
    ┃   ┃       maybe_globs: Optional[AmortizedGlobs] = None,
    ┃   ┃       syntax_highlighting: bool = False,
    ┃   ┃       concise: bool = False,
    ┃   ┃   ) -> TreePlus
    ┃   ┣━━ def _add_subtree(
    ┃   ┃       *,
    ┃   ┃       root: TreePlus = None,
    ┃   ┃       subtree: TreePlus = None,
    ┃   ┃   )
    ┃   ┣━━ def _from_glob(
    ┃   ┃       *,
    ┃   ┃       pattern: str,
    ┃   ┃       maybe_ignore: Optional[Tuple[str]] = DEFAULT_IGNORE,
    ┃   ┃       maybe_globs: Optional[AmortizedGlobs] = None,
    ┃   ┃       syntax_highlighting: bool = False,
    ┃   ┃       concise: bool = False,
    ┃   ┃   ) -> TreePlus
    ┃   ┣━━ def _from_folder(
    ┃   ┃       *,
    ┃   ┃       folder_path: Path,
    ┃   ┃       maybe_ignore: Optional[Tuple[str]] = DEFAULT_IGNORE,
    ┃   ┃       maybe_globs: Optional[AmortizedGlobs] = None,
    ┃   ┃       syntax_highlighting: bool = False,
    ┃   ┃       concise: bool = False,
    ┃   ┃   ) -> TreePlus
    ┃   ┣━━ def _from_file(
    ┃   ┃       *,
    ┃   ┃       file_path: Path,
    ┃   ┃       syntax_highlighting: bool = False,
    ┃   ┃       concise: bool = False,
    ┃   ┃   ) -> TreePlus
    ┃   ┣━━ def _get_lexer(file_path: Path) -> str
    ┃   ┗━━ def _syntax_highlight(
    ┃           *,
    ┃           file_path: Path = None,
    ┃           components: List[str] = None,
    ┃       ) -> Union[Syntax, str]
    ┣━━ 📄 ignore.py (2335 tokens, 329 lines)
    ┃   ┣━━ TODO (Line 129): incorporate gitignore
    ┃   ┣━━ def _is_all_str(x: Any) -> bool
    ┃   ┣━━ def can_parse(x) -> bool
    ┃   ┣━━ @lru_cache
    ┃   ┃   def parse_ignore(
    ┃   ┃       maybe_ignore_tuple: Optional[Tuple[str]] = None, override: bool = False
    ┃   ┃   ) -> Optional[Tuple[str]]
    ┃   ┣━━ @lru_cache
    ┃   ┃   def is_glob(x: str) -> bool
    ┃   ┣━━ @lru_cache
    ┃   ┃   def parse_globs(
    ┃   ┃       maybe_globs_tuple: Optional[Tuple[str]] = None,
    ┃   ┃   ) -> Tuple[str]
    ┃   ┣━━ @dataclass(frozen=True)
    ┃   ┃   class AmortizedGlobs
    ┃   ┣━━ def amortize_globs(paths: Tuple[Path], globs: Tuple[str]) -> Optional[AmortizedGlobs]
    ┃   ┗━━ @lru_cache(maxsize=None)
    ┃       def should_ignore(
    ┃           path: Path,
    ┃           ignore: Optional[Tuple[str]] = DEFAULT_IGNORE,
    ┃           globs: Optional[AmortizedGlobs] = None,
    ┃       ) -> bool
    ┣━━ 📄 isabelle_symbols.py (4084 tokens, 462 lines)
    ┃   ┣━━ @lru_cache
    ┃   ┃   def _replace_symbol(match: re.Match) -> str
    ┃   ┗━━ def replace_isabelle_symbols(content: str) -> str
    ┣━━ 📄 parse_file.py (21447 tokens, 2329 lines)
    ┃   ┣━━ BUG (Line 595): catastrophic backtracking in some c files
    ┃   ┣━━ @lru_cache(maxsize=None)
    ┃   ┃   def read_file(
    ┃   ┃       file_path: str,
    ┃   ┃       raise_exceptions: bool = False,
    ┃   ┃       n_lines: Optional[int] = None,
    ┃   ┃   ) -> str
    ┃   ┣━━ def parse_file(file_path: Union[str, Path]) -> List[str]
    ┃   ┣━━ def extract_and_debug_print_groups(match: re.Match, named_only: bool = False) -> dict
    ┃   ┣━━ def parse_rst(contents: str) -> List[str]
    ┃   ┣━━ def parse_c(contents: str) -> List[str]
    ┃   ┣━━ @lru_cache
    ┃   ┃   def is_binary_string(data: bytes) -> bool
    ┃   ┣━━ @lru_cache
    ┃   ┃   def is_binary(file_path: str) -> bool
    ┃   ┣━━ def clean_isabelle_text(content: str) -> str
    ┃   ┣━━ def parse_isabelle(contents: str) -> List[str]
    ┃   ┣━━ def parse_fortran(contents: str) -> List[str]
    ┃   ┣━━ def remove_c_comments(contents: str) -> str
    ┃   ┣━━ def parse_ts(contents: str) -> List[str]
    ┃   ┣━━ def remove_py_comments(input_string: str) -> str
    ┃   ┣━━ def parse_py(contents: str) -> List[str]
    ┃   ┣━━ def parse_rb(contents: str) -> List[str]
    ┃   ┣━━ def parse_fsharp(contents: str) -> List[str]
    ┃   ┣━━ def parse_tcl(contents: str) -> List[str]
    ┃   ┣━━ def parse_erl(contents: str) -> List[str]
    ┃   ┣━━ def parse_rs(contents: str) -> List[str]
    ┃   ┣━━ def parse_csv(contents: str, max_leaves=11) -> List[str]
    ┃   ┣━━ def parse_mathematica(contents: str) -> List[str]
    ┃   ┣━━ def parse_r(contents: str) -> List[str]
    ┃   ┣━━ def parse_zig(contents: str) -> List[str]
    ┃   ┣━━ def parse_hs(contents: str) -> List[str]
    ┃   ┣━━ def parse_lisp(content: str) -> List[str]
    ┃   ┣━━ def parse_capnp(contents: str) -> List[str]
    ┃   ┣━━ def parse_grpc(contents: str) -> List[str]
    ┃   ┣━━ def parse_openrpc_json(contents: str) -> List[str]
    ┃   ┣━━ def parse_json_rpc(contents: str) -> List[str]
    ┃   ┣━━ def parse_graphql(contents: str) -> List[str]
    ┃   ┣━━ def format_dependency(name, details)
    ┃   ┣━━ def parse_cargo_toml(contents: str) -> List[str]
    ┃   ┣━━ def parse_pyproject_toml(contents: str) -> List[str]
    ┃   ┣━━ def parse_lean(lean_content: str) -> List[str]
    ┃   ┣━━ def parse_cs(contents: str) -> List[str]
    ┃   ┣━━ def parse_tex(tex_content: str) -> List[str]
    ┃   ┣━━ def parse_go(contents: str) -> List[str]
    ┃   ┣━━ def parse_swift(contents: str) -> List[str]
    ┃   ┣━━ def parse_bash(contents: str) -> List[str]
    ┃   ┣━━ def parse_d_dot_ts(contents: str) -> List[str]
    ┃   ┣━━ def parse_angular_app_module(contents: str) -> List[str]
    ┃   ┣━━ def parse_angular_routes(content: str) -> List[str]
    ┃   ┣━━ def parse_angular_spec(content: str) -> List[str]
    ┃   ┣━━ def parse_environment_ts(contents: str) -> List[str]
    ┃   ┣━━ def parse_dot_env(contents: str) -> List[str]
    ┃   ┣━━ def parse_requirements_txt(contents: str) -> List[str]
    ┃   ┣━━ def parse_json_schema(contents: str) -> List[str]
    ┃   ┣━━ def parse_package_json(contents: str) -> List[str]
    ┃   ┣━━ def parse_makefile(contents: str) -> List[str]
    ┃   ┣━━ def parse_sql(contents: str) -> List[str]
    ┃   ┣━━ def is_openapi_yml(ymls: Tuple[dict]) -> bool
    ┃   ┣━━ def is_k8s_yml(ymls: Tuple[dict]) -> bool
    ┃   ┣━━ def is_ansible_yml(ymls: Tuple[dict]) -> bool
    ┃   ┣━━ def is_github_yml(ymls: Tuple[dict]) -> bool
    ┃   ┣━━ def parse_github_yml(ymls: Tuple[dict]) -> List[str]
    ┃   ┣━━ def parse_k8s(ymls: Tuple[dict]) -> List[str]
    ┃   ┣━━ def parse_ansible(ymls: Tuple[dict]) -> List[str]
    ┃   ┣━━ def parse_openapi_yml(ymls: Tuple[dict]) -> List[str]
    ┃   ┣━━ def parse_yml(contents: str) -> List[str]
    ┃   ┣━━ def parse_db(db_path: str) -> List[str]
    ┃   ┣━━ def dedent_components(components: List[str]) -> List[str]
    ┃   ┣━━ def parse_cbl(content: str) -> List[str]
    ┃   ┣━━ def parse_java(contents: str) -> List[str]
    ┃   ┣━━ def parse_jl(contents: str) -> List[str]
    ┃   ┣━━ def parse_kt(contents: str) -> List[str]
    ┃   ┣━━ def parse_lua(content: str) -> List[str]
    ┃   ┣━━ def parse_objective_c(content: str) -> List[str]
    ┃   ┣━━ def parse_ocaml(content: str) -> List[str]
    ┃   ┣━━ def parse_apl(content: str) -> List[str]
    ┃   ┣━━ def parse_perl(content: str) -> List[str]
    ┃   ┣━━ def parse_php(content: str) -> List[str]
    ┃   ┣━━ def parse_ps1(contents: str) -> List[str]
    ┃   ┣━━ def parse_matlab(content: str) -> List[str]
    ┃   ┣━━ def parse_scala(contents: str) -> List[str]
    ┃   ┣━━ def parse_tf(contents: str) -> List[str]
    ┃   ┣━━ def parse_md(content: str) -> List[str]
    ┃   ┣━━ def parse_txt(content: str) -> List[str]
    ┃   ┗━━ def parse_markers(content: str) -> List[str]
    ┣━━ 📁 scripts (1 folder, 1 file) 
    ┃   ┗━━ 📄 alias_tree_plus.sh (277 tokens, 31 lines)
    ┃       ┣━━ add_alias()
    ┃       ┗━━ create_conda_env()
    ┗━━ 📄 version.py (18 tokens, 2 lines)
        ┗━━ __version__ = "1.0.25"

tree_plus v(1.0.25) ignore=('tests',) globs=() syntax=False paths=()
4 folder(s), 22 file(s), 7,454 line(s), 80,171 token(s) in 0.22 second(s).

```
<!-- t1-end -->
- [x] Demonstrate Parsed Checkboxes

Here's how `tree_plus --help` looks (`-h` and `-H` both also work) 
<!-- t5-start -->
```sh
tree_plus -h
Usage: tree_plus [OPTIONS] [PATHS]...

  A `tree` util enhanced with tokens, lines, and components.

  Wrap patterns in quotes: -i "*.py" / -g "*.rs"

  Examples:

          Show tree_plus_src and tests simultaneously
              > tree_plus tree_plus_src tests

          Show files matching "*.*s" within tests/more_languages
              > tree_plus -g "*.*s" tests/more_languages

          Ignore Java files
              > tree_plus -i "*.java" tests

          Override DEFAULT_IGNORE: Only ignore .ini files.
              > tree_plus -o -i "*.ini" tests/dot_dot

          Syntax Highlight python files in src and tests
              > tree_plus -s tree_plus_src/*.py tests/*.py

          Concise Mode (No Parsing)
              > tree_plus -c

Options:
  -i, -I, --ignore TEXT  Patterns to ignore, in quotes: -i "*.java"
  -o, -O, --override     Override DEFAULT_IGNORE (includes ignored content):
                         -o -i "*.java"
  -g, -G, --glob TEXT    Patterns to find, in quotes: -g "*.rs"
  -v, -V, --version      Print the version and exit.
  -d, -D, --debug        Enables $DEBUG_TREE_PLUS.
  -s, -S, --syntax       Enables Syntax Highlighting (WIP).
  -c, -C, --concise      Enables Syntax Highlighting (WIP).
  -h, -H, --help         Show this message and exit.

  v(1.0.25) --- https://github.com/bionicles/tree_plus/blob/main/README.md

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

## Library Usage:

Here's how to import the library
```py
import tree_plus_src as tp
```

Check out the **tree_plus_programs** folder for a library usage example.
(More to come!) 

To run the library usage example (make sure you've installed `tree_plus`!)
```sh
make library_demo
```

## Moar Languages

<!-- t2-start -->
```sh
tree_plus -i group_todo tests/more_languages
📁 more_languages (8 folders, 74 files) 
┣━━ 📁 group1 (1 folder, 11 files) 
┃   ┣━━ 📄 addamt.cobol (408 tokens, 41 lines)
┃   ┃   ┣━━ IDENTIFICATION DIVISION.
┃   ┃   ┣━━ PROGRAM-ID.
┃   ┃   ┃              ADDAMT.
┃   ┃   ┣━━ DATA DIVISION.
┃   ┃   ┣━━ WORKING-STORAGE SECTION.
┃   ┃   ┣━━ 01  KEYED-INPUT.
┃   ┃   ┣━━     05  CUST-NO-IN.
┃   ┃   ┣━━     05  AMT1-IN.
┃   ┃   ┣━━     05  AMT2-IN.
┃   ┃   ┣━━     05  AMT3-IN.
┃   ┃   ┣━━ 01  DISPLAYED-OUTPUT.
┃   ┃   ┣━━     05  CUST-NO-OUT.
┃   ┃   ┣━━     05  TOTAL-OUT.
┃   ┃   ┣━━ 01  MORE-DATA.
┃   ┃   ┣━━ PROCEDURE DIVISION.
┃   ┃   ┗━━ 100-MAIN.
┃   ┣━━ 📄 CUSTOMER-INVOICE.CBL (547 tokens, 60 lines)
┃   ┃   ┣━━ IDENTIFICATION DIVISION.
┃   ┃   ┣━━ PROGRAM-ID. CUSTOMER-INVOICE.
┃   ┃   ┣━━ AUTHOR. JANE DOE.
┃   ┃   ┣━━ DATE. 2023-12-30.
┃   ┃   ┣━━   DATE-COMPILED. 06/30/10.
┃   ┃   ┣━━     DATE-WRITTEN. 12/34/56.
┃   ┃   ┣━━ ENVIRONMENT DIVISION.
┃   ┃   ┣━━ INPUT-OUTPUT SECTION.
┃   ┃   ┣━━ FILE-CONTROL.
┃   ┃   ┣━━     SELECT CUSTOMER-FILE.
┃   ┃   ┣━━     SELECT INVOICE-FILE.
┃   ┃   ┣━━     SELECT REPORT-FILE.
┃   ┃   ┣━━ DATA DIVISION.
┃   ┃   ┣━━ FILE SECTION.
┃   ┃   ┣━━ FD CUSTOMER-FILE.
┃   ┃   ┣━━ 01 CUSTOMER-RECORD.
┃   ┃   ┣━━    05 CUSTOMER-ID.
┃   ┃   ┣━━    05 CUSTOMER-NAME.
┃   ┃   ┣━━    05 CUSTOMER-BALANCE.
┃   ┃   ┣━━ FD INVOICE-FILE.
┃   ┃   ┣━━ 01 INVOICE-RECORD.
┃   ┃   ┣━━    05 INVOICE-ID.
┃   ┃   ┣━━    05 CUSTOMER-ID.
┃   ┃   ┣━━    05 INVOICE-AMOUNT.
┃   ┃   ┣━━ FD REPORT-FILE.
┃   ┃   ┣━━ 01 REPORT-RECORD.
┃   ┃   ┣━━ WORKING-STORAGE SECTION.
┃   ┃   ┣━━ 01 WS-CUSTOMER-FOUND.
┃   ┃   ┣━━ 01 WS-END-OF-FILE.
┃   ┃   ┣━━ 01 WS-TOTAL-BALANCE.
┃   ┃   ┣━━ PROCEDURE DIVISION.
┃   ┃   ┣━━ 0000-MAIN-ROUTINE.
┃   ┃   ┣━━ 1000-PROCESS-RECORDS.
┃   ┃   ┣━━ 1100-UPDATE-CUSTOMER-BALANCE.
┃   ┃   ┗━━ END PROGRAM CUSTOMER-INVOICE.
┃   ┣━━ 📄 JavaTest.java (470 tokens, 87 lines)
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
┃   ┣━━ 📄 JuliaTest.jl (482 tokens, 63 lines)
┃   ┃   ┣━━ module JuliaTest_EdgeCase
┃   ┃   ┣━━ struct Location
┃   ┃   ┃       name::String 
┃   ┃   ┃       lat::Float32
┃   ┃   ┃       lon::Float32
┃   ┃   ┃   end
┃   ┃   ┣━━ mutable struct mPerson
┃   ┃   ┃       name::String
┃   ┃   ┃       age::Int
┃   ┃   ┃   end
┃   ┃   ┣━━ Base.@kwdef mutable struct Param
┃   ┃   ┃       Δt::Float64 = 0.1
┃   ┃   ┃       n::Int64
┃   ┃   ┃       m::Int64
┃   ┃   ┃   end
┃   ┃   ┣━━     sic(x,y)
┃   ┃   ┣━━ welcome(l::Location)
┃   ┃   ┣━━ ∑(α, Ω)
┃   ┃   ┣━━ function noob()
┃   ┃   ┃   end
┃   ┃   ┣━━ function ye_olde(hello::String, world::Location)
┃   ┃   ┃   end
┃   ┃   ┣━━ function multiline_greet(
┃   ┃   ┃           p::mPerson, 
┃   ┃   ┃           greeting::String
┃   ┃   ┃       )
┃   ┃   ┃   end
┃   ┃   ┣━━ function julia_is_awesome(prob::DiffEqBase.AbstractDAEProblem{uType, duType, tType,
┃   ┃   ┃           isinplace};
┃   ┃   ┃       kwargs...) where {uType, duType, tType, isinplace}
┃   ┃   ┃   end
┃   ┃   ┗━━ end
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
┃   ┣━━ 📄 lesson.cbl (682 tokens, 79 lines)
┃   ┃   ┣━━ IDENTIFICATION DIVISION.
┃   ┃   ┣━━ PROGRAM-ID.    CBL0002.
┃   ┃   ┣━━ AUTHOR.        Otto B. Fun.
┃   ┃   ┣━━ ENVIRONMENT DIVISION.
┃   ┃   ┣━━ INPUT-OUTPUT SECTION.
┃   ┃   ┣━━ FILE-CONTROL.
┃   ┃   ┣━━     SELECT PRINT-LINE.
┃   ┃   ┣━━     SELECT ACCT-REC.
┃   ┃   ┣━━ DATA DIVISION.
┃   ┃   ┣━━ FILE SECTION.
┃   ┃   ┣━━ FD  PRINT-LINE.
┃   ┃   ┣━━ 01  PRINT-REC.
┃   ┃   ┣━━     05  ACCT-NO-O.
┃   ┃   ┣━━     05  ACCT-LIMIT-O.
┃   ┃   ┣━━     05  ACCT-BALANCE-O.
┃   ┃   ┣━━     05  LAST-NAME-O.
┃   ┃   ┣━━     05  FIRST-NAME-O.
┃   ┃   ┣━━     05  COMMENTS-O.
┃   ┃   ┣━━ FD  ACCT-REC.
┃   ┃   ┣━━ 01  ACCT-FIELDS.
┃   ┃   ┣━━     05  ACCT-NO.
┃   ┃   ┣━━     05  ACCT-LIMIT.
┃   ┃   ┣━━     05  ACCT-BALANCE.
┃   ┃   ┣━━     05  LAST-NAME.
┃   ┃   ┣━━     05  FIRST-NAME.
┃   ┃   ┣━━     05  CLIENT-ADDR.
┃   ┃   ┣━━         10  STREET-ADDR.
┃   ┃   ┣━━         10  CITY-COUNTY.
┃   ┃   ┣━━         10  USA-STATE.
┃   ┃   ┣━━     05  RESERVED.
┃   ┃   ┣━━     05  COMMENTS.
┃   ┃   ┣━━ WORKING-STORAGE SECTION.
┃   ┃   ┣━━ 01 FLAGS.
┃   ┃   ┣━━   05 LASTREC.
┃   ┃   ┣━━ PROCEDURE DIVISION.
┃   ┃   ┣━━ OPEN-FILES.
┃   ┃   ┣━━ READ-NEXT-RECORD.
┃   ┃   ┣━━ CLOSE-STOP.
┃   ┃   ┣━━ READ-RECORD.
┃   ┃   ┗━━ WRITE-RECORD.
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
┃   ┗━━ 📄 test.ts (871 tokens, 166 lines)
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
┃       ┣━━ export default async function tsFunctionComplicated<A, B, C>({
┃       ┃     a = 1 | 2,
┃       ┃     b = "bob",
┃       ┃     c = async () => "charlie",
┃       ┃   }: {
┃       ┃     a: number;
┃       ┃     b: string;
┃       ┃     c: () => Promise<string>;
┃       ┃   }): Promise<string>
┃       ┣━━ const tsArrowFunctionSigned = ({
┃       ┃     a,
┃       ┃     b,
┃       ┃   }: {
┃       ┃     a: number;
┃       ┃     b: string;
┃       ┃   }) =>
┃       ┣━━ export const tsComplicatedArrow = async ({
┃       ┃     a = 1 | 2,
┃       ┃     b = "bob",
┃       ┃     c = async () => "charlie",
┃       ┃   }: {
┃       ┃     a: number;
┃       ┃     b: string;
┃       ┃     c: () => Promise<string>;
┃       ┃   }): Promise<string> =>
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
┣━━ 📁 group2 (1 folder, 8 files) 
┃   ┣━━ 📄 apl_test.apl (44 tokens, 5 lines)
┃   ┃   ┣━━ :Namespace HelloWorld
┃   ┃   ┣━━ :Namespace HelloWorld -> hello ← 'Hello, World!'
┃   ┃   ┗━━ :Namespace HelloWorld -> plus ← {⍺+⍵}
┃   ┣━━ 📄 c_test.c (886 tokens, 142 lines)
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
┃   ┃   ┣━━     while((ln = listNext(&li)))
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
┃   ┃   ┃     param3 map[string]interface{},
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
┃   ┣━━ 📄 PowershellTest.ps1 (523 tokens, 89 lines)
┃   ┃   ┣━━ function Say-Nothing()
┃   ┃   ┣━━ class Person
┃   ┃   ┣━━     Person([string]$name)
┃   ┃   ┣━━     [string]Greet()
┃   ┃   ┣━━     [string]GreetMany([int]$times)
┃   ┃   ┣━━     [string]GreetWithDetails([string]$greeting, [int]$times)
┃   ┃   ┣━━     [string]GreetMultiline(
┃   ┃   ┃           [string]$greeting,
┃   ┃   ┃           [int]$times
┃   ┃   ┃       )
┃   ┃   ┣━━     NoReturn([int]$times)
┃   ┃   ┣━━     NoReturnNoArgs()
┃   ┃   ┣━━ function Say-Hello([Person]$person)
┃   ┃   ┣━━ function Multi-Hello([Person]$personA, [Person]$personB)
┃   ┃   ┣━━ function Switch-Item
┃   ┃   ┣━━   param ([switch]$on)
┃   ┃   ┣━━ function Get-SmallFiles
┃   ┃   ┣━━   param (
┃   ┃   ┃         [PSDefaultValue(Help = '100')]
┃   ┃   ┃         $Size = 100)
┃   ┃   ┣━━ function Get-User
┃   ┃   ┣━━   [CmdletBinding(DefaultParameterSetName="ID")]
┃   ┃   ┣━━   [OutputType("System.Int32", ParameterSetName="ID")]
┃   ┃   ┣━━   [OutputType([String], ParameterSetName="Name")]
┃   ┃   ┣━━   Param (
┃   ┃   ┃       [parameter(Mandatory=$true, ParameterSetName="ID")]
┃   ┃   ┃       [Int[]]
┃   ┃   ┃       $UserID,
┃   ┃   ┃       [parameter(Mandatory=$true, ParameterSetName="Name")]
┃   ┃   ┃       [String[]]
┃   ┃   ┃       $UserName)
┃   ┃   ┣━━ filter Get-ErrorLog ([switch]$Message)
┃   ┃   ┗━━ function global:MultilineSignature(
┃   ┃         [string]$param1,
┃   ┃         [int]$param2,
┃   ┃         [Parameter(Mandatory=$true)]
┃   ┃         [string]$param3
┃   ┃       )
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
┃   ┗━━ 📄 test.csv
┃       ┣━━ Name
┃       ┣━━ Age
┃       ┣━━ Country
┃       ┣━━ City
┃       ┗━━ Email
┣━━ 📁 group3 (1 folder, 16 files) 
┃   ┣━━ 📄 bash_test.sh (154 tokens, 23 lines)
┃   ┃   ┣━━ echo_hello_world()
┃   ┃   ┣━━ function fun_echo_hello_world()
┃   ┃   ┣━━ export SECRET
┃   ┃   ┣━━ alias md='make debug'
┃   ┃   ┣━━ add_alias()
┃   ┃   ┗━━ create_conda_env()
┃   ┣━━ 📄 cpp_test.cpp (1737 tokens, 259 lines)
┃   ┃   ┣━━ class Person
┃   ┃   ┣━━ public:
┃   ┃   ┣━━     Person(std::string n) : name(n)
┃   ┃   ┣━━     void greet()
┃   ┃   ┣━━ void globalGreet()
┃   ┃   ┣━━ int main()
┃   ┃   ┣━━ void printMessage(const std::string &message)
┃   ┃   ┣━━ template<typename T>
┃   ┃   ┣━━ void printVector(const std::vector<T>& vec)
┃   ┃   ┣━━ struct Point
┃   ┃   ┣━━     Point(int x, int y) : x(x), y(y)
┃   ┃   ┣━━ class Animal
┃   ┃   ┣━━ public:
┃   ┃   ┣━━     Animal(const std::string &name) : name(name)
┃   ┃   ┣━━     virtual void speak() const
┃   ┃   ┣━━     virtual ~Animal()
┃   ┃   ┣━━ class Dog : public Animal
┃   ┃   ┣━━ public:
┃   ┃   ┣━━     Dog(const std::string &name) : Animal(name)
┃   ┃   ┣━━     void speak() const override
┃   ┃   ┣━━ class Cat : public Animal
┃   ┃   ┣━━ public:
┃   ┃   ┣━━     Cat(const std::string &name) : Animal(name)
┃   ┃   ┣━━     void speak() const override
┃   ┃   ┣━━ nb::bytes BuildRnnDescriptor(int input_size, int hidden_size, int num_layers,
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
┃   ┃   ┣━━   private:
┃   ┃   ┣━━     static T *ralloc(size_t num)
┃   ┃   ┣━━     static void dealloc(T *ptr)
┃   ┃   ┣━━     static T *ralloc(size_t num)
┃   ┃   ┣━━     static void dealloc(T *ptr)
┃   ┃   ┣━━   public:
┃   ┃   ┣━━     arr() : p(0), sz(0)
┃   ┃   ┣━━     arr(size_t n) : p(ralloc(n)), sz(n)
┃   ┃   ┣━━     arr(arr &&other)
┃   ┃   ┃         : p(other.p), sz(other.sz)
┃   ┃   ┣━━     ~arr()
┃   ┃   ┣━━     void resize(size_t n)
┃   ┃   ┣━━     T &operator[](size_t idx)
┃   ┃   ┣━━     T *data()
┃   ┃   ┣━━     size_t size() const
┃   ┃   ┣━━ class Buffer
┃   ┃   ┗━━ std::tuple<array, array, array> quantize(
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
┃   ┣━━ 📄 ruby_test.rb (157 tokens, 37 lines)
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
┣━━ 📁 group4 (1 folder, 10 files) 
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
┃   ┃   ┃           {integer(), any()} | [any()]
┃   ┃   ┃       ) -> processed, integer(), any()} | [{item, any()}].
┃   ┃   ┣━━ -spec complex_function({integer(), any()} | [any()]) -> 
┃   ┃   ┃       {processed, integer(), any()} | [{item, any()}].
┃   ┃   ┣━━ -spec list_manipulation([integer()]) -> [integer()].
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
┃   ┃   ┣━━ person[name_]
┃   ┃   ┣━━ sayHello[]
┃   ┃   ┗━━ sumList[list_List]
┃   ┣━━ 📄 matlab_test.m (45 tokens, 12 lines)
┃   ┃   ┣━━ classdef HelloWorld -> function greet
┃   ┃   ┗━━ function loneFun
┃   ┣━━ 📄 RTest.R (367 tokens, 47 lines)
┃   ┃   ┣━━ class(person)
┃   ┃   ┣━━ greet.Person <- function
┃   ┃   ┣━━ ensure_between = function
┃   ┃   ┗━━ run_intermediate_annealing_process = function
┃   ┣━━ 📄 rust_test.rs (890 tokens, 159 lines)
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
┃   ┃   ┃       L: Loader<S3, S4>
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
┃   ┣━━ 📄 test.zig (436 tokens, 61 lines)
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
┣━━ 📁 group5 (1 folder, 18 files) 
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
┃   ┃   ┣━━ - [x] Task 3
┃   ┃   ┣━━     - [ ] Subtask 3.1
┃   ┃   ┣━━ - [x] Task 6
┃   ┃   ┣━━     - [x] Subtask 6.1
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
┃   ┃   ┣━━ flask[async]
┃   ┃   ┣━━ flask_cors
┃   ┃   ┣━━ stripe
┃   ┃   ┣━━ pyjwt[crypto]
┃   ┃   ┣━━ cognitojwt[async]
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
┣━━ 📁 group6 (1 folder, 7 files) 
┃   ┣━━ 📄 catastrophic.c (4144 tokens, 581 lines)
┃   ┃   ┣━━ struct Point
┃   ┃   ┣━━ struct Point getOrigin()
┃   ┃   ┣━━ float mul_two_floats(float x1, float x2)
┃   ┃   ┣━━ enum days
┃   ┃   ┣━━ long add_two_longs(long x1, long x2)
┃   ┃   ┣━━ double multiplyByTwo(double num)
┃   ┃   ┣━━ char getFirstCharacter(char *str)
┃   ┃   ┣━━ void greet(Person p)
┃   ┃   ┣━━ typedef struct Person
┃   ┃   ┣━━ typedef struct PersonA
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
┃   ┃   ┣━━     while((ln = listNext(&li)))
┃   ┃   ┣━━ static struct config
┃   ┃   ┣━━ class Person
┃   ┃   ┣━━ public:
┃   ┃   ┣━━     Person(std::string n) : name(n)
┃   ┃   ┣━━     void greet()
┃   ┃   ┣━━ void globalGreet()
┃   ┃   ┣━━ int main()
┃   ┃   ┣━━ void printMessage(const std::string &message)
┃   ┃   ┣━━ template<typename T>
┃   ┃   ┣━━ void printVector(const std::vector<T>& vec)
┃   ┃   ┣━━ struct Point
┃   ┃   ┣━━     Point(int x, int y) : x(x), y(y)
┃   ┃   ┣━━ class Animal
┃   ┃   ┣━━   public:
┃   ┃   ┣━━     Animal(const std::string &name) : name(name)
┃   ┃   ┣━━     virtual void speak() const
┃   ┃   ┣━━     virtual ~Animal()
┃   ┃   ┣━━ class Dog : public Animal
┃   ┃   ┣━━   public:
┃   ┃   ┣━━     Dog(const std::string &name) : Animal(name)
┃   ┃   ┣━━     void speak() const override
┃   ┃   ┣━━ class Cat : public Animal
┃   ┃   ┣━━   public:
┃   ┃   ┣━━     Cat(const std::string &name) : Animal(name)
┃   ┃   ┣━━     void speak() const override
┃   ┃   ┣━━ class CatDog: public Animal, public Cat, public Dog
┃   ┃   ┣━━   public:
┃   ┃   ┣━━       CatDog(const std::string &name) : Animal(name)
┃   ┃   ┣━━       int meow_bark()
┃   ┃   ┣━━ nb::bytes BuildRnnDescriptor(int input_size, int hidden_size, int num_layers,
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
┃   ┃   ┣━━   private:
┃   ┃   ┣━━     static T *ralloc(size_t num)
┃   ┃   ┣━━     static void dealloc(T *ptr)
┃   ┃   ┣━━     static T *ralloc(size_t num)
┃   ┃   ┣━━     static void dealloc(T *ptr)
┃   ┃   ┣━━   public:
┃   ┃   ┣━━     arr() : p(0), sz(0)
┃   ┃   ┣━━     arr(size_t n) : p(ralloc(n)), sz(n)
┃   ┃   ┣━━     arr(arr &&other)
┃   ┃   ┃         : p(other.p), sz(other.sz)
┃   ┃   ┣━━     ~arr()
┃   ┃   ┣━━     void resize(size_t n)
┃   ┃   ┣━━     T &operator[](size_t idx)
┃   ┃   ┣━━     T *data()
┃   ┃   ┣━━     size_t size() const
┃   ┃   ┣━━ class Buffer
┃   ┃   ┣━━ std::tuple<array, array, array> quantize(
┃   ┃   ┃       const array& w,
┃   ┃   ┃       int group_size,
┃   ┃   ┃       int bits,
┃   ┃   ┃       StreamOrDevice s)
┃   ┃   ┣━━ #define PY_SSIZE_T_CLEAN
┃   ┃   ┣━━ #define PLATFORM_IS_X86
┃   ┃   ┣━━ #define PLATFORM_WINDOWS
┃   ┃   ┣━━ #define GETCPUID(a, b, c, d, a_inp, c_inp)
┃   ┃   ┣━━ static int GetXCR0EAX()
┃   ┃   ┣━━ #define GETCPUID(a, b, c, d, a_inp, c_inp)
┃   ┃   ┣━━ static int GetXCR0EAX()
┃   ┃   ┣━━   asm("XGETBV" : "=a"(eax), "=d"(edx) : "c"(0))
┃   ┃   ┣━━ static void ReportMissingCpuFeature(const char* name)
┃   ┃   ┣━━ static PyObject *CheckCpuFeatures(PyObject *self, PyObject *args)
┃   ┃   ┣━━ static PyObject *CheckCpuFeatures(PyObject *self, PyObject *args)
┃   ┃   ┣━━ static PyMethodDef cpu_feature_guard_methods[]
┃   ┃   ┣━━ static struct PyModuleDef cpu_feature_guard_module
┃   ┃   ┣━━ #define EXPORT_SYMBOL __declspec(dllexport)
┃   ┃   ┣━━ #define EXPORT_SYMBOL __attribute__ ((visibility("default")))
┃   ┃   ┗━━ EXPORT_SYMBOL PyMODINIT_FUNC PyInit_cpu_feature_guard(void)
┃   ┣━━ 📄 fractal.thy (2183 tokens, 148 lines)
┃   ┃   ┣━━ Title:      fractal.thy
┃   ┃   ┣━━ Author:     Isabelle/HOL Contributors!
┃   ┃   ┣━━ Author:     edge cases r us
┃   ┃   ┣━━ theory Simplified_Ring
┃   ┃   ┣━━ section ‹Basic Algebraic Structures›
┃   ┃   ┣━━ class everything = nothing + itself
┃   ┃   ┣━━ subsection ‹Monoids›
┃   ┃   ┣━━ definition ring_hom :: "[('a, 'm) ring_scheme, ('b, 'n) ring_scheme] => ('a => 'b) set"
┃   ┃   ┣━━ fun example_fun :: "nat ⇒ nat"
┃   ┃   ┣━━ locale monoid =
┃   ┃   ┃     fixes G (structure)
┃   ┃   ┃     assumes m_closed: "⟦x ∈ carrier G; y ∈ carrier G⟧ ⟹  x ⊗ y ∈ carrier G"
┃   ┃   ┃       and m_assoc: "⟦x ∈ carrier G; y ∈ carrier G; z ∈ carrier G⟧ ⟹  (x ⊗ y) ⊗ z = x ⊗ (y ⊗ z)"
┃   ┃   ┃       and one_closed: "𝟭 ∈ carrier G"
┃   ┃   ┃       and l_one: "x ∈ carrier G ⟹  𝟭 ⊗ x = x"
┃   ┃   ┃       and r_one: "x ∈ carrier G ⟹  x ⊗ 𝟭 = x"
┃   ┃   ┣━━ subsection ‹Groups›
┃   ┃   ┣━━ locale group = monoid +
┃   ┃   ┃     assumes Units_closed: "x ∈ Units G ⟹  x ∈ carrier G"
┃   ┃   ┃       and l_inv_ex: "x ∈ carrier G ⟹  ∃ y ∈ carrier G. y ⊗ x = 𝟭"
┃   ┃   ┃       and r_inv_ex: "x ∈ carrier G ⟹  ∃ y ∈ carrier G. x ⊗ y = 𝟭"
┃   ┃   ┣━━ subsection ‹Rings›
┃   ┃   ┣━━ locale ring = abelian_group R + monoid R +
┃   ┃   ┃     assumes l_distr: "⟦x ∈ carrier R; y ∈ carrier R; z ∈ carrier R⟧ ⟹  (x ⊕ y) ⊗ z = x ⊗ z ⊕ y ⊗ z"
┃   ┃   ┃       and r_distr: "⟦x ∈ carrier R; y ∈ carrier R; z ∈ carrier R⟧ ⟹  z ⊗ (x ⊕ y) = z ⊗ x ⊕ z ⊗ y"
┃   ┃   ┣━━ locale commutative_ring = ring +
┃   ┃   ┃     assumes m_commutative: "⟦x ∈ carrier R; y ∈ carrier R⟧ ⟹  x ⊗ y = y ⊗ x"
┃   ┃   ┣━━ locale domain = commutative_ring +
┃   ┃   ┃     assumes no_zero_divisors: "⟦a ⊗ b = 𝟬; a ∈ carrier R; b ∈ carrier R⟧ ⟹  a = 𝟬 ∨ b = 𝟬"
┃   ┃   ┣━━ locale field = domain +
┃   ┃   ┃     assumes inv_ex: "x ∈ carrier R - {𝟬} ⟹  inv x ∈ carrier R"
┃   ┃   ┣━━ subsection ‹Morphisms›
┃   ┃   ┣━━ lemma example_lemma: "example_fun n = n"
┃   ┃   ┣━━ qualified lemma gcd_0:
┃   ┃   ┃     "gcd a 0 = normalize a"
┃   ┃   ┣━━ lemma abelian_monoidI:
┃   ┃   ┃     fixes R (structure)
┃   ┃   ┃         and f :: "'edge::{} ⇒ 'case::{}"
┃   ┃   ┃     assumes "⋀x y. ⟦ x ∈ carrier R; y ∈ carrier R ⟧ ⟹  x ⊕ y ∈ carrier R"
┃   ┃   ┃         and "𝟬 ∈ carrier R"
┃   ┃   ┃         and "⋀x y z. ⟦ x ∈ carrier R; y ∈ carrier R; z ∈ carrier R ⟧ ⟹  (x ⊕ y) ⊕ z = x ⊕ (y ⊕ z)"
┃   ┃   ┃     shows "abelian_monoid R"
┃   ┃   ┣━━ lemma euclidean_size_gcd_le1 [simp]:
┃   ┃   ┃     assumes "a ≠ 0"
┃   ┃   ┃     shows "euclidean_size (gcd a b) ≤ euclidean_size a"
┃   ┃   ┣━━ theorem Residue_theorem:
┃   ┃   ┃     fixes S pts::"complex set" and f::"complex ⇒ complex"
┃   ┃   ┃       and g::"real ⇒ complex"
┃   ┃   ┃     assumes "open S" "connected S" "finite pts" and
┃   ┃   ┃             holo:"f holomorphic_on S-pts" and
┃   ┃   ┃             "valid_path g" and
┃   ┃   ┃             loop:"pathfinish g = pathstart g" and
┃   ┃   ┃             "path_image g ⊆ S-pts" and
┃   ┃   ┃             homo:"∀z. (z ∉ S) ⟶  winding_number g z  = 0"
┃   ┃   ┃     shows "contour_integral g f = 2 * pi * 𝗂 *(∑p ∈ pts. winding_number g p * residue f p)"
┃   ┃   ┣━━ corollary fps_coeff_residues_bigo':
┃   ┃   ┃     fixes f :: "complex ⇒ complex" and r :: real
┃   ┃   ┃     assumes exp: "f has_fps_expansion F"
┃   ┃   ┃     assumes "open A" "connected A" "cball 0 r ⊆ A" "r > 0" 
┃   ┃   ┃     assumes "f holomorphic_on A - S" "S ⊆ ball 0 r" "finite S" "0 ∉ S"
┃   ┃   ┃     assumes "eventually (λn. g n = -(∑z ∈ S. residue (λz. f z / z ^ Suc n) z)) sequentially"
┃   ┃   ┃                (is "eventually (λn. _ = -?g' n) _")
┃   ┃   ┃     shows   "(λn. fps_nth F n - g n) ∈ O(λn. 1 / r ^ n)" (is "(λn. ?c n - _) ∈ O(_)")
┃   ┃   ┗━━ end
┃   ┣━━ 📄 Microsoft.PowerShell_profile.ps1 (3854 tokens, 498 lines)
┃   ┃   ┣━━ function Log($message)
┃   ┃   ┣━━ function Remove-ChocolateyFromPath
┃   ┃   ┣━━ function Show-Profiles
┃   ┃   ┣━━ function Show-Path
┃   ┃   ┣━━ function Show-Error($err)
┃   ┃   ┣━━ function Get-ScoopPackagePath
┃   ┃   ┣━━   param(
┃   ┃   ┃       [Parameter(Mandatory = $true)]
┃   ┃   ┃       [string]$PackageName)
┃   ┃   ┣━━ function Check-Command
┃   ┃   ┣━━   param(
┃   ┃   ┃       [Parameter(Mandatory = $true)]
┃   ┃   ┃       [string]$Name)
┃   ┃   ┣━━ function Add-ToPath
┃   ┃   ┣━━   param(
┃   ┃   ┃       [Parameter(Mandatory = $true)]
┃   ┃   ┃       [string]$PathToAdd)
┃   ┃   ┣━━ function Install-Scoop
┃   ┃   ┣━━ function Scoop-Install
┃   ┃   ┣━━   param(
┃   ┃   ┃       [Parameter(Mandatory = $true)]
┃   ┃   ┃       [string]$Name,
┃   ┃   ┃       [string]$PathToAdd)
┃   ┃   ┣━━ function Start-CondaEnv
┃   ┃   ┣━━ function Install-PipPackage
┃   ┃   ┣━━   param(
┃   ┃   ┃           [Parameter(Mandatory = $true)]
┃   ┃   ┃       [string]$PackageName)
┃   ┃   ┣━━ function Install-VSBuildTools
┃   ┃   ┣━━ function Install-Crate
┃   ┃   ┣━━   param(
┃   ┃   ┃           [Parameter(Mandatory = $true)]
┃   ┃   ┃       [string]$CrateName)
┃   ┃   ┣━━ function Get-ScoopVersion
┃   ┃   ┣━━ function Get-Version
┃   ┃   ┣━━     param(
┃   ┃   ┃           [Parameter(Mandatory = $true)]
┃   ┃   ┃           [string]$ExecutablePath,
┃   ┃   ┃           [string]$ExecutableName)
┃   ┃   ┣━━ function Show-Requirements
┃   ┃   ┣━━   function Measure-Status
┃   ┃   ┣━━     param(
┃   ┃   ┃         [Parameter(Mandatory = $true)]
┃   ┃   ┃         [string]$Name)
┃   ┃   ┣━━ function Find-Profile
┃   ┃   ┣━━ function Edit-Profile
┃   ┃   ┣━━ function Set-Profile
┃   ┃   ┗━━ function Show-Profile
┃   ┣━━ 📄 ramda__cloneRegExp.js (105 tokens, 9 lines)
┃   ┃   ┗━━ export default function _cloneRegExp(pattern)
┃   ┣━━ 📄 ramda_prop.js (747 tokens, 86 lines)
┃   ┃   ┣━━ /**
┃   ┃   ┃    * Returns a function that when supplied an object returns the indicated
┃   ┃   ┃    * property of that object, if it exists.
┃   ┃   ┃    * @category Object
┃   ┃   ┃    * @typedefn Idx = String | Int | Symbol
┃   ┃   ┃    * @sig Idx -> {s: a} -> a | Undefined
┃   ┃   ┃    * @param {String|Number} p The property name or array index
┃   ┃   ┃    * @param {Object} obj The object to query
┃   ┃   ┃    * @return {*} The value at `obj.p`.
┃   ┃   ┃    */
┃   ┃   ┃   var prop = _curry2(function prop(p, obj)
┃   ┃   ┣━━ /**
┃   ┃   ┃    * Solves equations of the form a * x = b
┃   ┃   ┃    * @param {{
┃   ┃   ┃    *  z: number
┃   ┃   ┃    * }} x
┃   ┃   ┃    */
┃   ┃   ┃   function foo(x)
┃   ┃   ┣━━ /**
┃   ┃   ┃    * Deconstructs an array field from the input documents to output a document for each element.
┃   ┃   ┃    * Each output document is the input document with the value of the array field replaced by the element.
┃   ┃   ┃    * @category Object
┃   ┃   ┃    * @sig String -> {k: [v]} -> [{k: v}]
┃   ┃   ┃    * @param {String} key The key to determine which property of the object should be unwound.
┃   ┃   ┃    * @param {Object} object The object containing the list to unwind at the property named by the key.
┃   ┃   ┃    * @return {List} A list of new objects, each having the given key associated to an item from the unwound list.
┃   ┃   ┃    */
┃   ┃   ┃   var unwind = _curry2(function(key, object)
┃   ┃   ┗━━   return _map(function(item)
┃   ┣━━ 📄 test.f (200 tokens, 31 lines)
┃   ┃   ┣━━ MODULE basic_mod
┃   ┃   ┣━━     TYPE :: person
┃   ┃   ┃           CHARACTER(LEN=50) :: name
┃   ┃   ┃           INTEGER :: age
┃   ┃   ┃       END TYPE person
┃   ┃   ┣━━     SUBROUTINE short_hello(happy, path)
┃   ┃   ┃       END SUBROUTINE short_hello
┃   ┃   ┣━━     SUBROUTINE long_hello(
┃   ┃   ┃           p,
┃   ┃   ┃           message
┃   ┃   ┃       )
┃   ┃   ┃       END SUBROUTINE long_hello
┃   ┃   ┣━━ END MODULE basic_mod
┃   ┃   ┗━━ PROGRAM HelloFortran
┃   ┃       END PROGRAM HelloFortran
┃   ┗━━ 📄 torch.rst (53 tokens, 9 lines)
┃       ┣━━ # libtorch (C++-only)
┃       ┗━━ - Building libtorch using Python
┗━━ 📁 group_lisp (1 folder, 4 files) 
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

tree_plus v(1.0.25) ignore=('group_todo',) globs=() syntax=False paths=('tests/more_languages',)
8 folder(s), 74 file(s), 5,759 line(s), 41,377 token(s) in 0.47 second(s).

```
<!-- t2-end -->
## Got Globs?

<!-- t3-start -->
```sh
tree_plus -g "*.*s" -i group_todo tests/more_languages
📁 more_languages (6 folders, 16 files) 
┣━━ 📁 group1 (1 folder, 2 files) 
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
┃   ┗━━ 📄 test.ts (871 tokens, 166 lines)
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
┃       ┣━━ export default async function tsFunctionComplicated<A, B, C>({
┃       ┃     a = 1 | 2,
┃       ┃     b = "bob",
┃       ┃     c = async () => "charlie",
┃       ┃   }: {
┃       ┃     a: number;
┃       ┃     b: string;
┃       ┃     c: () => Promise<string>;
┃       ┃   }): Promise<string>
┃       ┣━━ const tsArrowFunctionSigned = ({
┃       ┃     a,
┃       ┃     b,
┃       ┃   }: {
┃       ┃     a: number;
┃       ┃     b: string;
┃       ┃   }) =>
┃       ┣━━ export const tsComplicatedArrow = async ({
┃       ┃     a = 1 | 2,
┃       ┃     b = "bob",
┃       ┃     c = async () => "charlie",
┃       ┃   }: {
┃       ┃     a: number;
┃       ┃     b: string;
┃       ┃     c: () => Promise<string>;
┃       ┃   }): Promise<string> =>
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
┣━━ 📁 group3 (1 folder, 1 file) 
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
┣━━ 📁 group4 (1 folder, 3 files) 
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
┃   ┣━━ 📄 rust_test.rs (890 tokens, 159 lines)
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
┃   ┃   ┃       L: Loader<S3, S4>
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
┣━━ 📁 group5 (1 folder, 8 files) 
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
┃   ┣━━ 📄 environment.test.ts (193 tokens, 19 lines)
┃   ┃   ┣━━ environment:
┃   ┃   ┣━━    production
┃   ┃   ┣━━    cognitoUserPoolId
┃   ┃   ┣━━    cognitoAppClientId
┃   ┃   ┗━━    apiurl
┃   ┣━━ 📄 rust_todo_test.rs (109 tokens, 27 lines)
┃   ┃   ┣━━ TODO (Line 23): This todo tests parse_todo
┃   ┃   ┣━━ enum Color
┃   ┃   ┣━━ struct Point
┃   ┃   ┣━━ trait Drawable
┃   ┃   ┣━━     fn draw(&self)
┃   ┃   ┣━━ impl Drawable for Point
┃   ┃   ┣━━     fn draw(&self)
┃   ┃   ┗━━ fn main()
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
┗━━ 📁 group6 (1 folder, 2 files) 
    ┣━━ 📄 ramda__cloneRegExp.js (105 tokens, 9 lines)
    ┃   ┗━━ export default function _cloneRegExp(pattern)
    ┗━━ 📄 ramda_prop.js (747 tokens, 86 lines)
        ┣━━ /**
        ┃    * Returns a function that when supplied an object returns the indicated
        ┃    * property of that object, if it exists.
        ┃    * @category Object
        ┃    * @typedefn Idx = String | Int | Symbol
        ┃    * @sig Idx -> {s: a} -> a | Undefined
        ┃    * @param {String|Number} p The property name or array index
        ┃    * @param {Object} obj The object to query
        ┃    * @return {*} The value at `obj.p`.
        ┃    */
        ┃   var prop = _curry2(function prop(p, obj)
        ┣━━ /**
        ┃    * Solves equations of the form a * x = b
        ┃    * @param {{
        ┃    *  z: number
        ┃    * }} x
        ┃    */
        ┃   function foo(x)
        ┣━━ /**
        ┃    * Deconstructs an array field from the input documents to output a document for each element.
        ┃    * Each output document is the input document with the value of the array field replaced by the element.
        ┃    * @category Object
        ┃    * @sig String -> {k: [v]} -> [{k: v}]
        ┃    * @param {String} key The key to determine which property of the object should be unwound.
        ┃    * @param {Object} object The object containing the list to unwind at the property named by the key.
        ┃    * @return {List} A list of new objects, each having the given key associated to an item from the unwound list.
        ┃    */
        ┃   var unwind = _curry2(function(key, object)
        ┗━━   return _map(function(item)

tree_plus v(1.0.25) ignore=('group_todo',) globs=('*.*s',) syntax=False paths=('tests/more_languages',)
6 folder(s), 16 file(s), 1,919 line(s), 13,124 token(s) in 0.14 second(s).

```
<!-- t3-end -->

## Languages Todo:

Help me **add to** and **priorize** this list of languages to support!

- [Open a `tree_plus` GitHub Issue](https://github.com/bionicles/tree_plus/issues/new), or
- [Contact Bion on X](https://twitter.com/flesheatingemu)

<!-- t4-start -->
```sh
tree_plus tests/more_languages/group_todo
📁 group_todo (1 folder, 8 files) 
┣━━ 📄 crystal_test.cr (56 tokens, 15 lines)
┣━━ 📄 dart_test.dart (106 tokens, 24 lines)
┣━━ 📄 elixir_test.exs (49 tokens, 10 lines)
┣━━ 📄 nodemon.json (120 tokens, 21 lines)
┣━━ 📄 sas_test.sas (104 tokens, 22 lines)
┣━━ 📄 test_setup_py.test (118 tokens, 24 lines)
┣━━ 📄 testTypings.d.ts (149 tokens, 23 lines)
┗━━ 📄 vba_test.bas (72 tokens, 16 lines)

tree_plus v(1.0.25) ignore=() globs=() syntax=False paths=('tests/more_languages/group_todo',)
1 folder(s), 8 file(s), 155 line(s), 774 token(s) in 0.02 second(s).

```
<!-- t4-end -->

## Oppose Unfair Business Practices

Please consider contacting the authorities to report the issue described in this document:

[California OpenAI Complaint - Customer Noncompete Clause](https://www.tinyurl.com/cali-openai-complaint)

_Remember_: **Your Voice Matters!**

## License

MIT or Apache 2.0, at your option.