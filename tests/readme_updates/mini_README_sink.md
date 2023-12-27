# Mini Readme

<!-- t1-start -->
```sh
tree_plus -i tests
paths=('.',)
📁 tree_plus (33682 tokens, 4060 lines)
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
┣━━ 📁 tree_plus_src (21347 tokens, 2596 lines)
┃   ┣━━ 📁 scripts (3515 tokens, 450 lines)
┃   ┃   ┣━━ 📄 alias_tree_plus.sh (277 tokens, 31 lines)
┃   ┃   ┃   ┣━━ add_alias()
┃   ┃   ┃   ┗━━ create_conda_env()
┃   ┃   ┗━━ 📄 Microsoft.PowerShell_profile.ps1 (3238 tokens, 419 lines)
┃   ┃       ┣━━ function Log($message)
┃   ┃       ┗━━ function Show-Error($err)
┃   ┣━━ 📄 count_tokens_lines.py (775 tokens, 119 lines)
┃   ┃   ┣━━ TODO (Line 13): show off how well we parse_todo!
┃   ┃   ┣━━ @dataclass
┃   ┃   ┣━━ class TokenLineCount
┃   ┃   ┣━━ def add_tokens_lines(
┃   ┃   ┃       lhs_count: TokenLineCount, rhs_count: TokenLineCount
┃   ┃   ┃   ) -> TokenLineCount
┃   ┃   ┣━━ def count_tokens_lines(file_path: str) -> TokenLineCount
┃   ┃   ┗━━ def count_directory_tokens_lines(directory_path: str) -> 
┃   ┃       TokenLineCount
┃   ┣━━ 📄 debug.py (87 tokens, 20 lines)
┃   ┃   ┣━━ def enable_debug()
┃   ┃   ┣━━ def disable_debug()
┃   ┃   ┣━━ def debug_enabled()
┃   ┃   ┗━━ def debug_print(*args, **kwargs)
┃   ┣━━ 📄 deploy.py (2090 tokens, 240 lines)
┃   ┃   ┣━━ TODO (Line 167): test this reset readme command so we can clean out 
┃   ┃   ┃   the code blocks
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
┃   ┣━━ 📄 ignore.py (1356 tokens, 191 lines)
┃   ┃   ┣━━ @lru_cache
┃   ┃   ┣━━ def make_ignore(ignore: IgnoreInput) -> Ignore
┃   ┃   ┣━━ @lru_cache
┃   ┃   ┣━━ def make_globs(globs: IgnoreInput) -> FrozenSet
┃   ┃   ┣━━ @lru_cache()
┃   ┃   ┣━━ def is_binary_string(data: bytes) -> bool
┃   ┃   ┣━━ @lru_cache()
┃   ┃   ┣━━ def is_binary(file_path: str) -> bool
┃   ┃   ┣━━ @lru_cache(maxsize=None)
┃   ┃   ┗━━ def should_ignore(path: str, ignore: Ignore, globs: Optional[Ignore]
┃   ┃       = None) -> bool
┃   ┣━━ 📄 parse_file.py (16592 tokens, 1966 lines)
┃   ┃   ┣━━ def extract_groups(match: re.Match) -> dict
┃   ┃   ┣━━ def parse_file(file_path: str) -> List[str]
┃   ┃   ┣━━ def parse_csv(filename: str) -> list
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
┃   ┃   ┣━━ def parse_swift(contents) -> List[str]
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
┃   ┃   ┣━━ def remove_ts_comments_and_private_blocks(contents: str) -> str
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
┃   ┃   ┣━━ def parse_kotlin(content: str) -> List[str]
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
┃   ┃   ┣━━ def parse_markers(content: str) -> List[str]
┃   ┃   ┗━━ def parse_rs(contents: str) -> List[str]
┃   ┣━━ 📄 traverse_directory.py (429 tokens, 58 lines)
┃   ┃   ┗━━ def traverse_directory(
┃   ┃           directory_path: str, ignore: IgnoreInput = None, globs: 
┃   ┃       IgnoreInput = None
┃   ┃       ) -> List[str]
┃   ┗━━ 📄 version.py (18 tokens, 2 lines)
┃       ┗━━ __version__ = "1.0.10"
┣━━ 📄 .gitignore (138 tokens, 42 lines)
┣━━ 📄 LICENSE (2123 tokens, 81 lines)
┣━━ 📄 Makefile (368 tokens, 70 lines)
┃   ┣━━ SHELL := /bin/bash
┃   ┣━━ cli
┃   ┣━━ debug
┃   ┣━━ .PHONY: debug_command
┃   ┣━━ debug_command: test test_cli
┃   ┣━━ test: test_normally test_tp_dotdot test_cli test_deploy
┃   ┣━━ test_normally
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
┣━━ 📄 README.md (1123 tokens, 172 lines)
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
┗━━ 📄 tree_plus_cli.py (3449 tokens, 443 lines)
    ┣━━ NOTE (Line 403): parent_count unused, is that ok?
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
    ┣━━ def clean_tree(input_tree: Tree, root_node: bool = False) -> 
    ┃   Optional[Tree]
    ┣━━ def tree_plus(
    ┃       path_or_paths: Union[str, Tuple[str]],
    ┃       ignore: IgnoreInput = None,
    ┃       globs: IgnoreInput = None,
    ┃   ) -> Tree
    ┣━━ def _parse_paths(path_or_paths: Union[str, Tuple[str]]) -> Tuple[str]
    ┣━━ def flatten_to_str(collection: Collection)
    ┣━━ def _handle_paths(paths: Tuple[str], ignore: Ignore, globs: Ignore) -> 
    ┃   Tree
    ┗━━ def _handle_path(
            path: str, ignore: Ignore, globs: Ignore, paths_to_trees: dict
        ) -> Tuple[Tree, TokenLineCount]

```
<!-- t1-end -->

this text should remain