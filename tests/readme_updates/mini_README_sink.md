# Mini Readme

<!-- t1-start -->
```sh
tree_plus -i tests
paths=('.',)
📁 tree_plus (43364 tokens, 4702 lines)
┣━━ 📁 .github (107 tokens, 11 lines)
┃   ┣━━ 📁 workflows (1050 tokens, 127 lines)
┃   ┃   ┣━━ 📄 microsoft.yml (323 tokens, 40 lines)
┃   ┃   ┃   ┣━━ Microsoft
┃   ┃   ┃   ┣━━   job: build
┃   ┃   ┃   ┣━━     - Set up Python ${{ matrix.python-version }}
┃   ┃   ┃   ┣━━     - Install tree_plus
┃   ┃   ┃   ┣━━     - Create .env file
┃   ┃   ┃   ┣━━     - Set PYTHONUTF8 for Windows
┃   ┃   ┃   ┣━━     - Run generic tests
┃   ┃   ┃   ┗━━     - Run specific test
┃   ┃   ┗━━ 📄 unix.yml (727 tokens, 87 lines)
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
┃   ┃       ┗━━     - Commit Updates
┃   ┗━━ 📄 dependabot.yml (107 tokens, 11 lines)
┃       ┗━━ Unsupported YAML Category
┣━━ 📁 tree_plus_src (19671 tokens, 2423 lines)
┃   ┣━━ 📁 scripts (3515 tokens, 450 lines)
┃   ┃   ┣━━ 📄 alias_tree_plus.sh (277 tokens, 31 lines)
┃   ┃   ┃   ┣━━ add_alias()
┃   ┃   ┃   ┗━━ create_conda_env()
┃   ┃   ┗━━ 📄 Microsoft.PowerShell_profile.ps1 (3238 tokens, 419 lines)
┃   ┃       ┣━━ function Log($message)
┃   ┃       ┗━━ function Show-Error($err)
┃   ┣━━ 📄 count_tokens_lines.py (613 tokens, 82 lines)
┃   ┃   ┣━━ TODO (Line 12): show off how well we parse_todo!
┃   ┃   ┣━━ class TokenLineCount
┃   ┃   ┣━━ def add_tokens_lines
┃   ┃   ┣━━ def count_tokens_lines
┃   ┃   ┗━━ def count_directory_tokens_lines
┃   ┣━━ 📄 debug.py (87 tokens, 20 lines)
┃   ┃   ┣━━ def enable_debug
┃   ┃   ┣━━ def disable_debug
┃   ┃   ┣━━ def debug_enabled
┃   ┃   ┗━━ def debug_print
┃   ┣━━ 📄 deploy.py (1727 tokens, 207 lines)
┃   ┃   ┣━━ def extract
┃   ┃   ┣━━ def load
┃   ┃   ┣━━ def extract_version
┃   ┃   ┣━━ def increment_version
┃   ┃   ┣━━ def run_command
┃   ┃   ┣━━ def replace_readme_section
┃   ┃   ┣━━ def update_readme
┃   ┃   ┗━━ def main
┃   ┣━━ 📄 ignore.py (931 tokens, 145 lines)
┃   ┃   ┣━━ def make_ignore
┃   ┃   ┣━━ def make_globs
┃   ┃   ┣━━ def is_binary_string
┃   ┃   ┣━━ def is_binary
┃   ┃   ┗━━ def should_ignore
┃   ┣━━ 📄 parse_file.py (15866 tokens, 1909 lines)
┃   ┃   ┣━━ def extract_groups
┃   ┃   ┣━━ def parse_file
┃   ┃   ┣━━ def parse_csv
┃   ┃   ┣━━ def parse_mathematica
┃   ┃   ┣━━ def parse_r
┃   ┃   ┣━━ def parse_zig
┃   ┃   ┣━━ def parse_hs
┃   ┃   ┣━━ def parse_lisp
┃   ┃   ┣━━ def parse_capnp
┃   ┃   ┣━━ def parse_grpc
┃   ┃   ┣━━ def parse_openrpc_json
┃   ┃   ┣━━ def parse_json_rpc
┃   ┃   ┣━━ def parse_graphql
┃   ┃   ┣━━ def format_dependency
┃   ┃   ┣━━ def parse_cargo_toml
┃   ┃   ┣━━ def parse_pyproject_toml
┃   ┃   ┣━━ def parse_lean
┃   ┃   ┣━━ def parse_cs
┃   ┃   ┣━━ def parse_tex
┃   ┃   ┣━━ def parse_rb
┃   ┃   ┣━━ def remove_c_comments
┃   ┃   ┣━━ def parse_cpp
┃   ┃   ┣━━ def parse_c
┃   ┃   ┣━━ def parse_go
┃   ┃   ┣━━ def parse_swift
┃   ┃   ┣━━ def parse_bash
┃   ┃   ┣━━ def parse_d_dot_ts
┃   ┃   ┣━━ def parse_angular_app_module
┃   ┃   ┣━━ def parse_angular_routes
┃   ┃   ┣━━ def parse_angular_spec
┃   ┃   ┣━━ def parse_environment_ts
┃   ┃   ┣━━ def parse_dot_env
┃   ┃   ┣━━ def parse_requirements_txt
┃   ┃   ┣━━ def parse_json_schema
┃   ┃   ┣━━ def parse_package_json
┃   ┃   ┣━━ def remove_ts_comments_and_private_blocks
┃   ┃   ┣━━ def parse_ts
┃   ┃   ┣━━ def parse_makefile
┃   ┃   ┣━━ def parse_sql
┃   ┃   ┣━━ def is_openapi_yml
┃   ┃   ┣━━ def is_k8s_yml
┃   ┃   ┣━━ def is_ansible_yml
┃   ┃   ┣━━ def is_github_yml
┃   ┃   ┣━━ def parse_github_yml
┃   ┃   ┣━━ def parse_k8s
┃   ┃   ┣━━ def parse_ansible
┃   ┃   ┣━━ def parse_openapi_yml
┃   ┃   ┣━━ def parse_yml
┃   ┃   ┣━━ def extract_nodes
┃   ┃   ┣━━ def is_typing_construct
┃   ┃   ┣━━ def is_builtin_type
┃   ┃   ┣━━ def parse_py
┃   ┃   ┣━━ def parse_db
┃   ┃   ┣━━ def parse_cobol
┃   ┃   ┣━━ def parse_java
┃   ┃   ┣━━ def parse_julia
┃   ┃   ┣━━ def parse_kotlin
┃   ┃   ┣━━ def parse_lua
┃   ┃   ┣━━ def parse_objective_c
┃   ┃   ┣━━ def parse_ocaml
┃   ┃   ┣━━ def parse_apl
┃   ┃   ┣━━ def parse_perl
┃   ┃   ┣━━ def parse_php
┃   ┃   ┣━━ def parse_powershell
┃   ┃   ┣━━ def parse_matlab
┃   ┃   ┣━━ def parse_scala
┃   ┃   ┣━━ def parse_tf
┃   ┃   ┣━━ def parse_md
┃   ┃   ┣━━ def parse_txt
┃   ┃   ┣━━ def parse_markers
┃   ┃   ┗━━ def parse_rs
┃   ┣━━ 📄 traverse_directory.py (429 tokens, 58 lines)
┃   ┃   ┗━━ def traverse_directory
┃   ┗━━ 📄 version.py (18 tokens, 2 lines)
┣━━ 📄 .gitignore (138 tokens, 42 lines)
┣━━ 📄 LICENSE (2123 tokens, 81 lines)
┣━━ 📄 Makefile (309 tokens, 61 lines)
┃   ┣━━ SHELL := /bin/bash
┃   ┣━━ cli
┃   ┣━━ debug
┃   ┣━━ .PHONY: debug_command
┃   ┣━━ debug_command: test test_cli
┃   ┣━━ test: test_tp_dotdot
┃   ┣━━ test_tp_dotdot
┃   ┣━━ test_cli: cli
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
┣━━ 📄 nodemon.json (102 tokens, 18 lines)
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
┣━━ 📄 README.md (12609 tokens, 1007 lines)
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
┗━━ 📄 tree_plus_cli.py (3402 tokens, 438 lines)
    ┣━━ NOTE (Line 398): parent_count unused, is that ok?
    ┣━━ def tree_to_string
    ┣━━ def clean_string
    ┣━━ def safe_print
    ┣━━ def main
    ┣━━ def subtree
    ┣━━ def clean_tree
    ┣━━ def tree_plus
    ┣━━ def _parse_paths
    ┣━━ def flatten_to_str
    ┣━━ def _handle_paths
    ┗━━ def _handle_path

```
<!-- t1-end -->

this text should remain