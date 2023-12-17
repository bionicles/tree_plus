# Tree Plus

[![License](https://img.shields.io/badge/license-MIT%2FApache--2.0-brightgreen)](https://choosealicense.com/licenses/)
[![Docs](https://img.shields.io/badge/docs-README-blue)](https://github.com/bionicles/tree_plus/blob/main/README.md)
[![Linux & MacOS](https://github.com/bionicles/tree_plus/actions/workflows/unix.yml/badge.svg)](https://github.com/bionicles/tree_plus/actions/workflows/unix.yml)
[![Microsoft](https://github.com/bionicles/tree_plus/actions/workflows/microsoft.yml/badge.svg)](https://github.com/bionicles/tree_plus/actions/workflows/microsoft.yml)

**Supported Operating Systems:**
![Ubuntu](https://img.shields.io/badge/Supports-Ubuntu-orange)
![MacOS](https://img.shields.io/badge/Supports-MacOS-orange)
![Windows](https://img.shields.io/badge/Supports-Windows-orange)

**Supported Python Versions:**
![Python 3.8](https://img.shields.io/badge/Python-3.8-blue)
![Python 3.9](https://img.shields.io/badge/Python-3.9-blue)
![Python 3.10](https://img.shields.io/badge/Python-3.10-blue)
![Python 3.11](https://img.shields.io/badge/Python-3.11-blue)
![Python 3.12](https://img.shields.io/badge/Python-3.11-blue)

A simple command line interface (CLI) tool for developers to show a `tree` enhanced with token counts, line counts, and source code components.

## Example Output:
- [ ] Demonstrate Parsed Checkboxes
```sh
(py310) bion@WIN-QVRBL09D89C:~/hax/tree_plus$ tree_plus -i tests
📁 tree_plus (20023 tokens, 2071 lines)
┣━━ 📁 .github
┃   ┗━━ 📁 workflows (503 tokens, 70 lines)
┃       ┣━━ 📄 microsoft.yml (263 tokens, 36 lines)
┃       ┃   ┣━━ Microsoft
┃       ┃   ┣━━   job: build
┃       ┃   ┣━━     - Set up Python ${{ matrix.python-version }}
┃       ┃   ┣━━     - Install tree_plus
┃       ┃   ┣━━     - Set PYTHONUTF8 for Windows
┃       ┃   ┣━━     - Run generic tests
┃       ┃   ┗━━     - Run specific test
┃       ┗━━ 📄 unix.yml (240 tokens, 34 lines)
┃           ┣━━ Ubuntu & MacOS
┃           ┣━━   job: build
┃           ┣━━     - Set up Python ${{ matrix.python-version }}
┃           ┣━━     - Install tree_plus
┃           ┣━━     - Run generic tests
┃           ┗━━     - Run specific test
┣━━ 📁 tree_plus_src (8776 tokens, 1088 lines)
┃   ┣━━ 📄 __init__.py (64 tokens, 9 lines)
┃   ┣━━ 📄 count_tokens_lines.py (461 tokens, 67 lines)
┃   ┃   ┣━━ TODO (Line 11): show off how well we parse_todo!
┃   ┃   ┣━━ class TokenLineCount
┃   ┃   ┣━━ def count_tokens_lines
┃   ┃   ┗━━ def count_directory_tokens_lines
┃   ┣━━ 📄 traverse_directory.py (274 tokens, 39 lines)
┃   ┃   ┗━━ def traverse_directory
┃   ┣━━ 📄 default_ignore.py (248 tokens, 53 lines)
┃   ┗━━ 📄 parse_file.py (7729 tokens, 920 lines)
┃       ┣━━ def parse_file
┃       ┣━━ def is_k8s_yml
┃       ┣━━ def is_ansible_yml
┃       ┣━━ def is_github_yml
┃       ┣━━ def parse_github_yml
┃       ┣━━ def parse_k8s
┃       ┣━━ def parse_ansible
┃       ┣━━ def parse_yml
┃       ┣━━ def extract_nodes
┃       ┣━━ def is_typing_construct
┃       ┣━━ def is_builtin_type
┃       ┣━━ def parse_py
┃       ┣━━ def parse_db
┃       ┣━━ def parse_cobol
┃       ┣━━ def parse_java
┃       ┣━━ def parse_julia
┃       ┣━━ def parse_kotlin
┃       ┣━━ def parse_lisp
┃       ┣━━ def parse_lua
┃       ┣━━ def parse_objective_c
┃       ┣━━ def parse_ocaml
┃       ┣━━ def parse_apl
┃       ┣━━ def parse_perl
┃       ┣━━ def parse_php
┃       ┣━━ def parse_powershell
┃       ┣━━ def parse_matlab
┃       ┣━━ def parse_scala
┃       ┣━━ def parse_c
┃       ┣━━ def parse_rs
┃       ┣━━ def handle_block
┃       ┣━━ def parse_tf
┃       ┣━━ def parse_js
┃       ┣━━ def parse_md
┃       ┣━━ def parse_txt
┃       ┗━━ def parse_todo
┣━━ 📄 Makefile (85 tokens, 16 lines)
┣━━ 📄 README.md (4530 tokens, 319 lines)
┃   ┣━━ # Tree Plus
┃   ┣━━ ## Example Output:
┃   ┣━━ - [ ] Demonstrate Parsed Checkboxes
┃   ┣━━ ## Start Quick!
┃   ┗━━ ## Moar Usage
┣━━ 📄 tree_plus_cli.py (2586 tokens, 298 lines)
┃   ┣━━ def main
┃   ┣━━ def tree_plus
┃   ┗━━ def tree_to_string
┣━━ 📄 setup.py (393 tokens, 69 lines)
┃   ┗━━ TODO (Line 16): automatically install tree-sitter-{language} grammars
┣━━ 📄 requirements.txt (11 tokens, 4 lines)
┣━━ 📄 LICENSE (2123 tokens, 81 lines)
┣━━ 📄 .gitignore (97 tokens, 28 lines)
┗━━ 📄 sample_output.sh (3039 tokens, 184 lines)
```
- [x] Demonstrate Parsed Checkboxes


## Start Quick!

### Prerequisites

Ensure you have the following tools installed:

- [Miniconda3 (conda)](https://docs.conda.io/en/latest/miniconda.html)
- [GitHub CLI (gh)](https://cli.github.com/manual/installation)

### Install Tree Plus

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
pip install -e .
```

Now, you can use the `tree_plus` command.

3. Add slick aliases like `tp` to your RC file
Typical RC_FILE values `~/.bash_profile`, `~/.bashrc`, or `~/.zshrc`
```bash
RC_FILE=~/.bash_profile source alias_tree_plus.sh
```
```bash
RC_FILE=~/.bashrc source alias_tree_plus.sh
```
```zsh
RC_FILE=~/.zshrc source alias_tree_plus.sh
```

Soon, you'll be able to simply run 'pip install tree_plus'. Stay tuned!

### Usage

```bash
# Use tree_plus
tp
```

If you make changes to `tree_plus` and need to reinstall it quickly, use the `tpcli` alias:

```bash
# Reinstall tree_plus quickly after making changes
tpcli
```

## Moar Usage

Multiple directories:

```sh
(py310) bion@WIN-QVRBL09D89C:~/hax/tree_plus$ tp tests/path_to_test,tests/more_languages/group1
Multiple Directories:
┣━━ 📁 path_to_test (153 tokens, 38 lines)
┃   ┣━━ 📄 file.py (11 tokens, 2 lines)
┃   ┃   ┗━━ def hello_world
┃   ┣━━ 📄 file.txt (11 tokens, 2 lines)
┃   ┣━━ 📄 class_function_type.ts (45 tokens, 12 lines)
┃   ┃   ┣━━ type MyType
┃   ┃   ┣━━ class TsClass
┃   ┃   ┗━━ function tsFunction
┃   ┣━━ 📄 file.js (14 tokens, 3 lines)
┃   ┃   ┗━━ function helloWorld
┃   ┣━━ 📄 empty.py (0 tokens, 0 lines)
┃   ┣━━ 📄 file.md (12 tokens, 2 lines)
┃   ┃   ┗━━ # Hello, world!
┃   ┣━━ 📄 class_function.js (33 tokens, 9 lines)
┃   ┃   ┣━━ class MyClass
┃   ┃   ┗━━ function myFunction
┃   ┗━━ 📄 class_method_type.py (27 tokens, 8 lines)
┃       ┣━━ MyType
┃       ┣━━ class MyClass
┃       ┗━━ class MyClass -> def my_method
┗━━ 📁 group1 (402 tokens, 88 lines)
    ┣━━ 📄 KotlinTest.kt (29 tokens, 4 lines)
    ┃   ┣━━ data class Person(val name: String)
    ┃   ┗━━ fun greet(person: Person)
    ┣━━ 📄 JavaTest.java (47 tokens, 12 lines)
    ┃   ┣━━ class Person
    ┃   ┣━━ class Person -> Person(String name)
    ┃   ┗━━ class Person -> void greet()
    ┣━━ 📄 LispTest.lisp (31 tokens, 5 lines)
    ┃   ┣━━ defstruct person
    ┃   ┗━━ defun greet
    ┣━━ 📄 ObjectiveCTest.m (59 tokens, 16 lines)
    ┃   ┣━━ @interface HelloWorld
    ┃   ┣━━ @interface HelloWorld -> (void) sayHello
    ┃   ┣━━ @implementation HelloWorld
    ┃   ┣━━ @implementation HelloWorld -> (void) sayHello
    ┃   ┗━━ void sayHelloWorld()
    ┣━━ 📄 LuaTest.lua (84 tokens, 16 lines)
    ┃   ┣━━ function HelloWorld.new
    ┃   ┣━━ function HelloWorld.greet
    ┃   ┗━━ function say_hello
    ┣━━ 📄 JuliaTest.jl (42 tokens, 12 lines)
    ┃   ┣━━ module JuliaTest
    ┃   ┣━━ module JuliaTest -> struct Person
    ┃   ┗━━ module JuliaTest -> greet(p::Person)
    ┣━━ 📄 OcamlTest.ml (53 tokens, 12 lines)
    ┃   ┣━━ type color
    ┃   ┣━━ class hello
    ┃   ┣━━ class hello -> method say_hello
    ┃   ┗━━ let main ()
    ┗━━ 📄 COBOL_TEST.CBL (57 tokens, 11 lines)
        ┣━━ IDENTIFICATION DIVISION -> PROGRAM-ID. HELLO
        ┣━━ DATA DIVISION -> 01 GREETING
        ┗━━ PROCEDURE DIVISION
```
