# Tree Plus

**A `tree` util enhanced with tokens, lines, and components.**

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
```sh
(py310) [2023-12-24T20:38:59-0500]
~/hax/tree_plus (main) $
tree_plus -i tests
paths=('.',)
📁 tree_plus (40413 tokens, 4382 lines)
┣━━ 📁 .github
┃   ┗━━ 📁 workflows (625 tokens, 78 lines)
┃       ┣━━ 📄 microsoft.yml (323 tokens, 40 lines)
┃       ┃   ┣━━ Microsoft
┃       ┃   ┣━━   job: build
┃       ┃   ┣━━     - Set up Python ${{ matrix.python-version }}
┃       ┃   ┣━━     - Install tree_plus
┃       ┃   ┣━━     - Create .env file
┃       ┃   ┣━━     - Set PYTHONUTF8 for Windows
┃       ┃   ┣━━     - Run generic tests
┃       ┃   ┗━━     - Run specific test
┃       ┗━━ 📄 unix.yml (302 tokens, 38 lines)
┃           ┣━━ Linux & MacOS
┃           ┣━━   job: build
┃           ┣━━     - Set up Python ${{ matrix.python-version }}
┃           ┣━━     - Install tree_plus
┃           ┣━━     - Create .env file
┃           ┣━━     - Run generic tests
┃           ┗━━     - Run specific test
┣━━ 📁 tree_plus_src (14132 tokens, 1766 lines)
┃   ┣━━ 📁 scripts (6661 tokens, 861 lines)
┃   ┃   ┣━━ 📄 alias_tree_plus.sh (277 tokens, 31 lines)
┃   ┃   ┃   ┣━━ add_alias()
┃   ┃   ┃   ┗━━ create_conda_env()
┃   ┃   ┣━━ 📄 Microsoft.PowerShell_profile.ps1 (3238 tokens, 419 lines)
┃   ┃   ┃   ┣━━ function Log($message)
┃   ┃   ┃   ┗━━ function Show-Error($err)
┃   ┃   ┣━━ 📄 profile.toml (1192 tokens, 126 lines)
┃   ┃   ┣━━ 📄 scratch.ps1 (1317 tokens, 197 lines)
┃   ┃   ┗━━ 📄 scratch.sh (637 tokens, 88 lines)
┃   ┣━━ 📄 count_tokens_lines.py (609 tokens, 81 lines)
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
┃   ┣━━ 📄 ignore.py (931 tokens, 145 lines)
┃   ┃   ┣━━ def make_ignore
┃   ┃   ┣━━ def make_globs
┃   ┃   ┣━━ def is_binary_string
┃   ┃   ┣━━ def is_binary
┃   ┃   ┗━━ def should_ignore
┃   ┣━━ 📄 parse_file.py (12076 tokens, 1462 lines)
┃   ┃   ┣━━ TODO (Line 229): format hierarchical numbered outline
┃   ┃   ┣━━ def parse_file
┃   ┃   ┣━━ def parse_lean
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
┃   ┃   ┣━━ def extract_groups
┃   ┃   ┣━━ def parse_ts
┃   ┃   ┣━━ def parse_makefile
┃   ┃   ┣━━ def parse_sql
┃   ┃   ┣━━ def is_k8s_yml
┃   ┃   ┣━━ def is_ansible_yml
┃   ┃   ┣━━ def is_github_yml
┃   ┃   ┣━━ def parse_github_yml
┃   ┃   ┣━━ def parse_k8s
┃   ┃   ┣━━ def parse_ansible
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
┃   ┃   ┣━━ def parse_lisp
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
┃   ┗━━ 📄 traverse_directory.py (429 tokens, 58 lines)
┃       ┗━━ def traverse_directory
┣━━ 📄 .gitignore (124 tokens, 38 lines)
┣━━ 📄 LICENSE (2123 tokens, 81 lines)
┣━━ 📄 Makefile (298 tokens, 56 lines)
┃   ┣━━ SHELL := /bin/bash
┃   ┣━━ cli
┃   ┣━━ debug
┃   ┣━━ .PHONY: debug_command
┃   ┣━━ debug_command: test test_cli
┃   ┣━━ test: test_tp_dotdot
┃   ┣━━ test_tp_dotdot
┃   ┣━━ test_cli: cli
┃   ┣━━ test_dotenv
┃   ┣━━ build: install-build clean-dist
┃   ┣━━ install-build
┃   ┣━━ test-publish: install-twine
┃   ┣━━ install-twine
┃   ┣━━ publish: install-twine
┃   ┣━━ clean-dist
┃   ┣━━ t1
┃   ┣━━ t2
┃   ┣━━ t3
┃   ┗━━ t4
┣━━ 📄 nodemon.json (97 tokens, 17 lines)
┣━━ 📄 pyproject.toml (307 tokens, 39 lines)
┣━━ 📄 pytest.ini (11 tokens, 3 lines)
┣━━ 📄 README.md (12552 tokens, 998 lines)
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
┗━━ 📄 tree_plus_cli.py (3483 tokens, 445 lines)
    ┣━━ NOTE (Line 405): parent_count unused, is that ok?
    ┣━━ def tree_to_string
    ┣━━ def clean_string
    ┣━━ def safe_print
    ┣━━ def handle_version
    ┣━━ def main
    ┣━━ def subtree
    ┣━━ def clean_tree
    ┣━━ def tree_plus
    ┣━━ def _parse_paths
    ┣━━ def flatten_to_str
    ┣━━ def _handle_paths
    ┗━━ def _handle_path
```
- [x] Demonstrate Parsed Checkboxes

Here's how `tree_plus --help` looks (`-h` and `-H` both  also work) 
```sh
~/hax/tree_plus (main) $
> tp -h
Usage: tree_plus [OPTIONS] [PATHS]...

  A `tree` util enhanced with tokens, lines, and components.

  Wrap glob patterns in quotes: -i "*.py" / -g "*.rs"

  Examples:

          Show tree_plus_src and tests simultaneously
              > tree_plus tree_plus_src tests

          Show files matching "*.*s" tests/more_languages
              > tree_plus -g "*.*s" tests/more_languages

          Ignore Java files
              > tree_plus tests -i "*.java"

Options:
  -i, -I, --ignore TEXT  Patterns to ignore, in quotes: -i "*.java"
  -g, -G, --glob TEXT    Patterns to find, in quotes: -g "*.rs"
  -v, -V, --version      Print the version and exit.
  -d, -D, --debug        Enables $DEBUG_TREE_PLUS.
  -h, -H, --help         Show this message and exit.
```

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

```sh
(py310) [2023-12-24T19:58:07-0500]
~/hax/tree_plus (main) $
tree_plus -i group_todo tests/more_languages
paths=('tests/more_languages',)
DEBUG_TREE_PLUS=None
📁 more_languages (17960 tokens, 2547 lines)
┣━━ 📁 group1 (402 tokens, 88 lines)
┃   ┣━━ 📄 COBOL_TEST.CBL (57 tokens, 11 lines)
┃   ┃   ┣━━ IDENTIFICATION DIVISION -> PROGRAM-ID. HELLO
┃   ┃   ┣━━ DATA DIVISION -> 01 GREETING
┃   ┃   ┗━━ PROCEDURE DIVISION
┃   ┣━━ 📄 JavaTest.java (47 tokens, 12 lines)
┃   ┃   ┣━━ class Person
┃   ┃   ┣━━ class Person -> Person(String name)
┃   ┃   ┗━━ class Person -> void greet()
┃   ┣━━ 📄 JuliaTest.jl (42 tokens, 12 lines)
┃   ┃   ┣━━ module JuliaTest
┃   ┃   ┣━━ module JuliaTest -> struct Person
┃   ┃   ┗━━ module JuliaTest -> greet(p::Person)
┃   ┣━━ 📄 KotlinTest.kt (29 tokens, 4 lines)
┃   ┃   ┣━━ data class Person(val name: String)
┃   ┃   ┗━━ fun greet(person: Person)
┃   ┣━━ 📄 LispTest.lisp (31 tokens, 5 lines)
┃   ┃   ┣━━ defstruct person
┃   ┃   ┗━━ defun greet
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
┃   ┗━━ 📄 OcamlTest.ml (53 tokens, 12 lines)
┃       ┣━━ type color
┃       ┣━━ class hello
┃       ┣━━ class hello -> method say_hello
┃       ┗━━ let main ()
┣━━ 📁 group2 (754 tokens, 164 lines)
┃   ┣━━ 📄 apl_test.apl (44 tokens, 5 lines)
┃   ┃   ┣━━ :Namespace HelloWorld
┃   ┃   ┣━━ :Namespace HelloWorld -> hello ← 'Hello, World!'
┃   ┃   ┗━━ :Namespace HelloWorld -> plus ← {⍺+⍵}
┃   ┣━━ 📄 c_test.c (256 tokens, 69 lines)
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
┃   ┃   ┗━━ int* getArrayStart(int arr[], int size)
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
┃   ┗━━ 📄 ScalaTest.scala (136 tokens, 24 lines)
┃       ┣━━ def sumOfSquares(x: Int, y: Int): Int
┃       ┣━━ trait Bark
┃       ┣━━ trait Bark -> def bark: String
┃       ┣━━ case class Person(name: String)
┃       ┣━━ object HelloWorld
┃       ┣━━ object HelloWorld -> def greet(person: Person): Unit
┃       ┣━━ object HelloWorld -> def main(args: Array[String]): Unit
┃       ┗━━ def sumOfSquaresShort(x: Int, y: Int): Int
┣━━ 📁 group3 (4468 tokens, 625 lines)
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
┃   ┃   ┃                                int workspace_size, int reserve_space_size)
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
┃   ┣━━ 📄 go_test.go (169 tokens, 46 lines)
┃   ┃   ┣━━ type Greeting struct
┃   ┃   ┣━━ func (g Greeting) sayHello()
┃   ┃   ┣━━ func createGreeting(m string) Greeting
┃   ┃   ┣━━ type SomethingLong struct
┃   ┃   ┣━━ func (s *SomethingLong) WithAReasonableName(
┃   ┃   ┃           ctx context.Context,
┃   ┃   ┃           param1 string,
┃   ┃   ┃           param2 int,
┃   ┃   ┃           param3 mapinterface{},
┃   ┃   ┃           callback func(int) error,
┃   ┃   ┃   ) (resultType, error)
┃   ┃   ┣━━ type resultType struct
┃   ┃   ┗━━ func main()
┃   ┣━━ 📄 hallucination.tex (1465 tokens, 127 lines)
┃   ┃   ┣━━ Harnessing the Master Algorithm: Strategies for AI Large Language Models to Mitigate
┃   ┃   ┃   Hallucinations
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
┃   ┣━━ 📄 ruby_test.rb (110 tokens, 28 lines)
┃   ┃   ┣━━ module Greeter
┃   ┃   ┣━━   def self.say_hello
┃   ┃   ┣━━ class HelloWorld
┃   ┃   ┣━━   def say_hello
┃   ┃   ┣━━ class Human
┃   ┃   ┣━━   def self.bar
┃   ┃   ┣━━   def self.bar=(value)
┃   ┃   ┗━━ class Doctor < Human
┃   ┣━━ 📄 swift_test.swift (449 tokens, 99 lines)
┃   ┃   ┣━━ class Person
┃   ┃   ┣━━ func globalGreet()
┃   ┃   ┣━━ struct Point
┃   ┃   ┣━━ protocol Animal
┃   ┃   ┣━━ struct Dog: Animal
┃   ┃   ┣━━ class Cat: Animal
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
┃   ┗━━ 📄 test.lean (384 tokens, 43 lines)
┃       ┣━━ # Advanced Topics in Group Theory
┃       ┣━━ section GroupDynamics
┃       ┣━━ lemma group_stability (G : Type*) [Group G] (H : Subgroup G)
┃       ┣━━ theorem subgroup_closure {G : Type*} [Group G] (S : Set G)
┃       ┣━━ axiom group_homomorphism_preservation {G H : Type*} [Group G] [Group H] (f : G → H)
┃       ┣━━ end GroupDynamics
┃       ┣━━ section ConstructiveApproach
┃       ┣━━ lemma finite_group_order (G : Type*) [Group G] [Fintype G]
┃       ┣━━ lemma complex_lemma {X Y : Type*} [SomeClass X] [AnotherClass Y]
┃       ┃     (f : X → Y) (g : Y → X)
┃       ┗━━ end ConstructiveApproach
┣━━ 📁 group4 (944 tokens, 166 lines)
┃   ┣━━ 📄 matlab_test.m (45 tokens, 12 lines)
┃   ┃   ┣━━ classdef HelloWorld -> function greet
┃   ┃   ┗━━ function loneFun
┃   ┣━━ 📄 rust_test.rs (611 tokens, 115 lines)
┃   ┃   ┣━━ enum Days
┃   ┃   ┣━━ struct Point
┃   ┃   ┣━━ impl Point
┃   ┃   ┣━━ fn get_origin() -> Point
┃   ┃   ┣━━ struct Person
┃   ┃   ┣━━ impl Person
┃   ┃   ┣━━ fn greet(&self)
┃   ┃   ┣━━ fn add_two_longs(x1: i64, x2: i64) -> i64
┃   ┃   ┣━━ fn add_two_longs_longer(
┃   ┃   ┃       x1: i64,
┃   ┃   ┃       x2: i64,
┃   ┃   ┃   ) -> i64
┃   ┃   ┣━━ fn multiply_by_two(num: f64) -> f64
┃   ┃   ┣━━ fn get_first_character(s: &str) -> Option<char>
┃   ┃   ┣━━ trait Drawable
┃   ┃   ┣━━ fn draw(&self)
┃   ┃   ┣━━ impl Drawable for Point
┃   ┃   ┣━━ fn draw(&self)
┃   ┃   ┣━━ fn main()
┃   ┃   ┣━━ pub struct VisibleStruct
┃   ┃   ┣━━ mod my_module
┃   ┃   ┣━━ macro_rules! say_hello
┃   ┃   ┣━━ pub mod lib
┃   ┃   ┣━━ pub mod interfaces
┃   ┃   ┣━━ mod engine
┃   ┃   ┗━━ pub fn flow<S1, S2, S3, S4, E, T, L>(
┃   ┃           source: S1,
┃   ┃           extractor: E,
┃   ┃           inbox: S2,
┃   ┃           transformer: T,
┃   ┃           outbox: S3,
┃   ┃           loader: L,
┃   ┃           sink: &mut S4,
┃   ┃       ) -> Result<(), Box<dyn Error>>
┃   ┃       where
┃   ┃           S1: Extractable,
┃   ┃           S2: Extractable + Loadable,
┃   ┃           S3: Extractable + Loadable,
┃   ┃           S4: Loadable,
┃   ┃           E: Extractor<S1, S2>,
┃   ┃           T: Transformer<S2, S3>,
┃   ┃           L: Loader<S3, S4>,
┃   ┗━━ 📄 tf_test.tf (288 tokens, 39 lines)
┃       ┣━━ provider "aws"
┃       ┣━━ resource "aws_instance" "example"
┃       ┣━━ data "aws_ami" "ubuntu"
┃       ┣━━ variable "instance_type"
┃       ┣━━ output "instance_public_ip"
┃       ┣━━ locals
┃       ┗━━ module "vpc"
┗━━ 📁 group5 (11392 tokens, 1504 lines)
    ┣━━ 📄 ansible_test.yml (60 tokens, 15 lines)
    ┃   ┣━━ Install package
    ┃   ┣━━ Start service
    ┃   ┗━━ Create user
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
    ┃   ┗━━ class AppRoutingModule
    ┣━━ 📄 app.component.spec.ts (307 tokens, 48 lines)
    ┃   ┣━━ describe 'AppComponent'
    ┃   ┣━━     it should create the app
    ┃   ┣━━     it should welcome the user
    ┃   ┣━━     it should welcome 'Jimbo'
    ┃   ┗━━     it should request login if not logged in
    ┣━━ 📄 app.component.ts (243 tokens, 45 lines)
    ┃   ┣━━ class AppComponent
    ┃   ┣━━   checkSession
    ┃   ┣━━   async goToEvent
    ┃   ┗━━   valInvitedBy
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
    ┃   ┗━━ class AppModule
    ┣━━ 📄 checkbox_test.md (263 tokens, 22 lines)
    ┃   ┣━━ # My Checkbox Test
    ┃   ┣━━ ## My No Parens Test
    ┃   ┣━━ ## My Empty href Test
    ┃   ┣━━ ## My other url Test [Q&A]
    ┃   ┣━━ ## My other other url Test [Q&A]
    ┃   ┣━━ ## My 2nd other url Test [Q&A]
    ┃   ┣━━ ## My 3rd other url Test [Q&A]
    ┃   ┣━━ - [ ] Task 1
    ┃   ┣━━     - [ ] No Space Task 1.1
    ┃   ┣━━     - [ ] Two Spaces Task 1.2
    ┃   ┣━━         - [ ] Subtask 1.2.1
    ┃   ┣━━ - [ ] Task 2
    ┃   ┣━━ -  Task 3
    ┃   ┣━━     - [ ] Subtask 3.1
    ┃   ┣━━ -  Task 6
    ┃   ┣━━     -  Subtask 6.1
    ┃   ┗━━         - [ ] Handle edge cases
    ┣━━ 📄 checkbox_test.txt (276 tokens, 33 lines)
    ┃   ┣━━ - [ ] fix phone number format +1
    ┃   ┣━━ - [ ] add forgot password
    ┃   ┣━━ - [ ] ? add email verification
    ┃   ┣━━ - [ ] store token the right way
    ┃   ┣━━ - [ ] test nesting of checkboxes
    ┃   ┣━━ - [ ] user can use option to buy ticket at 2-referred price
    ┃   ┣━━ - [ ] CTA refer 2 people to get instant lower price
    ┃   ┗━━ - [ ] form to send referrals
    ┣━━ 📄 environment.test.ts (193 tokens, 19 lines)
    ┃   ┣━━ environment:
    ┃   ┣━━    production
    ┃   ┣━━    cognitoUserPoolId
    ┃   ┣━━    cognitoAppClientId
    ┃   ┗━━    apiurl
    ┣━━ 📄 k8s_test.yaml (171 tokens, 38 lines)
    ┃   ┣━━ apps/v1.Deployment -> my-app
    ┃   ┣━━ v1.Service -> my-service
    ┃   ┗━━ v1.ConfigMap -> my-config
    ┣━━ 📄 Makefile (882 tokens, 85 lines)
    ┃   ┣━━ include dotenv/dev.env
    ┃   ┣━━ .PHONY: dev
    ┃   ┣━━ dev
    ┃   ┣━━ services-down
    ┃   ┣━━ services-stop: services-down
    ┃   ┣━━ define CHECK_POSTGRES
    ┃   ┣━━ damage-report
    ┃   ┣━━ tail-logs
    ┃   ┗━━ cloud
    ┣━━ 📄 requirements_test.txt (40 tokens, 10 lines)
    ┃   ┣━━ psycopg2-binary
    ┃   ┣━━ pytest
    ┃   ┣━━ coverage
    ┃   ┣━━ flask
    ┃   ┣━━ flask_cors
    ┃   ┣━━ stripe
    ┃   ┣━━ pyjwt
    ┃   ┣━━ cognitojwt
    ┃   ┗━━ flask-lambda
    ┣━━ 📄 rust_todo_test.rs (109 tokens, 27 lines)
    ┃   ┣━━ TODO (Line 23): This todo tests parse_todo
    ┃   ┣━━ enum Color
    ┃   ┣━━ struct Point
    ┃   ┣━━ trait Drawable
    ┃   ┣━━ fn draw(&self)
    ┃   ┣━━ impl Drawable for Point
    ┃   ┣━━ fn draw(&self)
    ┃   ┗━━ fn main()
    ┣━━ 📄 sql_test.sql (300 tokens, 52 lines)
    ┃   ┣━━ CREATE TABLE promoters
    ┃   ┣━━    user_id serial PRIMARY KEY,
    ┃   ┣━━    type varchar(20) NOT NULL,
    ┃   ┣━━    username varchar(20) NOT NULL,
    ┃   ┣━━    password varchar(20) NOT NULL,
    ┃   ┣━━    email varchar(30) NOT NULL,
    ┃   ┣━━    phone varchar(20) NOT NULL,
    ┃   ┣━━    promocode varchar(20),
    ┃   ┣━━    info json,
    ┃   ┣━━    going text[],
    ┃   ┣━━    invites text[],
    ┃   ┣━━    balance integer NOT NULL,
    ┃   ┣━━    rewards text[],
    ┃   ┣━━    created timestamp
    ┃   ┣━━ CREATE TABLE events
    ┃   ┣━━    event_id serial PRIMARY KEY,
    ┃   ┣━━    name varchar(64) NOT NULL,
    ┃   ┣━━    date varchar(64) NOT NULL,
    ┃   ┣━━    location varchar(64) NOT NULL,
    ┃   ┣━━    performer varchar(64) NOT NULL,
    ┃   ┣━━    rewards json,
    ┃   ┗━━    created timestamp
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
    ┣━━ 📄 test.env (242 tokens, 26 lines)
    ┃   ┣━━ PROMO_PATH
    ┃   ┣━━ PRODUCTION
    ┃   ┣━━ SQL_SCHEMA_PATH
    ┃   ┣━━ DB_LOGS
    ┃   ┣━━ DB_LOG
    ┃   ┣━━ PGPASSWORD
    ┃   ┣━━ PGDATABASE
    ┃   ┣━━ PGHOST
    ┃   ┣━━ PGPORT
    ┃   ┣━━ PGUSER
    ┃   ┣━━ SERVER_LOG
    ┃   ┣━━ SERVER_LOGS
    ┃   ┣━━ API_URL
    ┃   ┣━━ APP_LOGS
    ┃   ┣━━ APP_LOG
    ┃   ┣━━ APP_URL
    ┃   ┣━━ COGNITO_USER_POOL_ID
    ┃   ┣━━ COGNITO_APP_CLIENT_ID
    ┃   ┣━━ AWS_REGION
    ┃   ┗━━ STRIPE_SECRET_KEY
    ┣━━ 📄 testJsonSchema.json (294 tokens, 49 lines)
    ┃   ┣━━ $schema: http://json-schema.org/draft-07/schema#
    ┃   ┣━━ type: object
    ┃   ┣━━ title: random_test
    ┃   ┗━━ description: A promoter's activites related to events
    ┣━━ 📄 testPackage.json (458 tokens, 44 lines)
    ┃   ┣━━ name: 'promo-app'
    ┃   ┣━━ version: 0.0.0
    ┃   ┣━━ scripts:
    ┃   ┣━━     ng: 'ng'
    ┃   ┣━━     start: 'ng serve'
    ┃   ┣━━     build: 'ng build'
    ┃   ┣━━     watch: 'ng build --watch --configuration development'
    ┃   ┗━━     test: 'ng test'
    ┗━━ 📄 tickets.component.ts (6950 tokens, 903 lines)
        ┣━━ interface EnrichedTicket extends Ticket
        ┣━━ interface SpinConfig
        ┣━━ interface RotationState
        ┣━━ interface SpeakInput
        ┣━━ const formatSpeakInput: =>
        ┣━━ function hourToSpeech
        ┣━━ class TicketsComponent implements AfterViewInit
        ┣━━   speak
        ┣━━   speakEvent
        ┣━━   formatEvent
        ┣━━   speakVenue
        ┣━━   formatDate
        ┣━━   formatDateForSpeech
        ┣━━   async spinQRCode
        ┣━━   ngAfterViewInit
        ┣━━   ngOnDestroy
        ┣━━   toggleColumn
        ┣━━   adjustColumns
        ┣━━   onResize
        ┣━━   async ngOnInit
        ┣━━   async loadTickets
        ┣━━   onDateRangeChange
        ┣━━   applyFilter
        ┣━━   formatDateForComparison
        ┣━━   onFilterChange
        ┣━━   onLatitudeChange
        ┣━━   onLongitudeChange
        ┣━━   onRadiusChange
        ┣━━   sortData
        ┣━━   onRowClick
        ┣━━ function isDate
        ┣━━ function isNonNullNumber
        ┣━━ function hasLocation
        ┣━━ const create_faker_ticket: async =>
        ┣━━ function compare
        ┣━━ function compare_dates
        ┣━━ async function mockMoreTickets
        ┣━━ const mockTickets: async =>
        ┗━━ const renderQRCode: async =>
```

## Got Globs?

```sh
(py310) [2023-12-24T19:59:31-0500]
~/hax/tree_plus (main) $
tree_plus -g "*.*s" -i group_todo tests/more_languages
paths=('tests/more_languages',)
📁 more_languages (9017 tokens, 1245 lines)
┣━━ 📁 group4 (611 tokens, 115 lines)
┃   ┗━━ 📄 rust_test.rs (611 tokens, 115 lines)
┃       ┣━━ enum Days
┃       ┣━━ struct Point
┃       ┣━━ impl Point
┃       ┣━━ fn get_origin() -> Point
┃       ┣━━ struct Person
┃       ┣━━ impl Person
┃       ┣━━ fn greet(&self)
┃       ┣━━ fn add_two_longs(x1: i64, x2: i64) -> i64
┃       ┣━━ fn add_two_longs_longer(
┃       ┃       x1: i64,
┃       ┃       x2: i64,
┃       ┃   ) -> i64
┃       ┣━━ fn multiply_by_two(num: f64) -> f64
┃       ┣━━ fn get_first_character(s: &str) -> Option<char>
┃       ┣━━ trait Drawable
┃       ┣━━ fn draw(&self)
┃       ┣━━ impl Drawable for Point
┃       ┣━━ fn draw(&self)
┃       ┣━━ fn main()
┃       ┣━━ pub struct VisibleStruct
┃       ┣━━ mod my_module
┃       ┣━━ macro_rules! say_hello
┃       ┣━━ pub mod lib
┃       ┣━━ pub mod interfaces
┃       ┣━━ mod engine
┃       ┗━━ pub fn flow<S1, S2, S3, S4, E, T, L>(
┃               source: S1,
┃               extractor: E,
┃               inbox: S2,
┃               transformer: T,
┃               outbox: S3,
┃               loader: L,
┃               sink: &mut S4,
┃           ) -> Result<(), Box<dyn Error>>
┃           where
┃               S1: Extractable,
┃               S2: Extractable + Loadable,
┃               S3: Extractable + Loadable,
┃               S4: Loadable,
┃               E: Extractor<S1, S2>,
┃               T: Transformer<S2, S3>,
┃               L: Loader<S3, S4>,
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
    ┃   ┗━━ class AppRoutingModule
    ┣━━ 📄 app.component.spec.ts (307 tokens, 48 lines)
    ┃   ┣━━ describe 'AppComponent'
    ┃   ┣━━     it should create the app
    ┃   ┣━━     it should welcome the user
    ┃   ┣━━     it should welcome 'Jimbo'
    ┃   ┗━━     it should request login if not logged in
    ┣━━ 📄 app.component.ts (243 tokens, 45 lines)
    ┃   ┣━━ class AppComponent
    ┃   ┣━━   checkSession
    ┃   ┣━━   async goToEvent
    ┃   ┗━━   valInvitedBy
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
    ┃   ┗━━ class AppModule
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
    ┃   ┣━━ fn draw(&self)
    ┃   ┣━━ impl Drawable for Point
    ┃   ┣━━ fn draw(&self)
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
        ┣━━ const formatSpeakInput: =>
        ┣━━ function hourToSpeech
        ┣━━ class TicketsComponent implements AfterViewInit
        ┣━━   speak
        ┣━━   speakEvent
        ┣━━   formatEvent
        ┣━━   speakVenue
        ┣━━   formatDate
        ┣━━   formatDateForSpeech
        ┣━━   async spinQRCode
        ┣━━   ngAfterViewInit
        ┣━━   ngOnDestroy
        ┣━━   toggleColumn
        ┣━━   adjustColumns
        ┣━━   onResize
        ┣━━   async ngOnInit
        ┣━━   async loadTickets
        ┣━━   onDateRangeChange
        ┣━━   applyFilter
        ┣━━   formatDateForComparison
        ┣━━   onFilterChange
        ┣━━   onLatitudeChange
        ┣━━   onLongitudeChange
        ┣━━   onRadiusChange
        ┣━━   sortData
        ┣━━   onRowClick
        ┣━━ function isDate
        ┣━━ function isNonNullNumber
        ┣━━ function hasLocation
        ┣━━ const create_faker_ticket: async =>
        ┣━━ function compare
        ┣━━ function compare_dates
        ┣━━ async function mockMoreTickets
        ┣━━ const mockTickets: async =>
        ┗━━ const renderQRCode: async =>
```

## Languages Todo:

Help me **add to** and **priorize** this list of languages to support!

- [Open a `tree_plus` GitHub Issue](https://github.com/bionicles/tree_plus/issues/new), or
- [Contact Bion on X](https://twitter.com/flesheatingemu)

```sh
(py310) [2023-12-24T20:00:51-0500]
~/hax/tree_plus (main) $
tree_plus tests/more_languages/group_todo
paths=('tests/more_languages/group_todo',)
📁 group_todo (1549 tokens, 284 lines)
┣━━ 📄 clojure_test.clj (59 tokens, 10 lines)
┣━━ 📄 crystal_test.cr (56 tokens, 15 lines)
┣━━ 📄 csharp_test.cs (397 tokens, 72 lines)
┣━━ 📄 dart_test.dart (106 tokens, 24 lines)
┣━━ 📄 elixir_test.exs (49 tokens, 10 lines)
┣━━ 📄 erl_test.erl (60 tokens, 9 lines)
┣━━ 📄 fortran_test.f90 (114 tokens, 21 lines)
┣━━ 📄 fsharp_test.fs (44 tokens, 6 lines)
┣━━ 📄 haskell_test.hs (34 tokens, 5 lines)
┣━━ 📄 mathematica_test.nb (132 tokens, 21 lines)
┣━━ 📄 nodemon.json (120 tokens, 21 lines)
┣━━ 📄 RTest.R (53 tokens, 9 lines)
┣━━ 📄 sas_test.sas (104 tokens, 22 lines)
┣━━ 📄 testTypings.d.ts (149 tokens, 23 lines)
┗━━ 📄 vba_test.bas (72 tokens, 16 lines)
```

## Oppose Unfair Business Practices

Please consider contacting the authorities to report the issue described in this document:

[California OpenAI Complaint - Customer Noncompete Clause](https://www.tinyurl.com/cali-openai-complaint)

_Remember_: **Your Voice Matters!**

## License

MIT or Apache 2.0, at your option.