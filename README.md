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
![Python 3.12](https://img.shields.io/badge/Python-3.12-blue)

A simple command line interface (CLI) tool for developers to show a `tree` enhanced with token counts, line counts, and source code components.

## Example Output:
- [ ] Demonstrate Parsed Checkboxes
```sh
~/hax/tree_plus (main) $
> tp -i tests
paths=('.',)
📁 tree_plus (11526 tokens, 1012 lines)
┣━━ 📁 .github
┃   ┗━━ 📁 workflows (523 tokens, 70 lines)
┃       ┣━━ 📄 microsoft.yml (272 tokens, 36 lines)
┃       ┃   ┣━━ Microsoft
┃       ┃   ┣━━   job: build
┃       ┃   ┣━━     - Set up Python ${{ matrix.python-version }}
┃       ┃   ┣━━     - Install tree_plus
┃       ┃   ┣━━     - Set PYTHONUTF8 for Windows
┃       ┃   ┣━━     - Run generic tests
┃       ┃   ┗━━     - Run specific test
┃       ┗━━ 📄 unix.yml (251 tokens, 34 lines)
┃           ┣━━ Ubuntu & MacOS
┃           ┣━━   job: build
┃           ┣━━     - Set up Python ${{ matrix.python-version }}
┃           ┣━━     - Install tree_plus
┃           ┣━━     - Run generic tests
┃           ┗━━     - Run specific test
┣━━ 📁 tree_plus_src (12997 tokens, 1542 lines)
┃   ┣━━ 📁 scripts (6661 tokens, 861 lines)
┃   ┃   ┣━━ 📄 alias_tree_plus.sh (277 tokens, 31 lines)
┃   ┃   ┣━━ 📄 Microsoft.PowerShell_profile.ps1 (3238 tokens, 419 lines)
┃   ┃   ┃   ┣━━ function Log($message)
┃   ┃   ┃   ┗━━ function Show-Error($err)
┃   ┃   ┣━━ 📄 profile.toml (1192 tokens, 126 lines)
┃   ┃   ┣━━ 📄 scratch.ps1 (1317 tokens, 197 lines)
┃   ┃   ┗━━ 📄 scratch.sh (637 tokens, 88 lines)
┃   ┣━━ 📄 count_tokens_lines.py (575 tokens, 78 lines)
┃   ┃   ┣━━ TODO (Line 11): show off how well we parse_todo!
┃   ┃   ┣━━ class TokenLineCount
┃   ┃   ┣━━ def add_tokens_lines
┃   ┃   ┣━━ def count_tokens_lines
┃   ┃   ┗━━ def count_directory_tokens_lines
┃   ┣━━ 📄 default_ignore.py (538 tokens, 94 lines)
┃   ┃   ┣━━ def make_ignore
┃   ┃   ┣━━ def is_binary_string
┃   ┃   ┗━━ def is_binary
┃   ┣━━ 📄 parse_file.py (11631 tokens, 1333 lines)
┃   ┃   ┣━━ TODO (Line 15): convert this to an environment variable and share across the modules
┃   ┃   ┣━━ def debug_print
┃   ┃   ┣━━ def parse_file
┃   ┃   ┣━━ def parse_d_dot_ts
┃   ┃   ┣━━ def parse_angular_app_module
┃   ┃   ┣━━ def parse_angular_component_ts
┃   ┃   ┣━━ def parse_angular_routes
┃   ┃   ┣━━ def parse_angular_spec
┃   ┃   ┣━━ def parse_environment_ts
┃   ┃   ┣━━ def parse_dot_env
┃   ┃   ┣━━ def parse_requirements_txt
┃   ┃   ┣━━ def parse_json_schema
┃   ┃   ┣━━ def parse_package_json
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
┃   ┃   ┣━━ def parse_c
┃   ┃   ┣━━ def parse_tf
┃   ┃   ┣━━ def parse_js
┃   ┃   ┣━━ def parse_md
┃   ┃   ┣━━ def parse_txt
┃   ┃   ┣━━ def parse_markers
┃   ┃   ┗━━ def parse_rs
┃   ┗━━ 📄 traverse_directory.py (253 tokens, 37 lines)
┃       ┗━━ def traverse_directory
┣━━ 📄 .gitignore (111 tokens, 32 lines)
┣━━ 📄 LICENSE (2123 tokens, 81 lines)
┣━━ 📄 Makefile (193 tokens, 26 lines)
┃   ┣━━ SHELL := /bin/bash
┃   ┣━━ debug
┃   ┣━━ .PHONY: debug_command
┃   ┣━━ debug_command: cli test
┃   ┣━━ cli
┃   ┣━━ test_s: test_tp_dotdot_s
┃   ┣━━ test_tp_dotdot_s: test_tp_dotdot
┃   ┣━━ test: test_tp_dotdot
┃   ┣━━ test_tp_dotdot
┃   ┗━━ move_powershell_profile
┣━━ 📄 nodemon.json (64 tokens, 13 lines)
┣━━ 📄 README.md (2682 tokens, 230 lines)
┃   ┣━━ # Tree Plus
┃   ┣━━ ## Example Output:
┃   ┣━━ - [ ] Demonstrate Parsed Checkboxes
┃   ┣━━ ## Start Quick!
┃   ┣━━ ### Prerequisites
┃   ┣━━ ### Install Tree Plus
┃   ┣━━ #### Local Hackable Install
┃   ┣━━ ### Usage
┃   ┗━━ ## Moar Languages
┣━━ 📄 sample_output.sh (3039 tokens, 184 lines)
┣━━ 📄 setup.py (608 tokens, 97 lines)
┃   ┗━━ TODO (Line 44): automatically install tree-sitter-{language} grammars
┣━━ 📄 TODO.md (393 tokens, 41 lines)
┃   ┣━━ # Do First
┃   ┣━━ # Backlog
┃   ┣━━ - [ ] Fix ordering of leaves by line number consistently across languages
┃   ┣━━     - [ ] Fix Python ordering
┃   ┣━━     - [ ] Fix JavaScript / TypeScript ordering
┃   ┣━━     - [ ] Review other language unit tests for ordering
┃   ┣━━ - [ ] expand test cases for the supported languages to verify ordering & completeness
┃   ┣━━ - [ ] Upgrade CICD GitHub Action
┃   ┣━━     - [ ] Build and deploy to PyPi if tests pass
┃   ┣━━ - [ ] test file input instead of directory
┃   ┣━━ - [ ] measure test coverage
┃   ┣━━ - [ ] test named lambdas in python
┃   ┣━━ - [ ] autoinstall tree-sitter grammars to enable real parsing instead of regex if available
┃   ┣━━ - [ ] modularize the tree_plus function to separate index creation from tree creation
┃   ┣━━ - [ ] brainstorm and integrate more flags / options to control results
┃   ┣━━     - [ ] add a todo flag to only show todos and unchecked checkboxes
┃   ┣━━     - [ ] add a search / filter input to filter results
┃   ┣━━ - [ ] make a tree_scan to apply AI LLMs to the tree
┃   ┣━━     - [ ] e.g. to suggest todos
┃   ┣━━     - [ ] to write docs
┃   ┣━━     - [ ] to write tests
┃   ┣━━     - [ ] to write code
┃   ┣━━     - [ ] to translate languages (e.g. 'rewrite it in rust' example)
┃   ┣━━ # Nice to have
┃   ┣━━ - [ ] Support more languages
┃   ┣━━     - [ ] SQL (create tables, procedures)
┃   ┣━━     - [ ] C#
┃   ┣━━     - [ ] C++
┃   ┣━━     - [ ] OpenAPI yaml
┃   ┣━━     - [ ] Json RPC 2.0 schemas
┃   ┣━━     - [ ] GRPC schemas
┃   ┣━━     - [ ] GraphQL schemas
┃   ┣━━     - [ ] Go
┃   ┣━━     - [ ] Dart
┃   ┣━━     - [ ] Swift
┃   ┗━━ # Done
┗━━ 📄 tree_plus_cli.py (2313 tokens, 308 lines)
    ┣━━ NOTE (Line 275): parent_count unused, is that ok?
    ┣━━ def debug_print
    ┣━━ def tree_to_string
    ┣━━ def clean_string
    ┣━━ def safe_print
    ┣━━ def main
    ┣━━ def tree_plus
    ┣━━ def _parse_paths
    ┣━━ def flatten_to_str
    ┣━━ def _handle_paths
    ┗━━ def _handle_path
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

## Moar Languages

```sh
/home/bion/hax/tree_plus/tests/more_languages (0 tokens, 0 lines)
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
┗━━ 📁 group5 (4442 tokens, 600 lines)
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
    ┣━━ 📄 app.component.ts (243 tokens, 44 lines)
    ┃   ┣━━ class AppComponent
    ┃   ┣━━     title: string = 'promo-app'
    ┃   ┣━━     user: any
    ┃   ┣━━     events: any
    ┃   ┣━━     events_list: any
    ┃   ┣━━     clientSecret: string
    ┃   ┣━━     card?: StripePaymentElementComponent
    ┃   ┣━━     cardOptions: StripePaymentElementOptions
    ┃   ┣━━     constructor(
    ┃   ┃           private http: HttpClient,
    ┃   ┃           private loginService: LoginService,
    ┃   ┃           private stripeService: StripeService
    ┃   ┃       )
    ┃   ┣━━     constructor(private loginService: LoginService)
    ┃   ┣━━     checkSession()
    ┃   ┣━━     async goToEvent(event_id: string)
    ┃   ┗━━     valInvitedBy(event: any, event_id: string)
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
    ┗━━ 📄 testPackage.json (458 tokens, 44 lines)
        ┣━━ name: 'promo-app'
        ┣━━ version: 0.0.0
        ┣━━ scripts:
        ┣━━     ng: 'ng'
        ┣━━     start: 'ng serve'
        ┣━━     build: 'ng build'
        ┣━━     watch: 'ng build --watch --configuration development'
        ┗━━     test: 'ng test'
```
