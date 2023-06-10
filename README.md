# Tree Plus

A simple command line interface (CLI) tool for developers to show a `tree` enhanced with token counts, line counts, and source code components.

## Example Output:
- [ ] Demonstrate Parsed Checkboxes
```sh
(py310) bion@WIN-QVRBL09D89C:~/hax/tree_plus$ tree_plus -i tests
📁 tree_plus (21768 tokens, 2113 lines)
┣━━ 📁 .github
┃   ┗━━ 📁 workflows (128 tokens, 26 lines)
┃       ┗━━ 📄 python-app.yml (128 tokens, 26 lines)
┃           ┣━━ Tree Plus CI
┃           ┣━━   job: build
┃           ┣━━     - Set up Python
┃           ┣━━     - Install dependencies
┃           ┗━━     - Run Tests
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

```sh
# Install Tree Plus
# Note: This assumes you already have installed on your machine:
# conda (miniconda3: https://docs.conda.io/en/latest/miniconda.html)
# GitHub CLI (gh: https://cli.github.com/manual/installation)

# Create and activate a conda environment for Python 3.10
conda create --name py310 python=3.10
conda activate py310
```

```sh
# TLDR one liner to install tree_plus:
gh repo clone https://github.com/bionicles/tree_plus.git && cd tree_plus && make cli
```

```sh
# The above is equivalent to:

# Clone the repository
gh repo clone https://github.com/bionicles/tree_plus.git
cd tree_plus

# If you have `make` installed, you can use the Makefile to install `tree_plus`
make cli

# If `make` is not installed, use `pip` instead
# pip install -e .
```

```.bash_profile
# if you want, you can alias tree_plus to tp like this
alias tp="tree_plus"
# usage: tp

# to reinstall tree_plus quickly after making changes, use this alias
alias tpcli="(cd ~/hax/tree_plus && make cli)"
# usage: tpcli
```

## Moar Usage

Multiple directories:

```sh
(py310) bion@WIN-QVRBL09D89C:~/hax/tree_plus$ tree_plus tests/path_to_test,tests/more_languages
(py310) bion@WIN-QVRBL09D89C:~/hax/tree_plus$ tp tests/path_to_test,tests/more_languages
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
┗━━ 📁 more_languages (4273 tokens, 835 lines)
    ┣━━ 📁 group1 (402 tokens, 88 lines)
    ┃   ┣━━ 📄 KotlinTest.kt (29 tokens, 4 lines)
    ┃   ┃   ┣━━ data class Person(val name: String)
    ┃   ┃   ┗━━ fun greet(person: Person)
    ┃   ┣━━ 📄 JavaTest.java (47 tokens, 12 lines)
    ┃   ┃   ┣━━ class Person
    ┃   ┃   ┣━━ class Person -> Person(String name)
    ┃   ┃   ┗━━ class Person -> void greet()
    ┃   ┣━━ 📄 LispTest.lisp (31 tokens, 5 lines)
    ┃   ┃   ┣━━ defstruct person
    ┃   ┃   ┗━━ defun greet
    ┃   ┣━━ 📄 ObjectiveCTest.m (59 tokens, 16 lines)
    ┃   ┃   ┣━━ @interface HelloWorld
    ┃   ┃   ┣━━ @interface HelloWorld -> (void) sayHello
    ┃   ┃   ┣━━ @implementation HelloWorld
    ┃   ┃   ┣━━ @implementation HelloWorld -> (void) sayHello
    ┃   ┃   ┗━━ void sayHelloWorld()
    ┃   ┣━━ 📄 LuaTest.lua (84 tokens, 16 lines)
    ┃   ┃   ┣━━ function HelloWorld.new
    ┃   ┃   ┣━━ function HelloWorld.greet
    ┃   ┃   ┗━━ function say_hello
    ┃   ┣━━ 📄 JuliaTest.jl (42 tokens, 12 lines)
    ┃   ┃   ┣━━ module JuliaTest
    ┃   ┃   ┣━━ module JuliaTest -> struct Person
    ┃   ┃   ┗━━ module JuliaTest -> greet(p::Person)
    ┃   ┣━━ 📄 OcamlTest.ml (53 tokens, 12 lines)
    ┃   ┃   ┣━━ type color
    ┃   ┃   ┣━━ class hello
    ┃   ┃   ┣━━ class hello -> method say_hello
    ┃   ┃   ┗━━ let main ()
    ┃   ┗━━ 📄 COBOL_TEST.CBL (57 tokens, 11 lines)
    ┃       ┣━━ IDENTIFICATION DIVISION -> PROGRAM-ID. HELLO
    ┃       ┣━━ DATA DIVISION -> 01 GREETING
    ┃       ┗━━ PROCEDURE DIVISION
    ┣━━ 📁 group2 (829 tokens, 178 lines)
    ┃   ┣━━ 📄 ScalaTest.scala (136 tokens, 24 lines)
    ┃   ┃   ┣━━ def sumOfSquares(x: Int, y: Int): Int
    ┃   ┃   ┣━━ trait Bark
    ┃   ┃   ┣━━ trait Bark -> def bark: String
    ┃   ┃   ┣━━ case class Person(name: String)
    ┃   ┃   ┣━━ object HelloWorld
    ┃   ┃   ┣━━ object HelloWorld -> def greet(person: Person): Unit
    ┃   ┃   ┣━━ object HelloWorld -> def main(args: Array[String]): Unit
    ┃   ┃   ┗━━ def sumOfSquaresShort(x: Int, y: Int): Int
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
    ┃   ┣━━ 📄 apl_test.apl (44 tokens, 5 lines)
    ┃   ┃   ┣━━ :Namespace HelloWorld
    ┃   ┃   ┣━━ :Namespace HelloWorld -> hello ← 'Hello, World!'
    ┃   ┃   ┗━━ :Namespace HelloWorld -> plus ← {⍺+⍵}
    ┃   ┣━━ 📄 RTest.R (53 tokens, 9 lines)
    ┃   ┣━━ 📄 PowershellTest.ps1 (169 tokens, 27 lines)
    ┃   ┃   ┣━━ function Test-Ordering($foo)
    ┃   ┃   ┣━━ class Person
    ┃   ┃   ┣━━ class Person -> Person($name)
    ┃   ┃   ┣━━ class Person -> Greet()
    ┃   ┃   ┣━━ class Person -> GreetMany($times)
    ┃   ┃   ┣━━ class Person -> NoReturn($times)
    ┃   ┃   ┣━━ class Person -> NoReturnNoArgs()
    ┃   ┃   ┗━━ function Say-Hello([Person]$person)
    ┃   ┣━━ 📄 PhpTest.php (74 tokens, 19 lines)
    ┃   ┃   ┣━━ class HelloWorld
    ┃   ┃   ┣━━ class HelloWorld -> function sayHello
    ┃   ┃   ┣━━ function greet
    ┃   ┃   ┣━━ class Person
    ┃   ┃   ┗━━ class Person -> function __construct
    ┃   ┣━━ 📄 PerlTest.pl (75 tokens, 20 lines)
    ┃   ┃   ┣━━ package PerlTest
    ┃   ┃   ┣━━ package PerlTest -> sub new
    ┃   ┃   ┣━━ package PerlTest -> sub hello
    ┃   ┃   ┗━━ package PerlTest -> sub say_hello
    ┃   ┗━━ 📄 bash_test.sh (22 tokens, 5 lines)
    ┣━━ 📁 group3 (941 tokens, 188 lines)
    ┃   ┣━━ 📄 elixir_test.exs (49 tokens, 10 lines)
    ┃   ┣━━ 📄 dart_test.dart (106 tokens, 24 lines)
    ┃   ┣━━ 📄 clojure_test.clj (59 tokens, 10 lines)
    ┃   ┣━━ 📄 fortran_test.f90 (114 tokens, 21 lines)
    ┃   ┣━━ 📄 cpp_test.cpp (100 tokens, 27 lines)
    ┃   ┣━━ 📄 csharp_test.cs (397 tokens, 72 lines)
    ┃   ┣━━ 📄 erl_test.erl (60 tokens, 9 lines)
    ┃   ┗━━ 📄 crystal_test.cr (56 tokens, 15 lines)
    ┣━━ 📁 group4 (1224 tokens, 246 lines)
    ┃   ┣━━ 📄 haskell_test.hs (34 tokens, 5 lines)
    ┃   ┣━━ 📄 matlab_test.m (45 tokens, 12 lines)
    ┃   ┃   ┣━━ classdef HelloWorld -> function greet
    ┃   ┃   ┗━━ function loneFun
    ┃   ┣━━ 📄 mathematica_test.nb (132 tokens, 21 lines)
    ┃   ┣━━ 📄 swift_test.swift (92 tokens, 22 lines)
    ┃   ┣━━ 📄 sas_test.sas (104 tokens, 22 lines)
    ┃   ┣━━ 📄 ruby_test.rb (50 tokens, 13 lines)
    ┃   ┣━━ 📄 fsharp_test.fs (44 tokens, 6 lines)
    ┃   ┣━━ 📄 tf_test.tf (288 tokens, 39 lines)
    ┃   ┃   ┣━━ provider "aws"
    ┃   ┃   ┣━━ resource "aws_instance" "example"
    ┃   ┃   ┣━━ data "aws_ami" "ubuntu"
    ┃   ┃   ┣━━ variable "instance_type"
    ┃   ┃   ┣━━ output "instance_public_ip"
    ┃   ┃   ┣━━ locals
    ┃   ┃   ┗━━ module "vpc"
    ┃   ┣━━ 📄 go_test.go (59 tokens, 16 lines)
    ┃   ┣━━ 📄 vba_test.bas (72 tokens, 16 lines)
    ┃   ┗━━ 📄 rust_test.rs (304 tokens, 74 lines)
    ┃       ┣━━ enum Days
    ┃       ┣━━ struct Point
    ┃       ┣━━ impl Point
    ┃       ┣━━ impl Point :: get_origin() -> Point
    ┃       ┣━━ struct Person
    ┃       ┣━━ impl Person
    ┃       ┣━━ impl Person :: greet(&self)
    ┃       ┣━━ fn add_two_longs(x1: i64, x2: i64) -> i64
    ┃       ┣━━ fn multiply_by_two(num: f64) -> f64
    ┃       ┣━━ fn get_first_character(s: &str) -> Option<char>
    ┃       ┣━━ trait Drawable
    ┃       ┣━━ trait Drawable :: draw(&self)
    ┃       ┣━━ impl Drawable for Point
    ┃       ┣━━ impl Drawable for Point :: draw(&self)
    ┃       ┣━━ fn main()
    ┃       ┣━━ pub struct VisibleStruct
    ┃       ┣━━ mod my_module
    ┃       ┗━━ macro_rules! say_hello
    ┗━━ 📁 group5 (877 tokens, 135 lines)
        ┣━━ 📄 rust_todo_test.rs (109 tokens, 27 lines)
        ┃   ┣━━ TODO (Line 23): This todo tests parse_todo
        ┃   ┣━━ enum Color
        ┃   ┣━━ struct Point
        ┃   ┣━━ trait Drawable
        ┃   ┣━━ trait Drawable :: draw(&self)
        ┃   ┣━━ impl Drawable for Point
        ┃   ┣━━ impl Drawable for Point :: draw(&self)
        ┃   ┗━━ fn main()
        ┣━━ 📄 checkbox_test.md (261 tokens, 22 lines)
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
        ┣━━ 📄 k8s_test.yaml (171 tokens, 38 lines)
        ┃   ┣━━ apps/v1.Deployment -> my-app
        ┃   ┣━━ v1.Service -> my-service
        ┃   ┗━━ v1.ConfigMap -> my-config
        ┣━━ 📄 checkbox_test.txt (276 tokens, 33 lines)
        ┃   ┣━━ - [ ] fix phone number format +1
        ┃   ┣━━ - [ ] add forgot password
        ┃   ┣━━ - [ ] ? add email verification
        ┃   ┣━━ - [ ] store token the right way
        ┃   ┣━━ - [ ] test nesting of checkboxes
        ┃   ┣━━ - [ ] user can use option to buy ticket at 2-referred price
        ┃   ┣━━ - [ ] CTA refer 2 people to get instant lower price
        ┃   ┗━━ - [ ] form to send referrals
        ┗━━ 📄 ansible_test.yml (60 tokens, 15 lines)
            ┣━━ Install package
            ┣━━ Start service
            ┗━━ Create user
```
