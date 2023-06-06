(py310) bion@WIN-QVRBL09D89C:~/hax/tree_plus$ tp tree_plus_src,tests
Multiple Directories:
┣━━ tree_plus_src (6168 tokens, 785 lines)
┃   ┗━━ 📁 tree_plus_src (6168 tokens, 785 lines)
┃       ┣━━ 📄 __init__.py (64 tokens, 9 lines)
┃       ┣━━ 📄 count_tokens_lines.py (400 tokens, 53 lines)
┃       ┃   ┣━━ class TokenLineCount
┃       ┃   ┣━━ def count_tokens_lines
┃       ┃   ┗━━ def count_directory_tokens_lines
┃       ┣━━ 📄 traverse_directory.py (254 tokens, 37 lines)
┃       ┃   ┗━━ def traverse_directory
┃       ┣━━ 📄 default_ignore.py (36 tokens, 8 lines)
┃       ┗━━ 📄 parse_file.py (5414 tokens, 678 lines)
┃           ┣━━ def parse_file
┃           ┣━━ def extract_nodes
┃           ┣━━ def is_typing_construct
┃           ┣━━ def is_builtin_type
┃           ┣━━ def parse_py
┃           ┣━━ def parse_db
┃           ┣━━ def parse_cobol
┃           ┣━━ def parse_java
┃           ┣━━ def parse_julia
┃           ┣━━ def parse_kotlin
┃           ┣━━ def parse_lisp
┃           ┣━━ def parse_lua
┃           ┣━━ def parse_objective_c
┃           ┣━━ def parse_ocaml
┃           ┣━━ def parse_apl
┃           ┣━━ def parse_perl
┃           ┣━━ def parse_php
┃           ┣━━ def parse_powershell
┃           ┣━━ def parse_matlab
┃           ┣━━ def parse_scala
┃           ┣━━ def parse_c
┃           ┣━━ def parse_js
┃           ┣━━ def parse_md
┃           ┗━━ def parse_todo
┗━━ tests (9186 tokens, 1304 lines)
    ┣━━ 📁 tests (6458 tokens, 708 lines)
    ┃   ┣━━ 📄 __init__.py (1 tokens, 1 lines)
    ┃   ┣━━ 📄 test_more_languages_e2e.py (2315 tokens, 177 lines)
    ┃   ┣━━ 📄 performance.py (80 tokens, 11 lines)
    ┃   ┣━━ 📄 test_cli.py (129 tokens, 25 lines)
    ┃   ┃   ┣━━ def test_imports
    ┃   ┃   ┗━━ def test_main_entry_point
    ┃   ┣━━ 📄 test_units.py (541 tokens, 88 lines)
    ┃   ┃   ┣━━ def test_valid_directory
    ┃   ┃   ┣━━ def test_invalid_directory
    ┃   ┃   ┣━━ def test_file_as_directory
    ┃   ┃   ┣━━ def test_file_parsing
    ┃   ┃   ┣━━ def test_parse_todo
    ┃   ┃   ┣━━ def test_token_counting
    ┃   ┃   ┗━━ def test_directory_token_line_counting
    ┃   ┣━━ 📄 test_e2e.py (1205 tokens, 111 lines)
    ┃   ┃   ┣━━ def test_e2e_single_directory
    ┃   ┃   ┣━━ def test_e2e_multiple_directories
    ┃   ┃   ┣━━ def test_e2e_ignore_parameter_filetype
    ┃   ┃   ┗━━ def test_e2e_ignore_parameter_directory
    ┃   ┗━━ 📄 test_more_language_units.py (2187 tokens, 295 lines)
    ┃       ┗━━ def test_more_languages_group2
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
    ┣━━ 📁 group4 (710 tokens, 155 lines)
    ┃   ┣━━ 📄 haskell_test.hs (34 tokens, 5 lines)
    ┃   ┣━━ 📄 matlab_test.m (45 tokens, 12 lines)
    ┃   ┃   ┣━━ classdef HelloWorld -> function greet
    ┃   ┃   ┗━━ function loneFun
    ┃   ┣━━ 📄 mathematica_test.nb (132 tokens, 21 lines)
    ┃   ┣━━ 📄 swift_test.swift (61 tokens, 18 lines)
    ┃   ┣━━ 📄 sas_test.sas (104 tokens, 22 lines)
    ┃   ┣━━ 📄 ruby_test.rb (50 tokens, 12 lines)
    ┃   ┣━━ 📄 fsharp_test.fs (44 tokens, 6 lines)
    ┃   ┣━━ 📄 go_test.go (59 tokens, 16 lines)
    ┃   ┣━━ 📄 vba_test.bas (72 tokens, 16 lines)
    ┃   ┗━━ 📄 rust_test.rs (109 tokens, 27 lines)
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
    ┣━━ 📁 group3 (634 tokens, 137 lines)
    ┃   ┣━━ 📄 elixir_test.exs (49 tokens, 10 lines)
    ┃   ┣━━ 📄 dart_test.dart (106 tokens, 24 lines)
    ┃   ┣━━ 📄 clojure_test.clj (59 tokens, 10 lines)
    ┃   ┣━━ 📄 fortran_test.f90 (114 tokens, 21 lines)
    ┃   ┣━━ 📄 cpp_test.cpp (100 tokens, 27 lines)
    ┃   ┣━━ 📄 csharp_test.cs (90 tokens, 21 lines)
    ┃   ┣━━ 📄 erl_test.erl (60 tokens, 9 lines)
    ┃   ┗━━ 📄 crystal_test.cr (56 tokens, 15 lines)
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