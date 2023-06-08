# tests/test_more_language_units.py
import pytest

from tree_plus_src import parse_file


# @pytest.mark.parametrize(
#     "file,expected",
#     [
# (
#     "tests/more_languages/group1/COBOL_TEST.CBL",
#     [
#         "IDENTIFICATION DIVISION -> PROGRAM-ID. HELLO",
#         "DATA DIVISION -> 01 GREETING",
#         "PROCEDURE DIVISION",
#     ],
# ),
# (
#     "tests/more_languages/group1/JavaTest.java",
#     [
#         "class Person",
#         "class Person -> Person(String name)",
#         "class Person -> void greet()",
#     ],
# ),
# (
#     "tests/more_languages/group1/JuliaTest.jl",
#     [
#         "module JuliaTest",
#         "module JuliaTest -> struct Person",
#         "module JuliaTest -> greet(p::Person)",
#     ],
# ),
# (
#     "tests/more_languages/group1/KotlinTest.kt",
#     [
#         "data class Person(val name: String)",
#         "fun greet(person: Person)",
#     ],
# ),
# (
#     "tests/more_languages/group1/LispTest.lisp",
#     [
#         "defstruct person",
#         "defun greet",
#     ],
# ),
# (
#     "tests/more_languages/group1/LuaTest.lua",
#     [
#         "function HelloWorld.new",
#         "function HelloWorld.greet",
#         "function say_hello",
#     ],
# ),
# (
#     "tests/more_languages/group1/ObjectiveCTest.m",
#     [
#         "@interface HelloWorld",
#         "@interface HelloWorld -> (void) sayHello",
#         "@implementation HelloWorld",
#         "@implementation HelloWorld -> (void) sayHello",
#         "void sayHelloWorld()",
#     ],
# ),
# (
#     "tests/more_languages/group1/OcamlTest.ml",
#     [
#         "type color",
#         "class hello",
#         "class hello -> method say_hello",
#         "let main ()",
#     ],
# ),
#     ],
# )
# def test_more_languages_group1(file, expected):
#     result = parse_file(file)
#     print(f"{result=}")
#     print(f"{expected=}")
#     assert result == expected


@pytest.mark.parametrize(
    "file,expected",
    [
        # (
        #     "tests/more_languages/group2/apl_test.apl",
        #     [
        #         ":Namespace HelloWorld",
        #         ":Namespace HelloWorld -> hello ← 'Hello, World!'",
        #         ":Namespace HelloWorld -> plus ← {⍺+⍵}",
        #     ],
        # ),
        # (
        #     "tests/more_languages/group2/PerlTest.pl",
        #     [
        #         "package PerlTest",
        #         "package PerlTest -> sub new",
        #         "package PerlTest -> sub hello",
        #         "package PerlTest -> sub say_hello",
        #     ],
        # ),
        # (
        #     "tests/more_languages/group2/PhpTest.php",
        #     [
        #         "class HelloWorld",
        #         "class HelloWorld -> function sayHello",
        #         "function greet",
        #         "class Person",
        #         "class Person -> function __construct",
        #     ],
        # ),
        #         (
        #             "tests/more_languages/group2/RTest.R",
        #             [
        #                 "person <- list(name = 'John Doe', age = 50)",
        #                 "greet.Person <- function(p)",
        #             ],
        #         ),
        # (
        #     "tests/more_languages/group2/ScalaTest.scala",
        #     [
        #         "def sumOfSquares(x: Int, y: Int): Int",
        #         "trait Bark",
        #         "trait Bark -> def bark: String",
        #         "case class Person(name: String)",
        #         "object HelloWorld",
        #         "object HelloWorld -> def greet(person: Person): Unit",
        #         "object HelloWorld -> def main(args: Array[String]): Unit",
        #         "def sumOfSquaresShort(x: Int, y: Int): Int",
        #     ],
        # ),
        #         (
        #             "tests/more_languages/group2/bash_test.sh",
        #             [
        #                 "echo_hello_world()",
        #             ],
        #         ),
        (
            "tests/more_languages/group2/c_test.c",
            [
                "struct Point",
                "struct Point getOrigin()",
                "float mul_two_floats(float x1, float x2)",
                "enum days",
                "long add_two_longs(long x1, long x2)",
                "double multiplyByTwo(double num)",
                "char getFirstCharacter(char *str)",
                "void greet(Person p)",
                "typedef struct Person",
                "int main()",
                "int* getArrayStart(int arr[], int size)",
            ],
        ),
        # (
        #     "tests/more_languages/group2/PowershellTest.ps1",
        #     [
        #         "function Test-Ordering([string]$foo)",
        #         "class Person",
        #         "class Person -> Person([string]$name)",
        #         "class Person -> [string]Greet()",
        #         "class Person -> [string]GreetMany([int]$times)",
        #         "class Person -> NoReturn([int]$times)",
        #         "class Person -> NoReturnNoArgs()",
        #         "function Say-Hello([Person]$person)",
        #     ],
        # ),
    ],
)
def test_more_languages_group2(file, expected):
    result = parse_file(file)
    print(f"{expected=}")
    print(f"{result=}")
    assert result == expected


# @pytest.mark.parametrize(
#     "file,expected",
#     [
#         (
#             "tests/more_languages/group3/clojure_test.clj",
#             ["defprotocol P", "defrecord Person", "defn -main"],
#         ),
#         (
#             "tests/more_languages/group3/cpp_test.cpp",
#             ["class Person", "void Person::greet", "void globalGreet", "int main"],
#         ),
#         (
#             "tests/more_languages/group3/crystal_test.cr",
#             ["class Person -> def greet", "def say_hello"],
#         ),
#         (
#             "tests/more_languages/group3/csharp_test.cs",
#             [
#                 "namespace HelloWorldApp -> class Person",
#                 "namespace HelloWorldApp -> class HelloWorld",
#                 "namespace HelloWorldApp -> static void Main",
#             ],
#         ),
#         (
#             "tests/more_languages/group3/dart_test.dart",
#             ["enum GreetingType", "class HelloWorld -> void sayHello", "void main"],
#         ),
#         (
#             "tests/more_languages/group3/elixir_test.exs",
#             ["defmodule Person", "defmodule HelloWorld -> def hello"],
#         ),
#         (
#             "tests/more_languages/group3/erl_test.erl",
#             ["-module(hello_world)", "-record(person)", "hello_world/0"],
#         ),
#         (
#             "tests/more_languages/group3/fortran_test.f90",
#             [
#                 "MODULE hello_mod -> TYPE person",
#                 "MODULE hello_mod -> SUBROUTINE say_hello",
#                 "PROGRAM HelloWorld",
#             ],
#         ),
#     ],
# )
# def test_more_languages_group3(file, expected):
#     result = parse_file(file)
#     print(f"{result=}")
#     print(f"{expected=}")
#     assert result == expected


@pytest.mark.parametrize(
    "file,expected",
    [
        #         (
        #             "tests/more_languages/group4/fsharp_test.fs",
        #             ["type Person -> member this.SayHello", "let person"],
        #         ),
        #         (
        #             "tests/more_languages/group4/go_test.go",
        #             ["type Greeting -> func (g Greeting) sayHello", "func createGreeting"],
        #         ),
        #         (
        #             "tests/more_languages/group4/haskell_test.hs",
        #             ["data Person", "greet :: Person -> String"],
        #         ),
        #         (
        #             "tests/more_languages/group4/mathematica_test.nb",
        #             [
        #                 "person[name_]",
        #                 'BeginPackage["TestModule"] -> sayHello::usage',
        #                 'BeginPackage["TestModule"] -> sumList::usage',
        #             ],
        #         ),
        # (
        #     "tests/more_languages/group4/matlab_test.m",
        #     ["classdef HelloWorld -> function greet", "function loneFun"],
        # ),
        #         (
        #             "tests/more_languages/group4/ruby_test.rb",
        #             [
        #                 "module Greeter -> def self.say_hello",
        #                 "class HelloWorld -> def say_hello",
        #             ],
        #         ),
        (
            "tests/more_languages/group4/rust_test.rs",
            [
                "enum Days",
                "struct Point",
                "impl Point",
                "impl Point :: get_origin() -> Point",
                "struct Person",
                "impl Person",
                "impl Person :: greet(&self)",
                "fn add_two_longs(x1: i64, x2: i64) -> i64",
                "fn multiply_by_two(num: f64) -> f64",
                "fn get_first_character(s: &str) -> Option<char>",
                "trait Drawable",
                "trait Drawable :: draw(&self)",
                "impl Drawable for Point",
                "impl Drawable for Point :: draw(&self)",
                "fn main()",
                "pub struct VisibleStruct",
                "mod my_module",
                "macro_rules! say_hello",
            ],
        ),
        #         (
        #             "tests/more_languages/group4/sas_test.sas",
        #             ["data work.testData", "%macro sayHello", "PROC SQL"],
        #         ),
        #         (
        #             "tests/more_languages/group4/swift_test.swift",
        #             ["class Person -> func greet", "struct CustomType"],
        #         ),
        #         (
        #             "tests/more_languages/group4/vba_test.bas",
        #             [
        #                 "Class CPerson -> Public Property Get Name",
        #                 "Class CPerson -> Public Property Let Name",
        #                 "Class CPerson -> Public Sub Greet",
        #             ],
        #         ),
    ],
)
def test_more_languages_group4(file, expected):
    result = parse_file(file)
    print(f"{result=}")
    print(f"{expected=}")
    assert result == expected
