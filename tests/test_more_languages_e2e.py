# # tests/test_more_languages_e2e.py
# import pytest  # noqa: F401
# import rich

# from cli import tree_plus, tree_to_string

# EXPECTATION_GROUP1 = """tests/more_languages/group1 (N tokens, N lines)
# ┗━━ 📁 group1 (N tokens, N lines)
#     ┣━━ 📄 COBOL_TEST.CBL (N tokens, N lines)
#     ┃   ┣━━ IDENTIFICATION DIVISION -> PROGRAM-ID. HELLO
#     ┃   ┣━━ DATA DIVISION -> 01 GREETING
#     ┃   ┗━━ PROCEDURE DIVISION
#     ┣━━ 📄 JavaTest.java (N tokens, N lines)
#     ┃   ┣━━ class Person -> Person(String name)
#     ┃   ┗━━ class Person -> void greet()
#     ┣━━ 📄 JuliaTest.jl (N tokens, N lines)
#     ┃   ┣━━ module JuliaTest -> struct Person
#     ┃   ┗━━ module JuliaTest -> greet(p::Person)
#     ┣━━ 📄 KotlinTest.kt (N tokens, N lines)
#     ┃   ┣━━ data class Person(val name: String)
#     ┃   ┗━━ fun greet(person: Person)
#     ┣━━ 📄 LispTest.lisp (N tokens, N lines)
#     ┃   ┣━━ defstruct person
#     ┃   ┗━━ defun greet
#     ┣━━ 📄 LuaTest.lua (N tokens, N lines)
#     ┃   ┣━━ function HelloWorld.new(self, name)
#     ┃   ┣━━ function HelloWorld.greet(self)
#     ┃   ┗━━ function say_hello(name)
#     ┣━━ 📄 ObjectiveCTest.m (N tokens, N lines)
#     ┃   ┣━━ @interface HelloWorld -> (void) sayHello
#     ┃   ┣━━ @implementation HelloWorld -> (void) sayHello
#     ┃   ┗━━ void sayHelloWorld()
#     ┗━━ 📄 OcamlTest.ml (N tokens, N lines)
#         ┣━━ type color
#         ┣━━ class hello -> method say_hello
#         ┗━━ let main ()
# """


# EXPECTATION_GROUP2 = """tests/more_languages/group2 (N tokens, N lines)
# ┗━━ 📁 group2 (N tokens, N lines)
#     ┣━━ 📄 PerlTest.pl (N tokens, N lines)
#     ┃   ┣━━ package PerlTest -> sub new
#     ┃   ┣━━ package PerlTest -> sub hello
#     ┃   ┗━━ package PerlTest -> sub say_hello
#     ┣━━ 📄 PhpTest.php (N tokens, N lines)
#     ┃   ┣━━ class HelloWorld -> function sayHello
#     ┃   ┣━━ function greet
#     ┃   ┗━━ class Person -> function __construct
#     ┣━━ 📄 PowershellTest.ps1 (N tokens, N lines)
#     ┃   ┣━━ class Person -> Person([string]$name)
#     ┃   ┣━━ class Person -> [string]Greet()
#     ┃   ┗━━ function Say-Hello
#     ┣━━ 📄 RTest.R (N tokens, N lines)
#     ┃   ┣━━ person <- list(name = "John Doe", age = 50)
#     ┃   ┗━━ greet.Person <- function(p)
#     ┣━━ 📄 ScalaTest.scala (N tokens, N lines)
#     ┃   ┣━━ case class Person(name: String)
#     ┃   ┣━━ object HelloWorld -> def greet(person: Person)
#     ┃   ┗━━ object HelloWorld -> def main(args: Array[String])
#     ┣━━ 📄 apl_test.apl (N tokens, N lines)
#     ┃   ┣━━ :Namespace HelloWorld -> hello ← 'Hello, World!'
#     ┃   ┗━━ :Namespace HelloWorld -> plus ← {⍺+⍵}
#     ┣━━ 📄 bash_test.sh (N tokens, N lines)
#     ┃   ┗━━ echo_hello_world()
#     ┗━━ 📄 c_test.c (N tokens, N lines)
#         ┣━━ typedef struct -> greet(Person p)
#         ┗━━ main()
# """


# EXPECTATION_GROUP3 = """tests/more_languages/group3 (N tokens, N lines)
# ┗━━ 📁 group3 (N tokens, N lines)
#     ┣━━ 📄 clojure_test.clj (N tokens, N lines)
#     ┃   ┣━━ defprotocol P
#     ┃   ┣━━ defrecord Person
#     ┃   ┗━━ defn -main
#     ┣━━ 📄 cpp_test.cpp (N tokens, N lines)
#     ┃   ┣━━ class Person
#     ┃   ┣━━ void Person::greet
#     ┃   ┣━━ void globalGreet
#     ┃   ┗━━ int main
#     ┣━━ 📄 crystal_test.cr (N tokens, N lines)
#     ┃   ┣━━ class Person -> def greet
#     ┃   ┗━━ def say_hello
#     ┣━━ 📄 csharp_test.cs (N tokens, N lines)
#     ┃   ┣━━ namespace HelloWorldApp -> class Person
#     ┃   ┣━━ namespace HelloWorldApp -> class HelloWorld
#     ┃   ┗━━ namespace HelloWorldApp -> static void Main
#     ┣━━ 📄 dart_test.dart (N tokens, N lines)
#     ┃   ┣━━ enum GreetingType
#     ┃   ┣━━ class HelloWorld -> void sayHello
#     ┃   ┗━━ void main
#     ┣━━ 📄 elixir_test.exs (N tokens, N lines)
#     ┃   ┣━━ defmodule Person
#     ┃   ┗━━ defmodule HelloWorld -> def hello
#     ┣━━ 📄 erl_test.erl (N tokens, N lines)
#     ┃   ┣━━ -module(hello_world)
#     ┃   ┣━━ -record(person)
#     ┃   ┗━━ hello_world/0
#     ┗━━ 📄 fortran_test.f90 (N tokens, N lines)
#         ┣━━ MODULE hello_mod -> TYPE person
#         ┣━━ MODULE hello_mod -> SUBROUTINE say_hello
#         ┗━━ PROGRAM HelloWorld
# """


# EXPECTATION_GROUP4 = """tests/more_languages/group4 (N tokens, N lines)
# ┗━━ 📁 group4 (N tokens, N lines)
#     ┣━━ 📄 fsharp_test.fs (N tokens, N lines)
#     ┃   ┣━━ type Person -> member this.SayHello
#     ┃   ┗━━ let person
#     ┣━━ 📄 go_test.go (N tokens, N lines)
#     ┃   ┣━━ type Greeting -> func (g Greeting) sayHello
#     ┃   ┗━━ func createGreeting
#     ┣━━ 📄 haskell_test.hs (N tokens, N lines)
#     ┃   ┣━━ data Person
#     ┃   ┗━━ greet :: Person -> String
#     ┣━━ 📄 mathematica_test.nb (N tokens, N lines)
#     ┃   ┣━━ person[name_]
#     ┃   ┣━━ BeginPackage["TestModule"] -> sayHello::usage
#     ┃   ┗━━ BeginPackage["TestModule"] -> sumList::usage
#     ┣━━ 📄 matlab_test.m (N tokens, N lines)
#     ┃   ┣━━ classdef HelloWorld -> function greet
#     ┃   ┗━━ function loneFun
#     ┣━━ 📄 ruby_test.rb (N tokens, N lines)
#     ┃   ┣━━ module Greeter -> def self.say_hello
#     ┃   ┗━━ class HelloWorld -> def say_hello
#     ┣━━ 📄 rust_test.rs (N tokens, N lines)
#     ┃   ┣━━ TODO (Line N): This todo tests parse_todo
#     ┃   ┣━━ enum Color
#     ┃   ┣━━ struct Point
#     ┃   ┗━━ impl Drawable for Point -> fn draw
#     ┣━━ 📄 sas_test.sas (N tokens, N lines)
#     ┃   ┣━━ data work.testData
#     ┃   ┣━━ %macro sayHello
#     ┃   ┗━━ PROC SQL
#     ┣━━ 📄 swift_test.swift (N tokens, N lines)
#     ┃   ┣━━ class Person -> func greet
#     ┃   ┗━━ struct CustomType
#     ┗━━ 📄 vba_test.bas (N tokens, N lines)
#         ┣━━ Class CPerson -> Public Property Get Name
#         ┣━━ Class CPerson -> Public Property Let Name
#         ┗━━ Class CPerson -> Public Sub Greet
# """


# def test_e2e_more_languages_group1():
#     result = tree_plus("tests/more_languages/group1")
#     assert isinstance(result, rich.tree.Tree)
#     result_str = tree_to_string(result)
#     print(result_str)
#     assert result_str == EXPECTATION_GROUP1


# def test_e2e_more_languages_group2():
#     result = tree_plus("tests/more_languages/group2")
#     assert isinstance(result, rich.tree.Tree)
#     result_str = tree_to_string(result)
#     print(result_str)
#     assert result_str == EXPECTATION_GROUP2


# def test_e2e_more_languages_group3():
#     result = tree_plus("tests/more_languages/group3")
#     assert isinstance(result, rich.tree.Tree)
#     result_str = tree_to_string(result)
#     print(result_str)
#     assert result_str == EXPECTATION_GROUP3


# def test_e2e_more_languages_group4():
#     result = tree_plus("tests/more_languages/group4")
#     assert isinstance(result, rich.tree.Tree)
#     result_str = tree_to_string(result)
#     print(result_str)
#     assert result_str == EXPECTATION_GROUP4
