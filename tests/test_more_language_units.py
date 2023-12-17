# tests/test_more_language_units.py
import pytest

from rich import print

from tree_plus_src import parse_file


@pytest.mark.parametrize(
    "file,expected",
    [
        (
            "tests/more_languages/group1/COBOL_TEST.CBL",
            [
                "IDENTIFICATION DIVISION -> PROGRAM-ID. HELLO",
                "DATA DIVISION -> 01 GREETING",
                "PROCEDURE DIVISION",
            ],
        ),
        (
            "tests/more_languages/group1/JavaTest.java",
            [
                "class Person",
                "class Person -> Person(String name)",
                "class Person -> void greet()",
            ],
        ),
        (
            "tests/more_languages/group1/JuliaTest.jl",
            [
                "module JuliaTest",
                "module JuliaTest -> struct Person",
                "module JuliaTest -> greet(p::Person)",
            ],
        ),
        (
            "tests/more_languages/group1/KotlinTest.kt",
            [
                "data class Person(val name: String)",
                "fun greet(person: Person)",
            ],
        ),
        (
            "tests/more_languages/group1/LispTest.lisp",
            [
                "defstruct person",
                "defun greet",
            ],
        ),
        (
            "tests/more_languages/group1/LuaTest.lua",
            [
                "function HelloWorld.new",
                "function HelloWorld.greet",
                "function say_hello",
            ],
        ),
        (
            "tests/more_languages/group1/ObjectiveCTest.m",
            [
                "@interface HelloWorld",
                "@interface HelloWorld -> (void) sayHello",
                "@implementation HelloWorld",
                "@implementation HelloWorld -> (void) sayHello",
                "void sayHelloWorld()",
            ],
        ),
        (
            "tests/more_languages/group1/OcamlTest.ml",
            [
                "type color",
                "class hello",
                "class hello -> method say_hello",
                "let main ()",
            ],
        ),
    ],
)
def test_more_languages_group1(file, expected):
    print(f"{file=}")
    result = parse_file(file)
    print(f"{result=}")
    print(f"{expected=}")
    assert result == expected


@pytest.mark.parametrize(
    "file,expected",
    [
        (
            "tests/more_languages/group2/apl_test.apl",
            [
                ":Namespace HelloWorld",
                ":Namespace HelloWorld -> hello ← 'Hello, World!'",
                ":Namespace HelloWorld -> plus ← {⍺+⍵}",
            ],
        ),
        (
            "tests/more_languages/group2/PerlTest.pl",
            [
                "package PerlTest",
                "package PerlTest -> sub new",
                "package PerlTest -> sub hello",
                "package PerlTest -> sub say_hello",
            ],
        ),
        (
            "tests/more_languages/group2/PhpTest.php",
            [
                "class HelloWorld",
                "class HelloWorld -> function sayHello",
                "function greet",
                "class Person",
                "class Person -> function __construct",
            ],
        ),
        (
            "tests/more_languages/group2/ScalaTest.scala",
            [
                "def sumOfSquares(x: Int, y: Int): Int",
                "trait Bark",
                "trait Bark -> def bark: String",
                "case class Person(name: String)",
                "object HelloWorld",
                "object HelloWorld -> def greet(person: Person): Unit",
                "object HelloWorld -> def main(args: Array[String]): Unit",
                "def sumOfSquaresShort(x: Int, y: Int): Int",
            ],
        ),
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
        (
            "tests/more_languages/group2/PowershellTest.ps1",
            [
                "function Test-Ordering([string]$foo)",
                "class Person",
                "class Person -> Person([string]$name)",
                "class Person -> [string]Greet()",
                "class Person -> [string]GreetMany([int]$times)",
                "class Person -> NoReturn([int]$times)",
                "class Person -> NoReturnNoArgs()",
                "function Say-Hello([Person]$person)",
            ],
        ),
    ],
)
def test_more_languages_group2(file, expected):
    print(f"{file=}")
    result = parse_file(file)
    print(f"{expected=}")
    print(f"{result=}")
    assert result == expected


@pytest.mark.parametrize(
    "file,expected",
    [
        (
            "tests/more_languages/group4/rust_test.rs",
            [
                "enum Days",
                "struct Point",
                "impl Point",
                "fn get_origin() -> Point",
                "struct Person",
                "impl Person",
                "fn greet(&self)",
                "fn add_two_longs(x1: i64, x2: i64) -> i64",
                """fn add_two_longs_longer(
    x1: i64,
    x2: i64,
) -> i64""",
                "fn multiply_by_two(num: f64) -> f64",
                "fn get_first_character(s: &str) -> Option<char>",
                "trait Drawable",
                "fn draw(&self)",
                "impl Drawable for Point",
                "fn draw(&self)",
                "fn main()",
                "pub struct VisibleStruct",
                "mod my_module",
                "macro_rules! say_hello",
                "pub mod lib",
                "pub mod interfaces",
                "mod engine",
                """pub fn flow<S1, S2, S3, S4, E, T, L>(
    source: S1, 
    extractor: E, 
    inbox: S2, 
    transformer: T, 
    outbox: S3, 
    loader: L, 
    sink: &mut S4,
) -> Result<(), Box<dyn Error>>
where
    S1: Extractable,
    S2: Extractable + Loadable,
    S3: Extractable + Loadable,
    S4: Loadable,
    E: Extractor<S1, S2>,
    T: Transformer<S2, S3>,
    L: Loader<S3, S4>,""",
            ],
        ),
        (
            "tests/more_languages/group4/tf_test.tf",
            [
                'provider "aws"',
                'resource "aws_instance" "example"',
                'data "aws_ami" "ubuntu"',
                'variable "instance_type"',
                'output "instance_public_ip"',
                "locals",
                'module "vpc"',
            ],
        ),
    ],
)
def test_more_languages_group4(file, expected):
    print(f"{file=}")
    result = parse_file(file)
    print(f"{result=}")
    print(f"{expected=}")
    assert result == expected


@pytest.mark.parametrize(
    "file,expected",
    [
        (
            "tests/more_languages/group5/ansible_test.yml",
            [
                "Install package",
                "Start service",
                "Create user",
            ],
        ),
        (
            "tests/more_languages/group5/k8s_test.yaml",
            [
                "apps/v1.Deployment -> my-app",
                "v1.Service -> my-service",
                "v1.ConfigMap -> my-config",
            ],
        ),
        (
            "tests/more_languages/group5/checkbox_test.md",
            [
                "# My Checkbox Test",
                "## My No Parens Test",
                "## My Empty href Test",
                "## My other url Test [Q&A]",
                "## My other other url Test [Q&A]",
                "## My 2nd other url Test [Q&A]",
                "## My 3rd other url Test [Q&A]",
                "- [ ] Task 1",
                "    - [ ] No Space Task 1.1",
                "    - [ ] Two Spaces Task 1.2",
                "        - [ ] Subtask 1.2.1",
                "- [ ] Task 2",
                "- [x] Task 3",
                "    - [ ] Subtask 3.1",
                "- [x] Task 6",
                "    - [x] Subtask 6.1",
                "        - [ ] Handle edge cases",
            ],
        ),
        (
            "tests/more_languages/group5/checkbox_test.txt",
            [
                "- [ ] fix phone number format +1",
                "- [ ] add forgot password",
                "- [ ] ? add email verification",
                "- [ ] store token the right way",
                "- [ ] test nesting of checkboxes",
                "- [ ] user can use option to buy ticket at 2-referred price",
                "- [ ] CTA refer 2 people to get instant lower price",
                "- [ ] form to send referrals",
            ],
        ),
        (
            ".github/workflows/unix.yml",
            [
                "Ubuntu & MacOS",
                "  job: build",
                "    - Set up Python ${{ matrix.python-version }}",
                "    - Install tree_plus",
                "    - Run generic tests",
                "    - Run specific test",
            ],
        ),
        (
            "tests/more_languages/group5/sql_test.sql",
            [
                "CREATE TABLE promoters",
                "   user_id serial PRIMARY KEY,",
                "   type varchar(20) NOT NULL,",
                "   username varchar(20) NOT NULL,",
                "   password varchar(20) NOT NULL,",
                "   email varchar(30) NOT NULL,",
                "   phone varchar(20) NOT NULL,",
                "   promocode varchar(20),",
                "   info json,",
                "   going text[],",
                "   invites text[],",
                "   balance integer NOT NULL,",
                "   rewards text[],",
                "   created timestamp",
                "CREATE TABLE events",
                "   event_id serial PRIMARY KEY,",
                "   name varchar(64) NOT NULL,",
                "   date varchar(64) NOT NULL,",
                "   location varchar(64) NOT NULL,",
                "   performer varchar(64) NOT NULL,",
                "   rewards json,",
                "   created timestamp",
            ],
        ),
        (
            "tests/more_languages/group5/app.component.spec.ts",
            [
                "describe 'AppComponent'",
                "    it should create the app",
                "    it should welcome the user",
                "    it should welcome 'Jimbo'",
                "    it should request login if not logged in",
            ],
        ),
        (
            "tests/more_languages/group5/app.component.ts",
            [
                "class AppComponent",
                "    title: string = 'promo-app'",
                "    user: any",
                "    events: any",
                "    events_list: any",
                "    clientSecret: string",
                "    card?: StripePaymentElementComponent",
                "    cardOptions: StripePaymentElementOptions",
                """    constructor(
        private http: HttpClient,
        private loginService: LoginService,
        private stripeService: StripeService
    )""",
                "    constructor(private loginService: LoginService)",
                "    checkSession()",
                "    async goToEvent(event_id: string)",
                "    valInvitedBy(event: any, event_id: string)",
            ],
        ),
        (
            "tests/more_languages/group5/app-routing.module.ts",
            [
                """const routes: Routes = [
    { path: '', redirectTo: 'login', pathMatch: 'full' },
    { path: '*', redirectTo: 'login' },
    { path: 'home', component: HomeComponent },
    { path: 'login', component: LoginComponent },
    { path: 'register', component: RegisterComponent },
    { path: 'events', component: EventsComponent },
    { path: 'invites', component: InvitesComponent },
    { path: 'rewards', component: RewardsComponent },
    { path: 'profile', component: ProfileComponent },
];""",
                "class AppRoutingModule",
            ],
        ),
        (
            "tests/more_languages/group5/app.module.ts",
            [
                """@NgModule({
    declarations: [
        AppComponent,
        HomeComponent,
        LoginComponent,
        RegisterComponent,
        EventsComponent,
        InvitesComponent,
        RewardsComponent,
        ProfileComponent""",
                "class AppModule",
            ],
        ),
        (
            "tests/more_languages/group5/requirements_test.txt",
            [
                "psycopg2-binary",
                "pytest",
                "coverage",
                "flask[async]",
                "flask_cors",
                "stripe",
                "pyjwt[crypto]",
                "cognitojwt[async]",
                "flask-lambda",
            ],
        ),
        (
            "tests/more_languages/group5/testJsonSchema.json",
            [
                "$schema: http://json-schema.org/draft-07/schema#",
                "type: object",
                "title: random_test",
                "description: A promoter's activites related to events",
            ],
        ),
        (
            "tests/more_languages/group5/Makefile",
            [
                "include dotenv/dev.env",
                ".PHONY: dev",
                "dev",
                "services-down",
                "services-stop: services-down",
                "define CHECK_POSTGRES",
                "damage-report",
                "tail-logs",
                "cloud",
            ],
        ),
        (
            "tests/more_languages/group5/test.env",
            [
                "PROMO_PATH",
                "PRODUCTION",
                "SQL_SCHEMA_PATH",
                "DB_LOGS",
                "DB_LOG",
                "PGPASSWORD",
                "PGDATABASE",
                "PGHOST",
                "PGPORT",
                "PGUSER",
                "SERVER_LOG",
                "SERVER_LOGS",
                "API_URL",
                "APP_LOGS",
                "APP_LOG",
                "APP_URL",
                "COGNITO_USER_POOL_ID",
                "COGNITO_APP_CLIENT_ID",
                "AWS_REGION",
                "STRIPE_SECRET_KEY",
            ],
        ),
        (
            "tests/more_languages/group5/testPackage.json",
            [
                "name: 'promo-app'",
                "version: 0.0.0",
                "scripts:",
                "    ng: 'ng'",
                "    start: 'ng serve'",
                "    build: 'ng build'",
                "    watch: 'ng build --watch --configuration development'",
                "    test: 'ng test'",
            ],
        ),
        (
            "tests/more_languages/group5/environment.test.ts",
            [
                "environment:",
                "   production",
                "   cognitoUserPoolId",
                "   cognitoAppClientId",
                "   apiurl",
            ],
        ),
    ],
)
def test_more_languages_group5(file, expected):
    print(f"{file=}")
    result = parse_file(file)
    print(f"{result=}")
    print(f"{expected=}")
    assert result == expected


# TODO:
# @pytest.mark.parametrize(
#     "file,expected",
#     [
#         (
#             "tests/more_languages/group3/crystal_test.cr",
#             ["class Person -> def greet", "def say_hello"],
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
#     ],
# )
# def test_more_languages_group3(file, expected):
#     result = parse_file(file)
#     print(f"{result=}")
#     print(f"{expected=}")
#     assert result == expected

# (
#     "tests/more_languages/group3/clojure_test.clj",
#     ["defprotocol P", "defrecord Person", "defn -main"],
# ),
# (
#     "tests/more_languages/group3/cpp_test.cpp",
#     ["class Person", "void Person::greet", "void globalGreet", "int main"],
# ),
# (
# (
#     "tests/more_languages/group3/fortran_test.f90",
#     [
#         "MODULE hello_mod -> TYPE person",
#         "MODULE hello_mod -> SUBROUTINE say_hello",
#         "PROGRAM HelloWorld",
#     ],
# ),
#         (
#     "tests/more_languages/group2/RTest.R",
#     [
#         "person <- list(name = 'John Doe', age = 50)",
#         "greet.Person <- function(p)",
#     ],
# ),
# (
#     "tests/more_languages/group2/bash_test.sh",
#     [
#         "echo_hello_world()",
#     ],
# ),
# (
#     "tests/more_languages/group4/fsharp_test.fs",
#     ["type Person -> member this.SayHello", "let person"],
# ),
# (
#     "tests/more_languages/group4/go_test.go",
#     ["type Greeting -> func (g Greeting) sayHello", "func createGreeting"],
# ),
# (
#     "tests/more_languages/group4/haskell_test.hs",
#     ["data Person", "greet :: Person -> String"],
# ),
# (
#     "tests/more_languages/group4/mathematica_test.nb",
#     [
#         "person[name_]",
#         'BeginPackage["TestModule"] -> sayHello::usage',
#         'BeginPackage["TestModule"] -> sumList::usage',
#     ],
# ),
# (
#     "tests/more_languages/group4/matlab_test.m",
#     ["classdef HelloWorld -> function greet", "function loneFun"],
# ),
# (
#     "tests/more_languages/group4/ruby_test.rb",
#     [
#         "module Greeter -> def self.say_hello",
#         "class HelloWorld -> def say_hello",
#     ],
# ),
# ( # unclear how to / what to include here, might be good to skip
#     "tests/more_languages/group5/nodemon.json",
#     [
#         "exec: make dev || exit 1",
#     ],
# ),
# ( # unclear how to / what to include here, might be good to skip
#     "tests/more_languages/group5/testTypings.d.ts",
#     [
#         "var stripe: any",
#         "var elements: any",
#         "function getArrayLength(arr: any[]): number",
#         "const maxInterval: 12",
#         "const helloWorld: RegExp",
#         "const pi: number",
#     ],
# ),
# (
#     "tests/more_languages/group4/sas_test.sas",
#     ["data work.testData", "%macro sayHello", "PROC SQL"],
# ),
# (
#     "tests/more_languages/group4/swift_test.swift",
#     ["class Person -> func greet", "struct CustomType"],
# ),
# (
#     "tests/more_languages/group4/vba_test.bas",
#     [
#         "Class CPerson -> Public Property Get Name",
#         "Class CPerson -> Public Property Let Name",
#         "Class CPerson -> Public Sub Greet",
#     ],
# ),


# FUCK csharp
# (
#     "tests/more_languages/group3/csharp_test.cs",
#     [
#         "public interface IExcelTemplate",
#         "public interface IExcelTemplate -> void LoadTemplate(string templateFilePath)",
#         "public interface IExcelTemplate -> void LoadData(Dictionary<string, string> data)",
#         "public interface IExcelTemplate -> void ModifyCell(string cellName, string value)",
#         "public interface IExcelTemplate -> void SaveToFile(string filePath)",
#         "public interface IGreet",
#         "public interface IGreet -> void Greet()",
#         "public enum WeekDays",
#         "public delegate void DisplayMessage(string message)",
#         "public struct Address",
#         "public static class HelperFunctions",
#         "public static class HelperFunctions -> public static void PrintMessage(string message)",
#         "public static class HelperFunctions -> public static int AddNumbers(int a, int b)",
#         "public static class HelperFunctions -> private static float CalculateAverage(float[] numbers)",
#         "namespace HelloWorldApp",
#         "namespace HelloWorldApp -> class Person : IGreet",
#         "namespace HelloWorldApp -> class Person -> public Person(string name, int age)",
#         "namespace HelloWorldApp -> class Person -> public void Greet()",
#         "namespace HelloWorldApp -> class Person -> private int GetAge()",
#         "namespace HelloWorldApp -> class HelloWorld",
#         "namespace HelloWorldApp -> class HelloWorld -> private static DisplayMessage displayDelegate",
#         "namespace HelloWorldApp -> class HelloWorld -> static void Main(string[] args)",
#         "namespace TemplateToExcelServer.Template",
#         "namespace TemplateToExcelServer.Template -> public interface ITemplateObject",
#         "namespace TemplateToExcelServer.Template -> public interface ITemplateObject -> string[,] GetContent()",
#         "namespace TemplateToExcelServer.Template -> public interface ITemplateObject -> string[] GetContentArray()",
#         "namespace TemplateToExcelServer.Template -> public interface ITemplateObject -> string[] GetFormat()",
#         "namespace TemplateToExcelServer.Template -> public interface ITemplateObject -> int? GetFormatLength()",
#         "namespace TemplateToExcelServer.Template -> public interface ITemplateObject -> TemplateObject SetContent(string[,] Content)",
#         "namespace TemplateToExcelServer.Template -> public interface ITemplateObject -> TemplateObject SetContentArray(string[] value)",
#         "namespace TemplateToExcelServer.Template -> public interface ITemplateObject -> TemplateObject SetFormaat(string[] Header)",
#         "namespace TemplateToExcelServer.Template -> public interface ITemplateObject -> TemplateObject SetNameOfReport(ReadOnlyMemory<byte> ReportName)",
#         "namespace TemplateToExcelServer.Template -> public interface ITemplateObject -> TemplateObject SetSheetName(ReadOnlyMemory<byte> SheetName)",
#     ],
# ),
