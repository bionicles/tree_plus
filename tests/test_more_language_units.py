# tests/test_more_language_units.py
from typing import List
import pytest
import os

from rich.text import Text
from rich import print
import sqlite3

from tree_plus_src import parse_file

SQLITE_PATH = "tests/more_languages/group3/test.sqlite"

# Define the SQL commands for table creation
create_students_table = """
CREATE TABLE IF NOT EXISTS students (
    id INTEGER PRIMARY KEY,
    name TEXT NOT NULL,
    age INTEGER NOT NULL
);
"""

create_courses_table = """
CREATE TABLE IF NOT EXISTS courses (
    id INTEGER PRIMARY KEY,
    title TEXT NOT NULL,
    credits INTEGER NOT NULL
);
"""

SQLITE_TEST_QUERIES = (create_students_table, create_courses_table)


def create_sqlite_test_db(
    db_path: str = SQLITE_PATH,
    test_queries: tuple = SQLITE_TEST_QUERIES,
    force: bool = False,
):
    if os.path.exists(db_path) and force:
        os.remove(db_path)
    conn = sqlite3.connect(db_path)
    cursor = conn.cursor()
    for query in test_queries:
        cursor.execute(query)
    conn.commit()
    conn.close()


@pytest.mark.parametrize(
    "file,expected",
    [
        (
            "tests/more_languages/group1/test.js",
            [
                "class MyClass",
                "  myMethod()",
                "  async asyncMethod(a, b)",
                "  methodWithDefaultParameters(a = 5, b = 10)",
                """  multilineMethod(
    c,
    d
  )""",
                """  multilineMethodWithDefaults(
    t = "tree",
    p = "plus"
  )""",
                "function myFunction(param1, param2)",
                """function multilineFunction(
  param1,
  param2
)""",
                "const arrowFunction = () =>",
                "const parametricArrow = (a, b) =>",
                "function ()",
                "function outerFunction(outerParam)",
                "  function innerFunction(innerParam)",
                "const myObject = {",
                "  myMethod: function (stuff)",
                "let myArrowObject = {",
                """  myArrow: ({
    a,
    b,
    c,
  }) =>""",
                "const myAsyncArrowFunction = async () =>",
                "function functionWithRestParameters(...args)",
                "const namedFunctionExpression = function myNamedFunction()",
                """const multilineArrowFunction = (
  a,
  b
) =>""",
                "function functionReturningFunction()",
                "  return function ()",
                """function destructuringOnMultipleLines({
  a,
  b,
})""",
                "const arrowFunctionWithDestructuring = ({ a, b }) =>",
                """const multilineDestructuringArrow = ({
  a,
  b,
}) =>""",
                "async function asyncFunctionWithErrorHandling()",
                "class Car",
                "  constructor(brand)",
                "  present()",
                "class Model extends Car",
                "  constructor(brand, mod)",
                "  show()",
            ],
        ),
        (
            "tests/more_languages/group1/test.ts",
            [
                "type MyType",
                "interface MyInterface",
                "class TsClass",
                "  myMethod()",
                "  myMethodWithArgs(param1: string, param2: number): void",
                "  static myStaticMethod<T>(param: T): T",
                """  multilineMethod(
    c: number,
    d: number
  ): number""",
                """  multilineMethodWithDefaults(
    t: string = "tree",
    p: string = "plus"
  ): string""",
                "export class AdvancedComponent implements MyInterface",
                """  async myAsyncMethod(
    a: string,
    b: number,
    c: string
  ): Promise<void>""",
                """  genericMethod<T, U>(
    arg1: T,
    arg2: U
  ): [T, U]""",
                "export class TicketsComponent implements MyInterface",
                "  async myAsyncMethod({ a, b, c }: { a: String; b: Number; c: String })",
                "function tsFunction()",
                """function tsFunctionSigned(
  param1: number,
  param2: number
): void""",
                """const tsArrowFunctionSigned = ({
  a,
  b,
}: {
  a: number;
  b: string;
}) =>""",
                "const arrowFunction = () =>",
                "const arrow = (a: String, b: Number) =>",
                "const asyncArrowFunction = async () =>",
                "const asyncArrow = async (a: String, b: Number) =>",
                "let weirdArrow = () =>",
                "const asyncPromiseArrow = async (): Promise<void> =>",
                "let myWeirdArrowSigned = (x: number): number =>",
                "class Person",
                "  constructor(private firstName: string, private lastName: string)",
                "  getFullName(): string",
                "  describe(): string",
                "class Employee extends Person",
                """  constructor(
    firstName: string,
    lastName: string,
    private jobTitle: string
  )""",
                "  describe(): string",
                "interface Shape",
                "interface Square extends Shape",
            ],
        ),
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
                "abstract class LivingBeing",
                "    abstract void breathe()",
                "interface Communicator",
                "    String communicate()",
                "@Log",
                "@Getter",
                "@Setter",
                "class Person extends LivingBeing implements Communicator",
                "    Person(String name, int age)",
                "    @Override",
                "    void breathe()",
                "    @Override",
                "    public String communicate()",
                "    void greet()",
                "    String personalizedGreeting(String greeting, Optional<Boolean> includeAge)",
                "@Singleton",
                "@RestController",
                "@SpringBootApplication",
                "public class Example",
                "    @Inject",
                "    public Example(Person person)",
                '    @RequestMapping("/greet")',
                """    String home(@RequestParam(value = "name", defaultValue = "World") String name,
                @RequestParam(value = "age", defaultValue = "30") int age)""",
                "    public static void main(String[] args)",
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
                "fun <T> processItems(items: List<T>, processor: (T) -> Unit)",
                "interface Source<out T>",
                "    fun nextT(): T",
                "fun MutableList<Int>.swap(index1: Int, index2: Int)",
                "fun Any?.toString(): String",
                "tailrec fun findFixPoint(x: Double = 1.0): Double",
                "class GenericRepository<T>",
                "    fun getItem(id: Int): T?",
                "sealed interface Error",
                "sealed class IOError(): Error",
                "object Runner",
                "    inline fun <reified S: SomeClass<T>, T> run() : T",
                "infix fun Int.shl(x: Int): Int",
                "class MyStringCollection",
                "    infix fun add(s: String)",
                "    fun build()",
                "open class Base(p: Int)",
                "class Derived(p: Int) : Base(p)",
                "open class Shape",
                "    open fun draw()",
                "    fun fill()",
                "    open fun edge(case: Int)",
                "interface Thingy",
                "    fun edge()",
                "class Circle() : Shape(), Thingy",
                "    override fun draw()",
                "    final override fun edge(case: Int)",
                "interface Base",
                "    fun print()",
                "class BaseImpl(val x: Int) : Base",
                "    override fun print()",
                "internal class Derived(b: Base) : Base by b",
                "class Person constructor(firstName: String)",
                """class People(
    firstNames: Array<String>,
    ages: Array<Int>(42),
)""",
                "    fun edgeCases(): Boolean",
                """class Alien public @Inject constructor(
    val firstName: String,
    val lastName: String,
    var age: Int,
    val pets: MutableList<Pet> = mutableListOf(),
)""",
                "    fun objectOriented(): String",
                " enum class IntArithmetics : BinaryOperator<Int>, IntBinaryOperator",
                """    PLUS {
        override fun apply(t: Int, u: Int): Int""",
                """    TIMES {
        override fun apply(t: Int, u: Int): Int""",
                "    override fun applyAsInt(t: Int, u: Int)",
                """fun reformat(
    str: String,
    normalizeCase: Boolean = true,
    upperCaseFirstLetter: Boolean = true,
    divideByCamelHumps: Boolean = false,
    wordSeparator: Char = ' ',
)""",
                "operator fun Point.unaryMinus()",
                "abstract class Polygon",
                "    abstract fun draw()",
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
def test_more_languages_group1(
    file: str,
    expected: List[str],
):
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
                "  def bark: String",
                "case class Person(name: String)",
                """class GenericClass[T](
    val data: T,
    val count: Int
)""",
                "  def getData: T",
                "object HelloWorld",
                "  def greet(person: Person): Unit",
                "  def main(args: Array[String]): Unit",
                """def complexFunction(
    a: Int,
    b: String,
    c: Float
): (Int, String) Option""",
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
                """long complexFunctionWithMultipleArguments(
    int param1,
    double param2,
    char *param3,
    struct Point point
)""",
                "keyPattern *ACLKeyPatternCreate(sds pattern, int flags)",
                "sds sdsCatPatternString(sds base, keyPattern *pat)",
                "static int ACLCheckChannelAgainstList(list *reference, const char *channel, int channellen, int is_pattern)",
                "static struct config",
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
        (
            "tests/more_languages/group2/test.csv",
            ["Name", "Age", "Country", "City", "Email"],
        ),
        (
            "tests/more_languages/group2/go_test.go",
            [
                "type Greeting struct",
                "func (g Greeting) sayHello()",
                "func createGreeting(m string) Greeting",
                "type SomethingLong struct",
                """func (s *SomethingLong) WithAReasonableName(
	ctx context.Context,
	param1 string,
	param2 int,
	param3 map[string]interface{},
	callback func(int) error,
) (resultType, error)""",
                "type resultType struct",
                "func main()",
            ],
        ),
    ],
)
def test_more_languages_group2(
    file: str,
    expected: List[str],
):
    print(f"{file=}")
    result = parse_file(file)
    print(f"{expected=}")
    print(f"{result=}")
    assert result == expected


@pytest.mark.parametrize(
    "file,expected",
    [
        (
            "tests/more_languages/group3/cpp_test.cpp",
            [
                "class Person",
                "void globalGreet()",
                "int main()",
                "void printMessage(const std :: string &message)",
                """template<typename T>
void printVector(const std :: vector<T>& vec)""",
                "struct Point",
                "class Animal",
                "class Dog : public Animal",
                "class Cat : public Animal",
                """nb :: bytes BuildRnnDescriptor(int input_size, int hidden_size, int num_layers,
                             int batch_size, int max_seq_length, float dropout,
                             bool bidirectional, bool cudnn_allow_tf32,
			     int workspace_size, int reserve_space_size)""",
                "int main()",
                "enum ECarTypes",
                "ECarTypes GetPreferredCarType()",
                "enum ECarTypes : uint8_t",
                "enum class ECarTypes : uint8_t",
                "void myFunction(string fname, int age)",
                "template <typename T> T cos(T)",
                "template <typename T> T sin(T)",
                "template <typename T> T sqrt(T)",
                "template<typename T> struct VLEN",
                "template<typename T> class arr",
                "class Buffer",
                """std :: tuple<array, array, array> quantize(
    const array& w,
    int group_size,
    int bits,
    StreamOrDevice s)""",
            ],
        ),
        (
            "tests/more_languages/group3/swift_test.swift",
            [
                "class Person",
                "    init(name: String)",
                "    func greet()",
                """    func yEdgeCase(
        fname: String, 
        lname: String, 
        age: Int, 
        address: String, 
        phoneNumber: String
    )""",
                "func globalGreet()",
                "struct Point",
                "protocol Animal",
                "    func speak()",
                "struct Dog: Animal",
                "class Cat: Animal",
                "    init(name: String)",
                "    func speak()",
                "enum CarType",
                "func getPreferredCarType() -> CarType",
                "enum CarType: UInt8",
                "enum class CarType: UInt8",
                "func myFunction(fname: String, age: Int)",
                """func myFunctionWithMultipleParameters(
    fname: String, 
    lname: String, 
    age: Int, 
    address: String, 
    phoneNumber: String
)""",
            ],
        ),
        (
            "tests/more_languages/group3/bash_test.sh",
            [
                "echo_hello_world()",
                "function fun_echo_hello_world()",
                "export SECRET",
                "alias md='make debug'",
                "add_alias()",
                "create_conda_env()",
            ],
        ),
        (
            "tests/more_languages/group3/ruby_test.rb",
            [
                "module Greeter",
                "  def self.say_hello",
                "class HelloWorld",
                "  def say_hello",
                "class Human",
                "  def self.bar",
                "  def self.bar=(value)",
                "class Doctor < Human",
                """  def brachial_plexus(
      roots,
      trunks,
      divisions: true,
      cords: [],
      branches: Time.now
    )""",
            ],
        ),
        (
            "tests/more_languages/group3/hallucination.tex",
            [
                "Harnessing the Master Algorithm: Strategies for AI LLMs to Mitigate Hallucinations",
                "Hallucinated Pedro Domingos et al.",
                "Christmas Eve 2023",
                "1 Introduction",
                "2 Representation in LLMs",
                "  2.1 Current Representational Models",
                "  2.2 Incorporating Cognitive Structures",
                "  2.3 Conceptual Diagrams of Advanced Representational Models",
                "3 Evaluation Strategies",
                "  3.1 Existing Evaluation Metrics for LLMs",
                "  3.2 Integrating Contextual and Ethical Considerations",
                "  3.3 Case Studies: Evaluation in Practice",
                "4 Optimization Techniques",
                "  4.1 Continuous Learning Models",
                "  4.2 Adaptive Algorithms for Real-time Adjustments",
                "  4.3 Performance Metrics Pre- and Post-Optimization",
                "5 Interdisciplinary Insights",
                "  5.1 Cognitive Science and AI: A Symbiotic Relationship",
                "  5.2 Learning from Human Cognitive Processes",
                "6 Challenges and Future Directions",
                "  6.1 Addressing Current Limitations",
                "  6.2 The Road Ahead: Ethical and Practical Considerations",
                "7 Conclusion",
                "  7.1 Summarizing Key Findings",
                "  7.2 The Next Steps in AI Development",
            ],
        ),
        (
            "tests/more_languages/group3/test.lean",
            [
                "# Advanced Topics in Group Theory",
                "section GroupDynamics",
                "lemma group_stability (G : Type*) [Group G] (H : Subgroup G)",  # everything to the first `:` not inside parens, maybe?
                "theorem subgroup_closure {G : Type*} [Group G] (S : Set G)",
                "axiom group_homomorphism_preservation {G H : Type*} [Group G] [Group H] (f : G → H)",
                "end GroupDynamics",
                "section ConstructiveApproach",
                "lemma finite_group_order (G : Type*) [Group G] [Fintype G]",
                """lemma complex_lemma {X Y : Type*} [SomeClass X] [AnotherClass Y]
  (f : X → Y) (g : Y → X)""",
                "end ConstructiveApproach",
            ],
        ),
        (
            "tests/more_languages/group3/csharp_test.cs",
            [
                "public interface IExcelTemplate",
                "    void LoadTemplate(string templateFilePath)",
                "    void LoadData(Dictionary<string, string> data)",
                "    void ModifyCell(string cellName, string value)",
                "    void SaveToFile(string filePath)",
                "public interface IGreet",
                "    void Greet()",
                "public enum WeekDays",
                "public delegate void DisplayMessage(string message)",
                "public struct Address",
                "public static class HelperFunctions",
                "    public static void PrintMessage(string message)",
                "    public static int AddNumbers(int a, int b)",
                # "    private static float CalculateAverage(float[] numbers)", # private?
                "namespace HelloWorldApp",
                "    class Person : IGreet",
                "        public Person(string name, int age)",
                "        public void Greet()",
                "    class HelloWorld",
                "        static void Main(string[] args)",
                "namespace TemplateToExcelServer.Template",
                "    public interface ITemplateObject",
                "        string[,] GetContent()",
                "        string[] GetContentArray()",
                "        string[] GetFormat()",
                "        int? GetFormatLength()",
                "        TemplateObject SetContent(string[,] Content)",
                "        TemplateObject SetContentArray(string[] value)",
                "        TemplateObject SetFormat(string[] Header)",
                """        TemplateObject SetNameOfReport(
            ReadOnlyMemory<byte> ReportName,
            int[] EdgeCase)""",
                "        TemplateObject SetSheetName(ReadOnlyMemory<byte> SheetName)",
                "public class BankAccount(string accountID, string owner)",
                "    public override string ToString() =>",
                "var IncrementBy = (int source, int increment = 1) =>",
                "Func<int, int, int> add = (x, y) =>",
                "button.Click += (sender, args) =>",
                "public Func<int, int> GetMultiplier(int factor)",
                """public void Method(
        int param1,
        int param2,
        int param3,
        int param4,
        int param5,
        int param6,
    )""",
                """System.Net.ServicePointManager.ServerCertificateValidationCallback +=
    (se, cert, chain, sslerror) =>""",
                "class ServerCertificateValidation",
                """    public bool OnRemoteCertificateValidation(
        object se,
        X509Certificate cert,
        X509Chain chain,
        SslPolicyErrors sslerror
    )""",
                "s_downloadButton.Clicked += async (o, e) =>",
                '[HttpGet, Route("DotNetCount")]',
                "static public async Task<int> GetDotNetCount(string URL)",
            ],
        ),
        (
            "tests/more_languages/group3/test.sqlite",
            [
                "students table:",
                "   id integer primary key",
                "   name text not null",
                "   age integer not null",
                "courses table:",
                "   id integer primary key",
                "   title text not null",
                "   credits integer not null",
            ],
        ),
        (
            "tests/more_languages/group3/test_pyproject.toml",
            [
                "name: tree_plus",
                "version: 1.0.8",
                "description: A `tree` util enhanced with tokens, lines, and components.",
                "License :: OSI Approved :: Apache Software License",
                "License :: OSI Approved :: MIT License",
                "dependencies:",
                "    tiktoken",
                "    PyYAML",
                "    click",
                "    rich",
                "    tomli",
            ],
        ),
        (
            "tests/more_languages/group3/test_Cargo.toml",
            [
                "name: test_cargo",
                "version: 0.1.0",
                "description: A test Cargo.toml",
                "license: MIT OR Apache-2.0",
                "dependencies:",
                "  clap 4.4",
                "  sqlx 0.7 (features: runtime-tokio, tls-rustls)",
            ],
        ),
        (
            "tests/more_languages/group3/test_openapi.yaml",
            [
                "openapi: 3.0.1",
                "    title: TODO Plugin",
                "    description: A plugin to create and manage TODO lists using ChatGPT.",
                "    version: v1",
                "servers:",
                "    - url: PLUGIN_HOSTNAME",
                "paths:",
                "    '/todos/{username}':",
                "        GET (getTodos): Get the list of todos",
                "        POST (addTodo): Add a todo to the list",
                "        DELETE (deleteTodo): Delete a todo from the list",
            ],
        ),
        (
            "tests/more_languages/group3/test.graphql",
            [
                "type Query",
                "    getBooks: [Book]",
                "    getAuthors: [Author]",
                "type Mutation",
                "    addBook(title: String, author: String): Book",
                "    removeBook(id: ID): Book",
                "type Book",
                "    id: ID",
                "    title: String",
                "    author: Author",
                "type Author",
                "    id: ID",
                "    name: String",
                "    books: [Book]",
            ],
        ),
        (
            "tests/more_languages/group3/test_openrpc.json",
            [
                "openrpc: 1.2.1",
                "info:",
                "    title: Demo Petstore",
                "    version: 1.0.0",
                "methods:",
                "    listPets: List all pets",
                "        params:",
                "            - limit: integer",
                "        result: pets = An array of pets",
            ],
        ),
        (
            "tests/more_languages/group3/test_json_rpc_2_0.json",
            [
                "jsonrpc: 2.0",
                "method: subtract",
                "params:",
                "    minuend: 42",
                "    subtrahend: 23",
                "id: 1",
            ],
        ),
        (
            "tests/more_languages/group3/test.capnp",
            [
                "struct Employee",
                "  id @0 :Int32",
                "  name @1 :Text",
                "  role @2 :Text",
                "  skills @3 :List(Skill)",
                "  struct Skill",
                "    name @0 :Text",
                "    level @1 :Level",
                "    enum Level",
                "      beginner @0",
                "      intermediate @1",
                "      expert @2",
                "  status :union",
                "    active @4 :Void",
                "    onLeave @5 :Void",
                "    retired @6 :Void",
                "struct Company",
                "  employees @0 :List(Employee)",
            ],
        ),
        (
            "tests/more_languages/group3/test.proto",
            [
                'syntax = "proto3"',
                "service EmployeeService",
                "    rpc GetEmployee(EmployeeId) returns (EmployeeInfo)",
                "    rpc AddEmployee(EmployeeData) returns (EmployeeInfo)",
                "    rpc UpdateEmployee(EmployeeUpdate) returns (EmployeeInfo)",
                "message EmployeeId",
                "    int32 id = 1",
                "message EmployeeInfo",
                "    int32 id = 1",
                "    string name = 2",
                "    string role = 3",
                "message EmployeeData",
                "    string name = 1",
                "    string role = 2",
                "message EmployeeUpdate",
                "    int32 id = 1",
                "    string name = 2",
                "    string role = 3",
            ],
        ),
    ],
)
def test_more_languages_group3(file: str, expected: List[str]):
    print(f"{file=}")
    if file.endswith("test.sqlite"):
        create_sqlite_test_db()
    os.environ.get("DEBUG_TREE_PLUS") == "1"
    result = parse_file(file)
    print(f"{result=}")
    print(f"{expected=}")
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
                "    fn get_origin() -> Point",
                "struct Person",
                "impl Person",
                "    fn greet(&self)",
                "fn add_two_longs(x1: i64, x2: i64) -> i64",
                """fn add_two_longs_longer(
    x1: i64,
    x2: i64,
) -> i64""",
                "fn multiply_by_two(num: f64) -> f64",
                "fn get_first_character(s: &str) -> Option<char>",
                "trait Drawable",
                "    fn draw(&self)",
                "impl Drawable for Point",
                "    fn draw(&self)",
                "fn main()",
                "pub struct VisibleStruct",
                "mod my_module",
                "    pub struct AlsoVisibleStruct<T>(T, T)",
                "macro_rules! say_hello",
                """#[macro_export]
macro_rules! hello_tree_plus""",
                "pub mod lib",
                "    pub mod interfaces",
                "    mod engine",
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
                "trait Container",
                "    fn items(&self) -> impl Iterator<Item = Widget>",
                "trait HttpService",
                "    async fn fetch(&self, url: Url) -> HtmlBody",
                "struct Pair<T, U>",
                "trait Transformer<T>",
                "    fn transform(&self, input: T) -> T",
                "impl<T: std::ops::Add<Output = T> + Copy> Transformer<T> for Pair<T, T>",
                "    fn transform(&self, input: T) -> T",
                "fn main()",
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
        (
            "tests/more_languages/group4/haskell_test.hs",
            [
                "data Person",
                "greet :: Person -> String",
                """resolveVariables ::
  forall m fragments.
  (MonadError QErr m, Traversable fragments) =>
  Options.BackwardsCompatibleNullInNonNullableVariables ->
  [G.VariableDefinition] ->
  GH.VariableValues ->
  [G.Directive G.Name] ->
  G.SelectionSet fragments G.Name ->
  m
    ( [G.Directive Variable],
      G.SelectionSet fragments Variable
    )""",
            ],
        ),
        (
            "tests/more_languages/group4/mathematica_test.nb",
            [
                "person[name_]",
                "sayHello[]",
                "sumList[list_List]",
            ],
        ),
        (
            "tests/more_languages/group4/test.zig",
            [
                "pub fn add(a: i32, b: i32) i32",
                'test "add function"',
                "const BunBuildOptions = struct",
                "    pub fn updateRuntime(this: *BunBuildOptions) anyerror!void",
                "    pub fn step(this: BunBuildOptions, b: anytype) *std.build.OptionsStep",
                """pub fn sgemv(
    order: Order,
    trans: Trans,
    m: usize,
    n: usize,
    alpha: f32,
    a: []const f32,
    lda: usize,
    x: []const f32,
    x_add: usize,
    beta: f32,
    y: []f32,
    y_add: usize,
) void""",
            ],
        ),
        (
            "tests/more_languages/group4/RTest.R",
            [
                "class(person)",
                "greet.Person <- function",
                "ensure_between = function",
                "run_intermediate_annealing_process = function",
            ],
        ),
        (
            "tests/more_languages/group4/test_tcl_tk.tcl",
            [
                "proc sayHello {}",
                "proc arrg { input }",
                """proc multiLine {
    x,
    y
}""",
            ],
        ),
        (
            "tests/more_languages/group4/erl_test.erl",
            [
                "-module(erl_test).",
                "-record(person).",
                "-type ra_peer_status().",
                "-type ra_membership().",
                "-opaque my_opaq_type().",
                "-type orddict(Key, Val).",
                """-type edge(
        Cases,
        Pwn,
    ).""",
                "-spec guarded(X) -> X when X :: tuple().",
                """-spec edge_case(
        {integer(), any()} | [any()]
    ) -> processed, integer(), any()} | [{item, any()}].""",
                """-spec complex_function({integer(), any()} | [any()]) -> 
    {processed, integer(), any()} | [{item, any()}].""",
                "-spec list_manipulation([integer()]) -> [integer()].",
                """-spec overload(T1, T2) -> T3
        ; (T4, T5) -> T6.""",
                """-spec multiguard({X, integer()}) -> X when X :: atom()
        ; ([Y]) -> Y when Y :: number().""",
                "-record(multiline).",
                "-record(maybe_undefined).",
            ],
        ),
        (
            "tests/more_languages/group4/test_fsharp.fs",
            [
                "module TestFSharp",
                "type Person = {",
                "let add x y =",
                """let multiply 
    (x: int) 
    (y: int): int =""",
                """let complexFunction
    (a: int)
    (b: string)
    (c: float)
    : (int * string) option =""",
                "type Result<'T> =",
            ],
        ),
    ],
)
def test_more_languages_group4(
    file: str,
    expected: List[str],
):
    print(f"{file=}")
    result = parse_file(file)
    if file.endswith("rust_test.rs"):
        expected = [Text(expectation) for expectation in expected]
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
                "Linux & MacOS",
                "  job: test",
                "    - Set up Python ${{ matrix.python-version }}",
                "    - Install tree_plus",
                "    - Create .env file",
                "    - Run generic tests",
                "    - Run specific test",
                "  job: deploy",
                "    - Set up Python",
                "    - Install pypa/build",
                "    - Increment Version",
                "    - Build",
                "    - Install",
                "    - Run generic tests",
                "    - Run specific test",
                "    - Update README",
                "    - Build Again",
                "    - Commit Updates",
                "    - Publish to PyPI",
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
        (  # TODO: UNCOMMENT AND VERIFY
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
                "export class AppComponent",
                "  constructor(private loginService: LoginService)",
                "  checkSession()",
                "  async goToEvent(event_id: string)",
                "  valInvitedBy(event: any, event_id: string)",
            ],
        ),
        (
            "tests/more_languages/group5/tickets.component.ts",
            [
                "interface EnrichedTicket extends Ticket",
                "interface SpinConfig",
                "interface RotationState",
                "interface SpeakInput",
                "const formatSpeakInput = (input: SpeakInput): string =>",
                "function hourToSpeech(hour: number, minute: number, period: string): string",
                "export class TicketsComponent implements AfterViewInit",
                "  speak(input: SpeakInput)",
                "  speakEvent(ticket: EnrichedTicket): void",
                "  formatEvent(ticket: EnrichedTicket): string",
                "  speakVenue(ticket: EnrichedTicket): void",
                "  formatDate(date: Date, oneLiner: boolean = false): string",
                "  formatDateForSpeech(date: Date): string",
                """  async spinQRCode(
    event: PointerEvent,
    config: SpinConfig = DEFAULT_SPIN_CONFIG
  )""",
                """  private animateRotation(
    imgElement: HTMLElement,
    targetRotation: number,
    config: SpinConfig,
    cleanup: () => void
  )""",
                "    const animate = (currentTime: number) =>",
                "  private getNext90Degree(currentRotation: number): number",
                "  private getCurrentRotation(matrix: string): number",
                "  ngAfterViewInit()",
                "      const mouseEnterListener = () =>",
                "      const mouseLeaveListener = () =>",
                "  ngOnDestroy()",
                "  toggleColumn(event: MatOptionSelectionChange, column: string)",
                "          (col) =>",
                "  adjustColumns(event?: Event)",
                "  onResize(event: Event)",
                "  async ngOnInit()",
                "  async loadTickets(): Promise<void>",
                """  onDateRangeChange(
    type: "start" | "end",
    event: MatDatepickerInputEvent<Date>
  )""",
                "  applyFilter(column: string): void",
                "  formatDateForComparison(date: Date): string",
                "  constructor(private renderer: Renderer2)",
                "  onFilterChange(event: Event, column: string)",
                "  onLatitudeChange(event: Event)",
                "  onLongitudeChange(event: Event)",
                "  onRadiusChange(event: Event)",
                "  sortData(sort: Sort): void",
                "  onRowClick(event: Event, row: any)",
                "function isDate(value: Date | undefined | null): value is Date",
                "function isNonNullNumber(value: number | null): value is number",
                """function hasLocation(
  ticket: any
): ticket is""",  # TODO: handle the 'ticket is { location: { latitude: number; longitude: number } }' EDGE CASE
                "const create_faker_ticket = async () =>",
                "function compare(a: number | string, b: number | string, isAsc: boolean)",
                "function compare_dates(a: Date, b: Date, isAsc: boolean)",
                "async function mockMoreTickets(): Promise<Ticket[]>",
                "const mockTickets = async () =>",
                "const renderQRCode = async (text: String): Promise<string> =>",
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
                "export class AppRoutingModule",
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
                "export class AppModule",
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
def test_more_languages_group5(
    file: str,
    expected: List[str],
):
    print(f"{file=}")
    result = parse_file(file)
    print(f"{result=}")
    print(f"{expected=}")
    assert result == expected


@pytest.mark.parametrize(
    "file,expected",
    [
        (
            "tests/more_languages/group_lisp/LispTest.lisp",
            [
                "defstruct person",
                "defun greet",
            ],
        ),
        (
            "tests/more_languages/group_lisp/clojure_test.clj",
            [
                "defprotocol P",
                "defrecord Person",
                "defn -main",
                "ns bion.likes_trees",
                "def repo-url",
                "defn config",
                "defmacro with-os",
                "defrecord SetFullElement",
            ],
        ),
        (
            "tests/more_languages/group_lisp/test_scheme.scm",
            [
                "define topological-sort",
                "  define table",
                "  define queue",
                "  define result",
                "  define set-up",
                "  define traverse",
            ],
        ),
        (
            "tests/more_languages/group_lisp/racket_struct.rkt",
            [
                "struct point",
            ],
        ),
    ],
)
def test_more_languages_group_lisp(
    file: str,
    expected: List[str],
):
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
#     "tests/more_languages/group4/fsharp_test.fs",
#     ["type Person -> member this.SayHello", "let person"],
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
