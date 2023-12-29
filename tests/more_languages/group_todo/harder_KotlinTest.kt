// harder_KotlinTest.kt

package foo

private fun foo() { ... } // visible inside example.kt

public var bar: Int = 5 // property is visible everywhere
    private set         // setter is visible only in example.kt

internal val baz = 6    // visible inside the same module

data class Person(val name: String, val age: Int)

fun greet(person: Person) = println("Hello, ${person.name}, age ${person.age}")

class MathOperations {
    fun add(a: Int, b: Int): Int = a + b

    fun multiply(a: Int, b: Int): Int {
        return a * b
    }

    // Multiline function signature
    fun divideWithRemainder(
        numerator: Int,
        denominator: Int
    ): Pair<Int, Int> {
        val quotient = numerator / denominator
        val remainder = numerator % denominator
        return Pair(quotient, remainder)
    }
}

class GenericRepository<T> {
    fun getItem(id: Int): T? {
        // Implementation
        return null
    }
}

interface Transformable<T> {
    fun transform(input: T): T
}

class AdvancedCalculator : MathOperations(), Transformable<Int> {
    override fun transform(input: Int): Int {
        return multiply(input, input)
    }

    companion object {
        fun subtract(a: Int, b: Int): Int = a - b
    }
}

fun <T> processItems(items: List<T>, processor: (T) -> Unit) {
    items.forEach { processor(it) }
}

interface Source<out T> {
    fun nextT(): T
}

fun demo(strs: Source<String>) {
    val objects: Source<Any> = strs // This is OK, since T is an out-parameter
    // ...
}

// Kotlin provides a complementary variance annotation: in. 
// It makes a type parameter contravariant, meaning it can only be consumed and never produced
interface Comparable<in T> {
    operator fun compareTo(other: T): Int
}

fun demo(x: Comparable<Number>) {
    x.compareTo(1.0) // 1.0 has type Double, which is a subtype of Number
    // Thus, you can assign x to a variable of type Comparable<Double>
    val y: Comparable<Double> = x // OK!
}

// extension function
fun MutableList<Int>.swap(index1: Int, index2: Int) {
    val tmp = this[index1] // 'this' corresponds to the list
    this[index1] = this[index2]
    this[index2] = tmp
}

// nullable receiver
fun Any?.toString(): String {
    if (this == null) return "null"
    // After the null check, 'this' is autocast to a non-nullable type, so the toString() below
    // resolves to the member function of the Any class
    return toString()
}

// extension properties
val <T> List<T>.lastIndex: Int
    get() = size - 1

fun fill(dest: Array<in String>, value: String) { ... }

fun <T> singletonList(item: T): List<T> {
    // ...
}

fun <T> T.basicToString(): String { // extension function
    // ...
}

fun <T : Comparable<T>> sort(list: List<T>) {  ... }


fun <T> copyWhenGreater(list: List<T>, threshold: T): List<String>
    where T : CharSequence,
          T : Comparable<T> {
    return list.filter { it > threshold }.map { it.toString() }
}

import org.jetbrains.annotations.*;

public interface Game<T> {
    public T save(T x) {}
    @NotNull
    public T load(@NotNull T x) {}
}

interface ArcadeGame<T1> : Game<T1> {
    override fun save(x: T1): T1
    // T1 is definitely non-nullable
    override fun load(x: T1 & Any): T1 & Any
}

data class User(val name: String, val age: Int)

enum class Direction {
    NORTH, SOUTH, WEST, EAST
}

enum class Color(val rgb: Int) {
    RED(0xFF0000),
    GREEN(0x00FF00),
    BLUE(0x0000FF)
}

enum class ProtocolState {
    WAITING {
        override fun signal() = TALKING
    },

    TALKING {
        override fun signal() = WAITING
    };

    abstract fun signal(): ProtocolState
}

// No need for the open keyword here, it’s already open by default
abstract class Animated {

    // This virtual function is already open by default as well
    abstract fun animate()
  
    open fun stopAnimating() { }

    fun animateTwice() { }
}

// the following function takes a lambda, f, and executes f passing it the string "lambda"
// note that (String) -> Unit indicates a lambda with a String parameter and Unit return type
fun executeLambda(f: (String) -> Unit) {
    f("lambda")
}

fun sayHello(maybe: String?, neverNull: Int) {
    // use of Elvis operator
    val name: String = maybe ?: "stranger"
    println("Hello $name")
}

// Class is visible only to current module
internal open class TalkativeButton {
    // method is only visible to current class 
    private fun yell() = println("Hey!")
    // method is visible to current class and derived classes
    protected fun whisper() = println("Let's talk!")
}
internal class MyTalkativeButton: TalkativeButton() {
    fun utter() = super.whisper()
}
MyTalkativeButton().utter()

enum class IntArithmetics : BinaryOperator<Int>, IntBinaryOperator {
    PLUS {
        override fun apply(t: Int, u: Int): Int = t + u
    },
    TIMES {
        override fun apply(t: Int, u: Int): Int = t * u
    };

    override fun applyAsInt(t: Int, u: Int) = apply(t, u)
}

sealed interface Error

sealed class IOError(): Error

class FileReadError(val file: File): IOError()
class DatabaseError(val source: DataSource): IOError()

object RuntimeError : Error

sealed interface Error // has implementations only in same package and module

sealed class IOError(): Error // extended only in same package and module
open class CustomError(): Error // can be extended wherever it's visible

fun log(e: Error) = when(e) {
    is FileReadError -> { println("Error while reading file ${e.file}") }
    is DatabaseError -> { println("Error while reading from database ${e.source}") }
    is RuntimeError ->  { println("Runtime error") }
    // the `else` clause is not required because all the cases are covered
}

object Runner {
    inline fun <reified S: SomeClass<T>, T> run() : T {
        return S::class.java.getDeclaredConstructor().newInstance().execute()
    }
}

enum class RGB { RED, GREEN, BLUE }

inline fun <reified T : Enum<T>> printAllValues() {
    println(enumValues<T>().joinToString { it.name })
}

fun powerOf(
    number: Int,
    exponent: Int, // trailing comma
) { /*...*/ }

fun read(
    b: ByteArray,
    off: Int = 0,
    len: Int = b.size,
) { /*...*/ }


open class A {
    open fun foo(i: Int = 10) { /*...*/ }
}

class B : A() {
    override fun foo(i: Int) { /*...*/ }  // No default value is allowed.
}

fun foo(
    bar: Int = 0,
    baz: Int = 1,
    qux: () -> Unit,
) { /*...*/ }

fun reformat(
    str: String,
    normalizeCase: Boolean = true,
    upperCaseFirstLetter: Boolean = true,
    divideByCamelHumps: Boolean = false,
    wordSeparator: Char = ' ',
) { /*...*/ }

fun printHello(name: String?): Unit {
    if (name != null)
        println("Hello $name")
    else
        println("Hi there!")
    // `return Unit` or `return` is optional
}

value class Password(private val s: String)

fun String.lastChar(): Char = get(length - 1)

val helloWorld = object {
    val hello = "Hello"
    val world = "World"
    // object expressions extend Any, so `override` is required on `toString()`
    override fun toString() = "$hello $world"
}

fun interface KRunnable {
   fun invoke()
}

class User(val id: Int, val name: String, val address: String)
    
fun saveUserToDb(user: User) {
    fun validate(user: User, value: String, fieldName: String) {
        require(value.isNotEmpty()) { "Can't save user ${user.id}: empty $fieldName" }
    }
    
    validate(user, user.name, "Name") 
    validate(user, user.address, "Address")
    // Save user to the database 
    ...
}

fun interface IntPredicate {
   fun accept(i: Int): Boolean
}

fun double(x: Int): Int = x * 2
fun implicitDouble(x: Int) = x * 2

fun <T> asList(vararg ts: T): List<T> {
    val result = ArrayList<T>()
    for (t in ts) // ts is an Array
        result.add(t)
    return result
}

// open on the class means this class will allow derived classes
open class MegaButton {

    // no-open on a function means that 
    //    polymorphic behavior disabled if function overridden in derived class
    fun disable() { ... }

    // open on a function means that
    //    polymorphic behavior allowed if function is overridden in derived class
    open fun animate() { ... }
}

class GigaButton: MegaButton() {

    // Explicit use of override keyword required to override a function in derived class
    override fun animate() { println("Giga Click!") } 
}

open class A(x: Int) {
    public open val y: Int = x
}

interface B { /*...*/ }

interface A {
    fun funFromA() {}
}
interface B

class C {
    // The return type is Any; x is not accessible
    fun getObject() = object {
        val x: String = "x"
    }

    // The return type is A; x is not accessible
    fun getObjectA() = object: A {
        override fun funFromA() {}
        val x: String = "x"
    }

    // The return type is B; funFromA() and x are not accessible
    fun getObjectB(): B = object: A, B { // explicit return type is required
        override fun funFromA() {}
        val x: String = "x"
    }
}

val ab: A = object : A(1), B {
    override val y = 15
}

class C {
    private fun getObject() = object {
        val x: String = "x"
    }

    fun printX() {
        println(getObject().x)
    }
}

infix fun Int.shl(x: Int): Int { ... }

class MyStringCollection {
    infix fun add(s: String) { /*...*/ }

    fun build() {
        this add "abc"   // Correct
        add("abc")       // Correct
        //add "abc"        // Incorrect: the receiver must be specified
    }
}

val eps = 1E-10 // "good enough", could be 10^-15

tailrec fun findFixPoint(x: Double = 1.0): Double =
    if (Math.abs(x - Math.cos(x)) < eps) x else findFixPoint(Math.cos(x))

interface IndexedContainer {
    operator fun get(index: Int)
}

class OrdersList: IndexedContainer {
    override fun get(index: Int) { /*...*/ }
}

data class Point(val x: Int, val y: Int)

operator fun Point.unaryMinus() = Point(-x, -y)

class Derived(b: Base) : Base by b
