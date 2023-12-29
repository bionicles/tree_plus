// KotlinTest.kt
data class Person(val name: String)

fun greet(person: Person) = println("Hello, ${person.name}")

// lambda functions
fun <T> processItems(items: List<T>, processor: (T) -> Unit) {
    items.forEach { processor(it) }
}

// generic interface
interface Source<out T> {
    fun nextT(): T
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

// tail recursion
val eps = 1E-10 // "good enough", could be 10^-15

tailrec fun findFixPoint(x: Double = 1.0): Double =
    if (Math.abs(x - Math.cos(x)) < eps) x else findFixPoint(Math.cos(x))

// generic class
class GenericRepository<T> {
    fun getItem(id: Int): T? {
        // Implementation
        return null
    }
}

// sealed interface and class
sealed interface Error
sealed class IOError(): Error

// named object 
object Runner {
    // inline functions with reified type parameters
    inline fun <reified S: SomeClass<T>, T> run() : T {
        return S::class.java.getDeclaredConstructor().newInstance().execute()
    }
}


// infix functions and operator overloading
infix fun Int.shl(x: Int): Int { ... }

class MyStringCollection {
    infix fun add(s: String) { /*...*/ }

    val unNecessary: String get() = super.annoyingEdgeCases

    fun build() {
        this add "abc"   // Correct
        add("abc")       // Correct
        //add "abc"        // Incorrect: the receiver must be specified
    }
}

// inheritance
open class Base(p: Int)

class Derived(p: Int) : Base(p)

// overriding properties
open class Shape {
    open fun draw() { /*...*/ }
    fun fill() { /*...*/ }
    open fun edge(case: Int) {
        return null
    }
}

interface Thingy {
    fun edge() {
        // case
        return null
    }
}

// simultaneous inheritance and implementation
class Circle() : Shape(), Thingy {
    override fun draw() { /*...*/ }
    final override fun edge(case: Int) {
        super<Thingy>.edge()
        super<Shape>.edge(1)
        println("GOTCHA")
        return null    
    }
}


// delegation
interface Base {
    fun print()
}

class BaseImpl(val x: Int) : Base {
    override fun print() { print(x) }
}

internal class Derived(b: Base) : Base by b

// for the madlads who use constructor
class Person constructor(firstName: String) { /*...*/ }

// and the madlads with multiline class 
class People(
    firstNames: Array<String>, /* heard you like */
    ages: Array<Int>(42), // edge cases galore
) {
    fun edgeCases(): Boolean {
        return True
    }
}

// of course we gotta keep going
class Alien public @Inject constructor(
    val firstName: String,
    val lastName: String,
    var age: Int, // trailing comma
    val pets: MutableList<Pet> = mutableListOf(),
) { 
    fun objectOriented(): String {
        return "if necessary"
    }
 }

 // i would like to stop now but kotlin keeps having more language patterns
 enum class IntArithmetics : BinaryOperator<Int>, IntBinaryOperator {
    PLUS {
        override fun apply(t: Int, u: Int): Int = t + u
    },
    TIMES {
        override fun apply(t: Int, u: Int): Int = t * u
    };

    override fun applyAsInt(t: Int, u: Int) = apply(t, u)
}

// multiline function signatures with default arguments
fun reformat(
    str: String,
    normalizeCase: Boolean = true,
    upperCaseFirstLetter: Boolean = true,
    divideByCamelHumps: Boolean = false,
    wordSeparator: Char = ' ',
) { 
    /* ... */
}


// last one plz
operator fun Point.unaryMinus() = Point(-x, -y)

//
abstract class Polygon {
    abstract fun draw()
}