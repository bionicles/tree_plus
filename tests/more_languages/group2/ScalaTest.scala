// ScalaTest.scala
def sumOfSquares(x: Int, y: Int): Int = {
  val x2 = x * x
  val y2 = y * y
  x2 + y2
}

trait Bark {
  def bark: String = "Woof"
}

case class Person(name: String)

// Generic class with a multiline constructor
class GenericClass[T](
    val data: T,
    val count: Int
) {
  def getData: T = data
}


object HelloWorld {
  def greet(person: Person): Unit = {
    println(s"Hello, ${person.name}")
  }

  def main(args: Array[String]): Unit = {
    greet(Person("World"))
  }
}

def complexFunction(
    a: Int,
    b: String,
    c: Float
): (Int, String) Option = {
  if (a > 10) Some(a, b) else None
}

def sumOfSquaresShort(x: Int, y: Int): Int = x * x + y * y