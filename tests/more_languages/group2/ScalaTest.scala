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

object HelloWorld {
  def greet(person: Person): Unit = {
    println(s"Hello, ${person.name}")
  }

  def main(args: Array[String]): Unit = {
    greet(Person("World"))
  }
}

def sumOfSquaresShort(x: Int, y: Int): Int = x * x + y * y