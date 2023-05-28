// ScalaTest.scala
case class Person(name: String)

object HelloWorld {
  def greet(person: Person): Unit = {
    println(s"Hello, ${person.name}")
  }

  def main(args: Array[String]): Unit = {
    greet(Person("World"))
  }
}
