# crystal_test.cr
class Person
  getter name : String

  def initialize(@name : String)
  end

  def greet
    "Hello, #{name}"
  end
end

def say_hello(person : Person)
  puts person.greet
end
