// csharp_test.cs
using System;

namespace HelloWorldApp {
    class Person {
        private string name;
        public Person(string name) {
            this.name = name;
        }
        public void Greet() {
            Console.WriteLine("Hello, " + name);
        }
    }

    class HelloWorld {
        static void Main(string[] args) {
            Person person = new Person("World");
            person.Greet();
        }
    }
}
