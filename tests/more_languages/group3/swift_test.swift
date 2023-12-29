// swift_test.swift
import Foundation

class Person {
    var name: String

    init(name: String) {
        self.name = name
    }

    func greet() {
        print("Hello, \(name)")
    }

    func yEdgeCase(
        fname: String, 
        lname: String, 
        age: Int, // { get wrecked by this comment }
        address: String, 
        phoneNumber: String
    ) {
        print("\(fname) \(lname) is \(age) years old.")
        print("Address: \(address), Phone: \(phoneNumber)")
    }
}

func globalGreet() {
    print("Global greeting")
}

struct Point {
    var x: Int
    var y: Int
}

protocol Animal {
    var name: String { get }
    func speak()
}

struct Dog: Animal {
    // bark
}

class Cat: Animal {
    var name: String

    init(name: String) {
        self.name = name
    }

    func speak() {
        print("\(name) says Meow!")
    }
}

// Enums with associated values
enum CarType {
    case sedan
    case hatchback
    case suv
    case wagon
}

func getPreferredCarType() -> CarType {
    return .hatchback
}

// Enums with raw values
enum CarType: UInt8 {
    case sedan = 0
    case hatchback = 1
    case suv = 254
    case hybrid = 255
}

enum class CarType: UInt8 {
    case sedan = 0
    case hatchback = 1
    case suv = 254
    case hybrid = 255
}

func myFunction(fname: String, age: Int) {
    print("\(fname) Refsnes. \(age) years old.")
}

func myFunctionWithMultipleParameters(
    fname: String, 
    lname: String, 
    age: Int, 
    address: String, 
    phoneNumber: String
) {
    print("\(fname) \(lname) is \(age) years old.")
    print("Address: \(address), Phone: \(phoneNumber)")
}

// Entry point
let person = Person(name: "World")
person.greet()
globalGreet()

let point = Point(x: 10, y: 20)
print("Point: (\(point.x), \(point.y))")

let dog = Dog(name: "Buddy")
let cat = Cat(name: "Whiskers")
dog.speak()
cat.speak()
