// swift_test.swift
import Swift

class Person {
    var name: String

    init(name: String) {
        self.name = name
    }

    func greet() {
        print("Hello, \(self.name)!")
    }
}

struct CustomType {
    var field: Int
}

func sayHello(to name: String, onDay day: String) -> String {
    return "Hello \(name), the day is \(day)"
}