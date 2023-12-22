// cpp_test.cpp
#include <iostream>

class Person
{
    std::string name;

public:
    Person(std::string n) : name(n) {}
    void greet()
    {
        std::cout << "Hello, " << name << "\n";
    }
};

void globalGreet()
{
    std::cout << "Global greeting\n";
}

int main()
{
    Person person("World");
    person.greet();
    globalGreet();
    return 0;
}

#include <iostream>
#include <vector>
#include <map>
#include <string>
#include <functional>

// Simple global function
void printMessage(const std::string &message) {
    std::cout << message << std::endl;
}

// Template function
template<typename T>
void printVector(const std::vector<T>& vec) {
    for (const auto& item : vec) {
        std::cout << item << " ";
    }
    std::cout << std::endl;
}

// Struct example
struct Point {
    int x, y;
    Point(int x, int y) : x(x), y(y) {}
};

// Class example
class Animal {
public:
    Animal(const std::string &name) : name(name) {}
    virtual void speak() const = 0;
    virtual ~Animal() = default;
protected:
    std::string name;
};

class Dog : public Animal {
public:
    Dog(const std::string &name) : Animal(name) {}
    void speak() const override {
        std::cout << name << " says Woof!" << std::endl;
    }
};

class Cat : public Animal {
public:
    Cat(const std::string &name) : Animal(name) {}
    void speak() const override {
        std::cout << name << " says Meow!" << std::endl;
    }
};

nb::bytes BuildRnnDescriptor(int input_size, int hidden_size, int num_layers,
                             int batch_size, int max_seq_length, float dropout,
                             bool bidirectional, bool cudnn_allow_tf32,
			     int workspace_size, int reserve_space_size/* = {} */) {
  return PackDescriptor(RnnDescriptor{
      input_size, hidden_size, num_layers, batch_size, max_seq_length, dropout,
      bidirectional, cudnn_allow_tf32, workspace_size, reserve_space_size
  });
}

// Main function
int main() {
    printMessage("Hello, world!");

    std::vector<int> numbers = {1, 2, 3, 4, 5};
    printVector(numbers);

    Point p(10, 20);
    std::cout << "Point: (" << p.x << ", " << p.y << ")" << std::endl;

    Dog dog("Buddy");
    Cat cat("Whiskers");

    dog.speak();
    cat.speak();

    return 0;
}

// Enums are a way to assign a value to a constant most commonly used for
// easier visualization and reading of code
enum ECarTypes
{
  Sedan,
  Hatchback,
  SUV,
  Wagon
};

ECarTypes GetPreferredCarType()
{
    return ECarTypes::Hatchback;
}

enum ECarTypes : uint8_t
{
  Sedan, // 0
  Hatchback, // 1
  SUV = 254, // 254
  Hybrid // 255
};

// On the other hand you may not want enums to be accidentally cast to an integer
// type or to other enums so it is instead possible to create an enum class which
// won't be implicitly converted
enum class ECarTypes : uint8_t
{
  Sedan, // 0
  Hatchback, // 1
  SUV = 254, // 254
  Hybrid // 255
};

void myFunction(string fname, int age) {
  cout << fname << " Refsnes. " << age << " years old. \n";
}
