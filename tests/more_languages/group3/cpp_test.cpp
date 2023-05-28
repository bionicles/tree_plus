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
