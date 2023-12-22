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

// Always use std:: for <cmath> functions
template <typename T> T cos(T) = delete;
template <typename T> T sin(T) = delete;
template <typename T> T sqrt(T) = delete;

template<typename T> struct VLEN { static constexpr size_t val=1; };

// template class example from mlx/3rdparty/pocketfft.h
// borrowed here because it caused a parsing bug which
// included the entire file
/*
This file is part of pocketfft.

Copyright (C) 2010-2022 Max-Planck-Society
Copyright (C) 2019-2020 Peter Bell

For the odd-sized DCT-IV transforms:
  Copyright (C) 2003, 2007-14 Matteo Frigo
  Copyright (C) 2003, 2007-14 Massachusetts Institute of Technology

Authors: Martin Reinecke, Peter Bell

All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.
* Redistributions in binary form must reproduce the above copyright notice, this
  list of conditions and the following disclaimer in the documentation and/or
  other materials provided with the distribution.
* Neither the name of the copyright holder nor the names of its contributors may
  be used to endorse or promote products derived from this software without
  specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
template<typename T> class arr
  {
  private:
    T *p;
    size_t sz;

#if defined(POCKETFFT_NO_VECTORS)
    static T *ralloc(size_t num)
      {
      if (num==0) return nullptr;
      void *res = malloc(num*sizeof(T));
      if (!res) throw std::bad_alloc();
      return reinterpret_cast<T *>(res);
      }
    static void dealloc(T *ptr)
      { free(ptr); }
#else
    static T *ralloc(size_t num)
      {
      if (num==0) return nullptr;
      void *ptr = aligned_alloc(64, num*sizeof(T));
      return static_cast<T*>(ptr);
      }
    static void dealloc(T *ptr)
      { aligned_dealloc(ptr); }
#endif

  public:
    arr() : p(0), sz(0) {}
    arr(size_t n) : p(ralloc(n)), sz(n) {}
    arr(arr &&other)
      : p(other.p), sz(other.sz)
      { other.p=nullptr; other.sz=0; }
    ~arr() { dealloc(p); }

    void resize(size_t n)
      {
      if (n==sz) return;
      dealloc(p);
      p = ralloc(n);
      sz = n;
      }

    T &operator[](size_t idx) { return p[idx]; }
    const T &operator[](size_t idx) const { return p[idx]; }

    T *data() { return p; }
    const T *data() const { return p; }

    size_t size() const { return sz; }
  };

// a couple of edge cases which led to issues, borrowed from MLX (c) Apple
class Buffer {
 private:
  void* ptr_;

std::tuple<array, array, array> quantize(
    const array& w,
    int group_size /* = 64 */,
    int bits /* = 4 */,
    StreamOrDevice s /* = {} */) {
  if (w.ndim() != 2) {
    throw std::invalid_argument("[quantize] Only matrices supported for now");
  }
