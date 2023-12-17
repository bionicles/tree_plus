// dart_test.dart
enum GreetingType { formal, casual }

class HelloWorld {
  GreetingType type;
  
  HelloWorld(this.type);
  
  void sayHello() {
    switch(this.type) {
      case GreetingType.formal:
        print('Hello, World!');
        break;
      case GreetingType.casual:
        print('Hey, World!');
        break;
    }
  }
}

void main() {
  HelloWorld hello = new HelloWorld(GreetingType.formal);
  hello.sayHello();
}
