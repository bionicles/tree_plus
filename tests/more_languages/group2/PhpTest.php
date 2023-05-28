<?php
// PhpTest.php
class HelloWorld {
  function sayHello() {
    echo "Hello, World!";
  }
}

function greet() {
  $instance = new HelloWorld();
  $instance->sayHello();
}

class Person {
    public $name;
    function __construct($name) {
        $this->name = $name;
    }
}
