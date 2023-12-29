// tests/more_languages/group1/test.js
class MyClass {
  myMethod() {
    console.log("Hello, world!");
  }

  async asyncMethod(a, b) {
    return a + b;
  }

  methodWithDefaultParameters(a = 5, b = 10) {
    console.log(a, b);
  }

  multilineMethod(
    c, // prevent formatting
    d
  ) {}

  multilineMethodWithDefaults(
    t = "tree", // prevent formatting
    p = "plus"
  ) {}
}

function myFunction(param1, param2) {
  console.log("Function with parameters:", param1, param2);
}

function multilineFunction(
  param1, // prevent formatting
  param2
) {
  console.log("Function with parameters:", param1, param2);
}

const arrowFunction = () => {
  console.log("Arrow Function");
};

const parametricArrow = (a, b) => a + b;

// Immediately Invoked Function Expression (IIFE)
(function () {
  console.log("IIFE runs immediately");
})();

// Nested functions
function outerFunction(outerParam) {
  function innerFunction(innerParam) {
    console.log("Nested function:", outerParam, innerParam);
  }
  innerFunction("inner");
}

// Function within an object
const myObject = {
  myMethod: function (stuff) {
    console.log(stuff);
  },
};

let myArrowObject = {
  myArrow: ({
    a,
    b /* gotta test em all */,
    c, // lol seriously no reformat
  }) => {
    console.log("Method in object");
  },
};

// Async arrow function
const myAsyncArrowFunction = async () => {
  const result = await Promise.resolve("Async result");
  console.log(result);
};

// Function with rest parameters
function functionWithRestParameters(...args) {
  console.log("Rest parameters:", args);
}

// Named function expression
const namedFunctionExpression = function myNamedFunction() {
  console.log("Named function expression");
};

// Arrow function with multiline body
const multilineArrowFunction = (
  a, // comment to prevent reformat
  b
) => {
  const sum = a + b;
  return sum;
};

// Function returning a function
function functionReturningFunction() {
  return function () {
    console.log("Returned from function");
  };
}

function destructuringOnMultipleLines({
  a, // edge case
  b,
}) {
  console.log("Destructured values:", a, b);
}

// Arrow function with destructuring
const arrowFunctionWithDestructuring = ({ a, b }) => {
  console.log("Destructured values:", a, b);
};

const multilineDestructuringArrow = ({
  a, // edge case
  b,
}) => {
  console.log("Destructured values:", a, b);
};

// Async function with try-catch
async function asyncFunctionWithErrorHandling() {
  try {
    const result = await Promise.reject("Error occurred");
    console.log(result);
  } catch (error) {
    console.error("Caught error:", error);
  }
}

// Exporting functions and class
export { myFunction, MyClass, myArrowFunction };

class Car {
  constructor(brand) {
    this.carname = brand;
  }
  present() {
    return "I have a " + this.carname;
  }
}

class Model extends Car {
  constructor(brand, mod) {
    super(brand);
    this.model = mod;
  }
  show() {
    return this.present() + ", it is a " + this.model;
  }
}
