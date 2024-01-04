// tests/more_languages/group1/test.ts
type MyType = "hello" | "world";

interface MyInterface {
  id: number;
  name: string;
}

class TsClass {
  myMethod() {
    console.log("Hello, world!");
  }

  myMethodWithArgs(param1: string, param2: number): void {
    console.log("Method in TsClass");
  }

  static myStaticMethod<T>(param: T): T {
    return param;
  }

  // Multiline method
  multilineMethod(
    c: number, // comments prevent auto-reformatting!
    d: number
  ): number {
    return c + d;
  }

  // Multiline method with default parameters
  multilineMethodWithDefaults(
    t: string = "tree", // edge case
    p: string = "plus"
  ): string {
    return t + p;
  }
}

export class AdvancedComponent implements MyInterface {
  id: number;
  name: string;

  // Async method with multiline signature
  async myAsyncMethod(
    a: string, // edge
    b: number, // case
    c: string
  ): Promise<void> {
    // Async method content
  }

  // Generic method with multiline signature
  genericMethod<T, U>(
    arg1: T,
    arg2: U // edge cases
  ): [T, U] {
    return [arg1, arg2];
  }
}

export class TicketsComponent implements MyInterface {
  id: number;
  name: string;

  // Async method with destructuring
  async myAsyncMethod({ a, b, c }: { a: String; b: Number; c: String }) {
    // Async method content
  }
}

function tsFunction() {
  console.log("Hello, world!");
}

// Standalone function with multiline signature
function tsFunctionSigned(
  param1: number,
  param2: number /* here's an edge case */
): void {
  console.log("Standalone function with parameters");
}

export default async function tsFunctionComplicated<A, B, C>({
  a = 1 | 2 /* edge cases */,
  b = "bob",
  c = async () => "charlie",
}: {
  a: number; // r us
  b: string;
  c: () => Promise<string>;
}): Promise<string> {
  return("Standalone function with parameters");
}

// Arrow function with destructuring and multiline signature
const tsArrowFunctionSigned = ({
  a /* edge cases */,
  b,
}: {
  a: number; // r us
  b: string;
}) => {
  console.log("Arrow Function with destructuring");
};

export const tsComplicatedArrow = async ({
  a = 1 | 2 /* edge cases */,
  b = "bob",
  c = async () => "charlie",
}: {
  a: number; // r us
  b: string;
  c: () => Promise<string>;
}): Promise<string> => {
  return "Standalone function with parameters";
}

const arrowFunction = () => {};

const arrow = (a: String, b: Number) => {};

const asyncArrowFunction = async () => {};

const asyncArrow = async (a: String, b: Number) => {};

let weirdArrow = () => {};

const asyncPromiseArrow = async (): Promise<void> => {
  // Async arrow function
};

let myWeirdArrowSigned = (x: number): number => x * 2;

class Person {
  constructor(private firstName: string, private lastName: string) {}

  getFullName(): string {
    return `${this.firstName} ${this.lastName}`;
  }

  describe(): string {
    return `This is ${this.firstName} ${this.lastName}`;
  }
}

class Employee extends Person {
  constructor(
    firstName: string,
    lastName: string, // edge case
    private jobTitle: string
  ) {
    super(firstName, lastName);
  }

  describe(): string {
    return `${super.describe()} and works as a ${this.jobTitle}`;
  }
}

interface Shape {
  color: string;
}

interface Square extends Shape {
  sideLength: number;
}