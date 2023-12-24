// path_to_test/class_function_type.ts
type MyType = 'hello' | 'world';

interface MyInterface {
    id: Number,
    name: String,
}

class TsClass {
    myMethod() {
        console.log("Hello, world!");
    }
}

export class TicketsComponent implements AfterViewInit {
    async myAsyncMethod(
        a: String,
        b: Number,
        c: String,
    ) {}
}

function tsFunction() {
    console.log("Hello, world!");
}

const myArrowFunction = () => {}

const myArrow = (
    a: String,
    b: Number,
    ) => {}
    
const myAsyncArrowFunction = async () => {}
const myAsyncArrow = async (
    a: String,
    b: Number,
) => {}

let myWeirdArrow = () => {}