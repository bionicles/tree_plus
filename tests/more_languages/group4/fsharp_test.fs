// fsharp_test.fs
type Person(name : string) =
    member this.SayHello() = printfn "Hello, %s" name

let person = new Person("World")
person.SayHello()
