// test_fsharp.fs
module TestFSharp

type Person = {
    Name: string
    Age: int
}

let add x y = 
    x + y

let multiply (x: int) (y: int): int =
    x * y

type Result<'T> =
    | Success of 'T
    | Error of string