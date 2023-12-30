// test_fsharp.fs
module TestFSharp

type Person = {
    Name: string
    Age: int
}

let add x y = 
    x + y

let multiply 
    (x: int) 
    (y: int): int =
    x * y

let complexFunction
    (a: int)
    (b: string)
    (c: float)
    : (int * string) option =
    if a > 10 then Some (a, b)
    else None

type Result<'T> =
    | Success of 'T
    | Error of string
