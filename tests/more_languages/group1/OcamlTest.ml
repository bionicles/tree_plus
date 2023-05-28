(* OcamlTest.ml *)
type color = Red | Green | Blue

class hello =
object
    method say_hello = print_endline "Hello, World!"
end

let main () =
  let greeting = new hello in
  greeting#say_hello

