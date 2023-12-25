package require Tk

proc sayHello {} {
    puts "Hello, World!"
}

button .hello -text "Hello" -command sayHello
pack .hello