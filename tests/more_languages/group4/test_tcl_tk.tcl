package require Tk

proc sayHello {} {
    puts "Hello, World!"
}

button .hello -text "Hello" -command sayHello
pack .hello

proc arrg { input } { return 1 }

proc multiLine {
    x,
    y
} { 
    puts "Tree, Plus!"
}