-- haskell_test.hs
data Person = Person String

greet :: Person -> String
greet (Person name) = "Hello, " ++ name
