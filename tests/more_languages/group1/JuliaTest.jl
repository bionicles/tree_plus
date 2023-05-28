# JuliaTest.jl
module JuliaTest

export Person, greet

struct Person
    name::String
end

greet(p::Person) = println("Hello, ", p.name)

end
