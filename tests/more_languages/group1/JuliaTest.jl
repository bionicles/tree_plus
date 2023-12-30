# JuliaTest.jl
module JuliaTest_EdgeCase

export julia_is_awesome

struct Location
    name::String 
    lat::Float32 # all i do is edge case
    lon::Float32
end


# thank you https://www.matecdev.com/posts/julia-structs.html#defining-a-struct
mutable struct mPerson
    name::String
    age::Int
end

Base.@kwdef mutable struct Param
    Δt::Float64 = 0.1
    n::Int64
    m::Int64
end

    sic(x,y) = x + y

welcome(l::Location) = println("Welcome to ", l.name, l.lat, l.lon)
∑(α, Ω) = x + y

function noob()
    println("hello world!")
end

function ye_olde(hello::String, world::Location)
    println(hello, world)
end

function multiline_greet(
        p::mPerson, 
        greeting::String # edge case
    )  where S<:Array{T} where T<:Number
    println(greeting, " ", p.name, ", who is ", p.age, " years old.")
end

# https://github.com/SciML/DifferentialEquations.jl/blob/6d05a21fad3c595815f3f3e393ddf9221c6bd9b1/src/dae_default_alg.jl#L1
function julia_is_awesome(prob::DiffEqBase.AbstractDAEProblem{uType, duType, tType,
        isinplace}; # edge case
    kwargs...) where {uType, duType, tType, isinplace}
    o = Dict{Symbol, Any}(kwargs)
    extra_kwargs = Any[]
    alg = IDA() # Standard default
    uEltype = eltype(prob.u0)

    alg_hints = get_alg_hints(o)

    # If adaptivity is not set and the tType is not a float, turn off adaptivity
    # Bad interaction with ForwardDiff
    #!(tType <: AbstractFloat) && (:adaptive ∉ keys(o)) && push!(extra_kwargs,:adaptive=>false)

    alg, extra_kwargs
end

end
