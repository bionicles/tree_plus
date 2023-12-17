% erl_test.erl
-module(hello_world).
-export([hello_world/0]).

-record(person, {name}).

hello_world() -> 
    Person = #person{name = "World"},
    io:fwrite("Hello, ~p!\n", [Person#person.name]).
