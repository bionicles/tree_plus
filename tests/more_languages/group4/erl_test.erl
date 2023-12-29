% erl_test.erl
-module(erl_test).

-export([complex_function/1, list_manipulation/1]).


-record(person, {name}).

hello_world() -> 
    Person = #person{name = "World"},
    io:fwrite("Hello, ~p!\n", [Person#person.name]).

-type ra_peer_status() :: normal |
                          {sending_snapshot, pid()} |
                          suspended |
                          disconnected.

-type ra_membership() :: voter | promotable | non_voter | unknown.

-opaque my_opaq_type() :: Type.

-type orddict(Key, Val) :: [{Key, Val}].

-type edge(
        Cases,
        Pwn,
    ) :: [{Tree, Plus}].

-spec guarded(X) -> X when X :: tuple().

-spec edge_case(
        {integer(), any()} | [any()]
    ) -> processed, integer(), any()} | [{item, any()}].

% Type specification for complex_function
-spec complex_function({integer(), any()} | [any()]) -> 
    {processed, integer(), any()} | [{item, any()}].

% Complex function with multiline signature and `-spec`
complex_function({Key, Value}) when is_integer(Key) ->
    % Processing tuple
    {processed, Key, Value};
complex_function(List) when is_list(List) ->
    % Handling list
    lists:map(fun(Item) -> {item, Item} end, List).

% Type specification for list_manipulation
-spec list_manipulation([integer()]) -> [integer()].

% Function manipulating a list with multiline signature
list_manipulation(List) ->
    % Filtering and transforming
    Filtered = lists:filter(fun(X) -> X rem 2 == 0 end, List),
    lists:map(fun(X) -> X * 2 end, Filtered).


-spec overload(T1, T2) -> T3
        ; (T4, T5) -> T6.

-spec multiguard({X, integer()}) -> X when X :: atom()
        ; ([Y]) -> Y when Y :: number().

-record(multiline, {f1 = 42 :: integer(),
            f2      :: float(),
            f3      :: 'a' | 'b'}).

-record(maybe_undefined, {f1 = 42 :: integer(),
            f2      :: 'undefined' | float(),
            f3      :: 'undefined' | 'a' | 'b'}).