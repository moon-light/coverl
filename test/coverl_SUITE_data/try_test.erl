-module(try_test).

-export([t/2, t1/1]).

t(Fun, Guard) ->
    try Fun()
    of ok -> ok
    ; error -> error
    ; _ when Guard -> true
    ; _ -> false
    catch _ -> value1
    ; error:A -> value2
    ; _:A when Guard -> value3
    ; _:_ -> value4
    end.

t1(Fun) ->
    try Fun()
    catch _ -> value1
    after ok
    end.
