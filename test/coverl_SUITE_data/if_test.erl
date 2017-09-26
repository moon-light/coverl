-module(if_test).

-export([t/2]).

t(T1, T2) ->
    if T1; T2 -> ok
    ; true -> ok1
    end.
