-module(guard).

-export([t/1]).

t(A) when is_list(A) ->
    list;
t(A) when is_binary(A); is_bitstring(A) ->
    binary;
t(A) when is_integer(A), A > 10; is_float(A), A < 1.0 ->
    num;
t(_) ->
    term.
