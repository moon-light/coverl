-module(one_line).

-export([t/0, t1/0]).

t() ->
    ok, ok, ok, ok, ok.

t1() ->
    case case 1 of 1 -> ok; 2 -> not_ok end
    of ok ->
        ok1
    ; not_ok ->
        ok2
    end.
