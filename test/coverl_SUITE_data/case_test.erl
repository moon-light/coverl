-module(case_test).

-export([t/2, case_block_line/0, case_block/0]).

t(A, B) ->
    case A + B
    of 1 -> ok
    ; 2 -> ok1
    ; _ -> ok2
    end.

case_block_line() ->
    case begin ok, 2 end
    of 1 -> ok
    ; 2 -> ok1
    end.

case_block() ->
    case
        begin
            ok,
            2
        end
    of 1 -> ok
    ; 2 -> ok1
    end.
