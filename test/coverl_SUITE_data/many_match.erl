-module(many_match).

-export([t/2, t/3]).

t(0, 6) ->
    6;
t(A, B) ->
    B / A.

t(0, 5, 6) ->
    5 + 6;
t(A, B, C) ->
    C / (A + B - 5).
