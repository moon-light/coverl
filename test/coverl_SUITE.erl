-module(coverl_SUITE).

-export([all/0]).
-export([groups/0]).

-export([end_per_suite/1]).
-export([end_per_testcase/2]).
-export([init_per_suite/1]).
-export([init_per_testcase/2]).

-export([case_test/1]).
-export([data/1]).
-export([guard/1]).
-export([if_test/1]).
-export([many_match/1]).
-export([no_arg/1]).
-export([one_line/1]).
-export([one_match_arg/1]).
-export([try_test/1]).

all() ->
    [{group, cover_test}, data].

groups() ->
    [ {cover_test, [parallel],
        [ no_arg, one_match_arg, one_line, many_match, guard, try_test, if_test
        , case_test
        ]}
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(coverl),
    Config.

end_per_suite(_) ->
    ok.

init_per_testcase(Name, Config) ->
    [{Name, ok}] = coverl:compile_module(Name),
    Config.

end_per_testcase(Name, _) ->
    coverl:reset(Name),
    ok.

no_arg(_) ->
    no_arg:t(),
    {[{Line, 1}], [{{CDLine, ID, CDNum, [], []}, 1}]} =
        check(?FUNCTION_NAME, coverl:report([?FUNCTION_NAME], raw, #{})),
    no_arg:t(),
    {[{Line, 2}], [{{CDLine, ID, CDNum, [], []}, 2}]} =
        check(?FUNCTION_NAME, coverl:report([?FUNCTION_NAME], raw, #{})),
    coverl:reset(no_arg),
    {[{Line, 0}], [{{CDLine, ID, CDNum, [], []}, 0}]} =
        check(?FUNCTION_NAME, coverl:report([?FUNCTION_NAME], raw, #{})),
    no_arg:t(),
    {[{Line, 1}], [{{CDLine, ID, CDNum, [], []}, 1}]} =
        check(?FUNCTION_NAME, coverl:report([?FUNCTION_NAME], raw, #{})),
    1 = Line - CDLine.

check(Module, Data) ->
    File = "test/coverl_SUITE_data/" ++ atom_to_list(Module) ++ ".erl",
    #{Module := #{File := #{line := LineInfo, branch := BranchInfo}}} = Data,
    {maps:to_list(LineInfo), maps:to_list(BranchInfo)}.

one_match_arg(_) ->
    one_match_arg:t(1),
    {[{6, 1}, {8, 0}],
        [ {{5, ID, 1, [false], []}, 0}, {{5, ID, 1, [true], []}, 1}
        , {{7, ID, 2, [any], []}, 0}
        ]} =
        check(?FUNCTION_NAME, coverl:report([?FUNCTION_NAME], raw, #{})),
    one_match_arg:t(2),
    {[{6, 1}, {8, 1}],
        [ {{5, ID, 1, [false], []}, 1}, {{5, ID, 1, [true], []}, 1}
        , {{7, ID, 2, [any], []}, 1}
        ]} =
        check(?FUNCTION_NAME, coverl:report([?FUNCTION_NAME], raw, #{})).

one_line(_) ->
    one_line:t(),
    one_line:t1(),
    {[{6, 5}, {9, 2}, {11, 1}, {13, 0}],
        [ {{5, _, 1, [], []}, 1}
        , {{8, _, 1, [], []}, 1}
        , {{9, ID1, 1, [false], []}, 0}, {{9, ID1, 1, [true], []}, 1}
        , {{9, ID1, 2, [true], []}, 0}
        , {{10, ID2, 1, [false], []}, 0}, {{10, ID2, 1, [true], []}, 1}
        , {{12, ID2, 2, [true], []}, 0}
        ]} =
        check(?FUNCTION_NAME, coverl:report([?FUNCTION_NAME], raw, #{})).

many_match(_) ->
    many_match:t(0, 6),
    {[{6, 1}, {8, 0} | _],
        [ {{5, ID1, 1, [false], []}, 0}, {{5, ID1, 1, [true, false], []}, 0}
        , {{5, ID1, 1, [true, true], []}, 1}
        , {{7, ID1, 2, [any, any], []}, 0}
        | _]} =
        check(?FUNCTION_NAME, coverl:report([?FUNCTION_NAME], raw, #{})),
    many_match:t(1, 6),
    many_match:t(4, 5),
    {[{6, 1}, {8, 2} | _],
        [ {{5, ID1, 1, [false], []}, 2}, {{5, ID1, 1, [true, false], []}, 0}
        , {{5, ID1, 1, [true, true], []}, 1}
        , {{7, ID1, 2, [any, any], []}, 2}
        | _]} =
        check(?FUNCTION_NAME, coverl:report([?FUNCTION_NAME], raw, #{})),
    {'EXIT', {badarith, _}} = (catch many_match:t(0, 5)),
    {[{6, 1}, {8, 3} | _],
        [ {{5, ID1, 1, [false], []}, 2}, {{5, ID1, 1, [true, false], []}, 1}
        , {{5, ID1, 1, [true, true], []}, 1}
        , {{7, ID1, 2, [any, any], []}, 3}
        | _]} =
        check(?FUNCTION_NAME, coverl:report([?FUNCTION_NAME], raw, #{})),
    many_match:t(0, 5, 6),
    {[_, _, {11, 1}, {13, 0}],
        [ _, _, _, _
        , {{10, ID2, 1, [false], []}, 0}, {{10, ID2, 1, [true, false], []}, 0}
        , {{10, ID2, 1, [true, true, false], []}, 0}
        , {{10, ID2, 1, [true, true, true], []}, 1}
        , {{12, ID2, 2, [any, any, any], []}, 0}
        ]} =
        check(?FUNCTION_NAME, coverl:report([?FUNCTION_NAME], raw, #{})),
    many_match:t(0, 6, 6),
    many_match:t(1, 5, 6),
    many_match:t(1, 6, 7),
    {[_, _, {11, 1}, {13, 3}],
        [ _, _, _, _
        , {{10, ID2, 1, [false], []}, 2}, {{10, ID2, 1, [true, false], []}, 1}
        , {{10, ID2, 1, [true, true, false], []}, 0}
        , {{10, ID2, 1, [true, true, true], []}, 1}
        , {{12, ID2, 2, [any, any, any], []}, 3}
        ]} =
        check(?FUNCTION_NAME, coverl:report([?FUNCTION_NAME], raw, #{})),
    {'EXIT', {badarith, _}} = (catch many_match:t(0, 5, 7)),
    {[_, _, {11, 1}, {13, 4}],
        [ _, _, _, _
        , {{10, ID2, 1, [false], []}, 2}, {{10, ID2, 1, [true, false], []}, 1}
        , {{10, ID2, 1, [true, true, false], []}, 1}
        , {{10, ID2, 1, [true, true, true], []}, 1}
        , {{12, ID2, 2, [any, any, any], []}, 4}
        ]} =
        check(?FUNCTION_NAME, coverl:report([?FUNCTION_NAME], raw, #{})).

guard(_) ->
    guard:t([]),
    guard:t(<<1:1>>),
    guard:t(<<"asd">>),
    {[{6, 1}, {8, 2}, {10, 0}, {12, 0}],
        [ {{5, ID, 1, [any], [[false]]}, 2}, {{5, ID, 1, [any], [[true]]}, 1}
        , {{7, ID, 2, [any], [false, [true]]}, 1}
        , {{7, ID, 2, [any], [[false], [false]]}, 0}
        , {{7, ID, 2, [any], [[true]]}, 1}
        , {{9, ID, 3, [any], [false, [true, true]]}, 0}
        , {{9, ID, 3, [any], [[false], [false]]}, 0}
        , {{9, ID, 3, [any], [[false], [true, false]]}, 0}
        , {{9, ID, 3, [any], [[true, false], [false]]}, 0}
        , {{9, ID, 3, [any], [[true, false], [true, false]]}, 0}
        , {{9, ID, 3, [any], [[true, true]]}, 0}
        , {{11, ID, 4, [any], []}, 0}
        ]} =
        check(?FUNCTION_NAME, coverl:report([?FUNCTION_NAME], raw, #{})),
    %t(A) when is_integer(A), A > 10; is_float(A), A < 1.0 ->
    term = guard:t(10),
    num = guard:t(11),
    term = guard:t(1.1),
    num = guard:t(0.9),
    term = guard:t(atom),
    {[{6, 1}, {8, 2}, {10, 2}, {12, 3}],
        [ {{5, ID, 1, [any], [[false]]}, 7}, {{5, ID, 1, [any], [[true]]}, 1}
        , {{7, ID, 2, [any], [false, [true]]}, 1}
        , {{7, ID, 2, [any], [[false], [false]]}, 5}
        , {{7, ID, 2, [any], [[true]]}, 1}
        , {{9, ID, 3, [any], [false, [true, true]]}, 1}
        , {{9, ID, 3, [any], [[false], [false]]}, 1}
        , {{9, ID, 3, [any], [[false], [true, false]]}, 1}
        , {{9, ID, 3, [any], [[true, false], [false]]}, 1}
        , {{9, ID, 3, [any], [[true, false], [true, false]]}, 0}
        , {{9, ID, 3, [any], [[true, true]]}, 1}
        , {{11, ID, 4, [any], []}, 3}
        ]} =
        check(?FUNCTION_NAME, coverl:report([?FUNCTION_NAME], raw, #{})).

try_test(_) ->
    value2 = try_test:t(throw, false),
    {
        [ {6, 1}, {7, 0}, {8, 0}, {9, 0}, {10, 0}, {11, 0}, {12, 1}, {13, 0}
        , {14, 0} | _],
        [ {{5, _, 1, [any, any], []}, 1}
        , {{7, ID1, 1, [false], []}, 0}, {{7, ID1, 1, [true], []}, 0}
        , {{8, ID1, 2, [false], []}, 0}, {{8, ID1, 2, [true], []}, 0}
        , {{9, ID1, 3, [any], [[false]]}, 0}, {{9, ID1, 3, [any], [[true]]}, 0}
        , {{10, ID1, 4, [any], []}, 0}
        , {{11, ID2, 1, [false], []}, 1}, {{11, ID2, 1, [true, any], []}, 0}
        , {{12, ID2, 2, [false], []}, 0}, {{12, ID2, 2, [true, any], []}, 1}
        , {{13, ID2, 3, [any, any], [[false]]}, 0}
        , {{13, ID2, 3, [any, any], [[true]]}, 0}
        , {{14, ID2, 4, [any, any], []}, 0}
        | _]} =
        check(?FUNCTION_NAME, coverl:report([?FUNCTION_NAME], raw, #{})),
    ok = try_test:t(fun() -> ok end, false),
    error = try_test:t(fun() -> error end, false),
    false = try_test:t(fun() -> none end, false),
    true = try_test:t(fun() -> none end, true),
    {
        [ {6, 5}, {7, 1}, {8, 1}, {9, 1}, {10, 1}, {11, 0}, {12, 1}, {13, 0}
        , {14, 0} | _],
        [ {{5, _, 1, [any, any], []}, 5}
        , {{7, ID1, 1, [false], []}, 3}, {{7, ID1, 1, [true], []}, 1}
        , {{8, ID1, 2, [false], []}, 2}, {{8, ID1, 2, [true], []}, 1}
        , {{9, ID1, 3, [any], [[false]]}, 1}, {{9, ID1, 3, [any], [[true]]}, 1}
        , {{10, ID1, 4, [any], []}, 1}
        , {{11, ID2, 1, [false], []}, 1}, {{11, ID2, 1, [true, any], []}, 0}
        , {{12, ID2, 2, [false], []}, 0}, {{12, ID2, 2, [true, any], []}, 1}
        , {{13, ID2, 3, [any, any], [[false]]}, 0}
        , {{13, ID2, 3, [any, any], [[true]]}, 0}
        , {{14, ID2, 4, [any, any], []}, 0}
        | _]} =
        check(?FUNCTION_NAME, coverl:report([?FUNCTION_NAME], raw, #{})),
    value1 = try_test:t(fun() -> throw(test) end, false),
    value2 = try_test:t(fun() -> error(test) end, false),
    value4 = try_test:t(fun() -> exit(test) end, false),
    value3 = try_test:t(fun() -> exit(test) end, true),
    {
        [ {6, 9}, {7, 1}, {8, 1}, {9, 1}, {10, 1}, {11, 1}, {12, 2}, {13, 1}
        , {14, 1} | _],
        [ {{5, _, 1, [any, any], []}, 9}
        , {{7, ID1, 1, [false], []}, 3}, {{7, ID1, 1, [true], []}, 1}
        , {{8, ID1, 2, [false], []}, 2}, {{8, ID1, 2, [true], []}, 1}
        , {{9, ID1, 3, [any], [[false]]}, 1}, {{9, ID1, 3, [any], [[true]]}, 1}
        , {{10, ID1, 4, [any], []}, 1}
        , {{11, ID2, 1, [false], []}, 4}, {{11, ID2, 1, [true, any], []}, 1}
        , {{12, ID2, 2, [false], []}, 2}, {{12, ID2, 2, [true, any], []}, 2}
        , {{13, ID2, 3, [any, any], [[false]]}, 1}
        , {{13, ID2, 3, [any, any], [[true]]}, 1}
        , {{14, ID2, 4, [any, any], []}, 1}
        | _]} =
        check(?FUNCTION_NAME, coverl:report([?FUNCTION_NAME], raw, #{})),
    value1 = try_test:t1(fun() -> throw(test) end),
    test = try_test:t1(fun() -> test end),
    {
        [ _, _, _, _, _, _, _, _, _, {18, 2}, {19, 1}, {20, 2}],
        [ _, _, _, _, _, _, _, _, _, _, _, _, _, _, _
        , {{17, _, 1, [any], []}, 2}
        , {{19, _, 1, [true, any], []}, 1}
        ]} =
        check(?FUNCTION_NAME, coverl:report([?FUNCTION_NAME], raw, #{})).

if_test(_) ->
    ok = if_test:t(false, true),
    ok = if_test:t(true, false),
    ok1 = if_test:t(false, false),
    {[{6, 2}, {7, 1}],
        [ {{5, _, 1, [any, any], []}, 3}
        , {{6, ID, 1, [], [false, [true]]}, 1}
        , {{6, ID, 1, [], [[false], [false]]}, 1}
        , {{6, ID, 1, [], [[true]]}, 1}
        , {{7, ID, 2, [], [[true]]}, 1}
        ]} =
        check(?FUNCTION_NAME, coverl:report([?FUNCTION_NAME], raw, #{})).

case_test(_) ->
    ok = case_test:t(1, 0),
    ok1 = case_test:t(1, 1),
    ok2 = case_test:t(1, 2),
    {[{6, 3}, {7, 1}, {8, 1}, {9, 1} | _],
        [ {{5, _, 1, [any, any], []}, 3}
        , {{7, ID1, 1, [false], []}, 2}
        , {{7, ID1, 1, [true], []}, 1}
        , {{8, ID1, 2, [false], []}, 1}
        , {{8, ID1, 2, [true], []}, 1}
        , {{9, ID1, 3, [any], []}, 1}
        | _]} =
        check(?FUNCTION_NAME, coverl:report([?FUNCTION_NAME], raw, #{})),
    case_test:case_block_line(),
    {[_, _, _, _, {13, 2}, {14, 0}, {15, 1} | _],
        [ _, _, _, _, _, _
        , {{12, _, 1, [], []}, 1}
        , {{14, ID2, 1, [false], []}, 1}
        , {{14, ID2, 1, [true], []}, 0}
        , {{15, ID2, 2, [true], []}, 1}
        | _]} =
        check(?FUNCTION_NAME, coverl:report([?FUNCTION_NAME], raw, #{})),
    case_test:case_block(),
    {[_, _, _, _, _, _, _, {21, 1}, {22, 1}, {24, 0}, {25, 1}],
        [ _, _, _, _, _, _, _, _, _, _
        , {{18, _, 1, [], []}, 1}
        , {{24, ID3, 1, [false], []}, 1}
        , {{24, ID3, 1, [true], []}, 0}
        , {{25, ID3, 2, [true], []}, 1}
        ]} =
        check(?FUNCTION_NAME, coverl:report([?FUNCTION_NAME], raw, #{})).

data(Config) ->
    {_, PrivDir} = lists:keyfind(priv_dir, 1, Config),
    File = filename:join(PrivDir, "test.data"),
    data:t(),
    {[{Line, 1}], [{{CDLine, ID, CDNum, [], []}, 1}]} =
        check(?FUNCTION_NAME, coverl:report([?FUNCTION_NAME], raw, #{})),
    coverl:export(File),
    coverl:reset(?FUNCTION_NAME),
    {[{Line, 0}], [{{CDLine, ID, CDNum, [], []}, 0}]} =
        check(?FUNCTION_NAME, coverl:report([?FUNCTION_NAME], raw, #{})),
    data:t(),
    coverl:reset(),
    {[{Line, 0}], [{{CDLine, ID, CDNum, [], []}, 0}]} =
        check(?FUNCTION_NAME, coverl:report([?FUNCTION_NAME], raw, #{})),
    coverl:import(File),
    {[{Line, 1}], [{{CDLine, ID, CDNum, [], []}, 1}]} =
        check(?FUNCTION_NAME, coverl:report([?FUNCTION_NAME], raw, #{})),
    ok.
