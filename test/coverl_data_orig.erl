-module(coverl_data_orig).

-type spec() :: {[boolean() | any], [[boolean()] | false]}.
-type hit()
    :: {{atom(), number(), line}, non_neg_integer()}
    | {{atom(), number(), file}, {file:name(), integer()}}
    | {{atom(), number(), {cd, number(), number()}}, [spec()]}
    | {{atom(), number(), {cd, number(), number(), spec()}}, integer()}.

-define(HIT_TAB, '$coverl_hit').

-export([cd_hit/4]).
-export([export/2]).
-export([set_empty/1]).
-export([get_all_hits/0]).
-export([get_all_module/0]).
-export([import/1]).
-export([init/0]).
-export([line_hit/3]).
-export([reset/0]).
-export([reset/1]).

-spec init() -> ok.
init() ->
    ?HIT_TAB = ets:new(?HIT_TAB,
        [named_table, public, ordered_set, {write_concurrency, true}]),
    ok.

-spec reset() -> ok.
reset() ->
    ModuleList = get_all_module(),
    true = ets:delete_all_objects(?HIT_TAB),
    set_empty(ModuleList),
    ok.

-spec reset(module()) -> ok.
reset(Module) ->
    true = ets:match_delete(?HIT_TAB, {{Module, '_', '_'}, '_'}),
    set_empty([Module]),
    ok.

-spec line_hit(atom(), file:name(), pos_integer()) -> ok.
line_hit(Module, File, Line) ->
    Key = {Module, File, Line, line},
    _ = ets:update_counter(?HIT_TAB, Key, 1, {key, 0}),
    ok.

-spec cd_hit(
    module(), pos_integer(), [erl_parse:abstract_expr()],
    [[erl_parse:abstract_expr()]])
-> ok.
cd_hit(Module, ID, ArgList, GuardArgList) ->
    ClauseSpecList = Module:'$match_spec'(ID),
    clause_match(Module, ID, 1, ClauseSpecList, ArgList, GuardArgList).

clause_match(
    Module, ID, ClauseNum,
    [{File, Line, ArgSpecList, GuardSpecList} | ClauseSpecTail],
    ArgList, GuardArgList)
->
    {IsMatch, ArgMatch, NextBinding} =
        match_arg(ArgList, ArgSpecList, erl_eval:new_bindings(), []),
    GuardBinding =
        lists:foldl(
            fun({Name, Value}, B) -> erl_eval:add_binding(Name, Value, B) end,
            NextBinding, GuardArgList),
    {IsGuardMatch, GuardMatch} =
        match_guard(IsMatch, GuardSpecList, [], GuardBinding),
    CDHit = {cd, ID, ClauseNum, ArgMatch, GuardMatch},
    ets:update_counter(?HIT_TAB, {Module, File, Line, CDHit}, 1, {key, 0}),
    IsMatch andalso IsGuardMatch orelse
        clause_match(Module, ID, ClauseNum + 1, ClauseSpecTail,
            ArgList, GuardArgList);
clause_match(_, _, _, [], _, _) ->
    ok.

match_arg([_ | ArgTail], [{var, _, '_'} | ArgSpecTail], Binding, Acc) ->
    match_arg(ArgTail, ArgSpecTail, Binding, [any | Acc]);
match_arg([Arg | ArgTail], [{var, _, Name} | ArgSpecTail], Binding, Acc) ->
    NextBinding = erl_eval:add_binding(Name, Arg, Binding),
    match_arg(ArgTail, ArgSpecTail, NextBinding, [any | Acc]);
match_arg([Arg | ArgTail], [ArgSpec | ArgSpecTail], Binding, Acc) ->
    case match_arg(Arg, ArgSpec, Binding)
    of {true, NextBinding} ->
        match_arg(ArgTail, ArgSpecTail, NextBinding, [true | Acc])
    ; false ->
        {false, lists:reverse([false | Acc]), Binding}
    end;
match_arg([], [], Binding, Acc) ->
    {true, lists:reverse(Acc), Binding}.

match_arg(Arg, ArgSpec, Binding) ->
    TempBinding = erl_eval:add_binding('__ARG', Arg, Binding),
    try
        FakeLine = erl_anno:new(0),
        MatchArg = {match, FakeLine, ArgSpec, {var, FakeLine, '__ARG'}},
        {value, _, NB} = erl_eval:expr(MatchArg, TempBinding),
        {true, NB}
    catch _:_ ->
        false
    end.

match_guard(true, [], [], _) ->
    {true, []};
match_guard(true, [], Acc, _) ->
    {false, lists:reverse(Acc)};
match_guard(true, [AndGuardList | GuardSpecTail], Acc, Binding) ->
    AndGuardMatch = match_guard(AndGuardList, Binding),
    case is_guard_true(AndGuardMatch)
    of true ->
        AllAny = [false || _ <- Acc],
        {true, lists:reverse([AndGuardMatch | AllAny])}
    ; false ->
        match_guard(true, GuardSpecTail, [AndGuardMatch | Acc], Binding)
    end;
match_guard(false, _, _, _) ->
    {false, []}.

match_guard([GuardSpec | GuardSpecTail], Binding) ->
    try erl_eval:expr(GuardSpec, Binding)
    of {value, true, _} ->
        [true | match_guard(GuardSpecTail, Binding)]
    ; _ ->
        [false]
    catch _:_ ->
        [false]
    end;
match_guard([], _) ->
    [].

is_guard_true([true | AndGuardTail]) ->
    is_guard_true(AndGuardTail);
is_guard_true([false | _]) ->
    false;
is_guard_true([]) ->
    true.

-spec export(file:name(), [module()] | all) -> ok | {error, term()}.
export(FileName, ModuleList) ->
    case dets:open_file(make_ref(), [{file, FileName}])
    of {ok, DetsName} ->
        export_to_dets(DetsName, ModuleList)
    ; Error ->
        Error
    end.

export_to_dets(DetsName, all) ->
    ok = dets:from_ets(DetsName, ?HIT_TAB),
    ok = dets:close(DetsName);
export_to_dets(DetsName, ModuleList) ->
    Data = ets:match_object(?HIT_TAB, '_', 100),
    traverse_ets(Data, DetsName, ModuleList).

traverse_ets('$end_of_table', DetsName, _) ->
    ok = dets:close(DetsName);
traverse_ets({ElementList, Continuation}, DetsName, ModuleList) ->
    ToDets =
        [ Element
        || {Key, _} = Element <- ElementList
        , lists:member(element(1, Key), ModuleList)
        ],
    ok = dets:insert(DetsName, ToDets),
    traverse_ets(ets:match(Continuation), DetsName, ModuleList).

-spec import(file:name()) -> ok.
import(FileName) ->
    Name = make_ref(),
    {ok, Name} = dets:open_file(Name, [{file, FileName}]),
    dets:traverse(Name, fun update_counter/1),
    ok = dets:close(Name),
    ok.

update_counter({Key, Value}) ->
    _ = ets:update_counter(?HIT_TAB, Key, Value, {key, 0}),
    continue.

-spec get_all_hits() -> [hit()].
get_all_hits() ->
    AllObject = ets:match_object(?HIT_TAB, '_'),
    lists:sort(AllObject).

-spec get_all_module() -> [hit()].
get_all_module() ->
    ModuleList = ets:match(?HIT_TAB, {{'$1', '_', '_', '_'}, '_'}),
    lists:usort([Module || [Module] <- ModuleList]).

-spec set_empty(list(atom())) -> ok.
set_empty(ModuleList) ->
    ets:insert(?HIT_TAB,
        [ {{Module, File, Line, line}, 0}
        || Module <- ModuleList, {File, Line} <- Module:'$line_info'()]),
    [ cd_spec(Module, ID, 1, Module:'$match_spec'(ID))
    || Module <- ModuleList, ID <- Module:'$cd_id_list'()],
    ok.

cd_spec(Module, CDID, ClauseNum, [{File, Line, ArgSpecList, []}]) ->
    TrueArgCD = true_arg_cd(ArgSpecList),
    Key = {Module, File, Line, {cd, CDID, ClauseNum, TrueArgCD, []}},
    ets:insert(?HIT_TAB, {Key, 0});
cd_spec(Module, CDID, ClauseNum, [{File, Line, ArgSpec, GuardSpec}]) ->
    TrueArgCD = true_arg_cd(ArgSpec),
    ets:insert(?HIT_TAB,
        [ {{Module, File, Line, {cd, CDID, ClauseNum, TrueArgCD, GuardCD}}, 0}
        || GuardCD <- true_guard_cd(GuardSpec, [])
        ]);
cd_spec(Module, CDID, ClauseNum,
    [{File, Line, ArgSpec, GuardSpec} | ClauseSpecTail])
->
    TrueArgCD = true_arg_cd(ArgSpec),
    ets:insert(?HIT_TAB,
        [ {{Module, File, Line, {cd, CDID, ClauseNum, TrueArgCD, GuardCD}}, 0}
        || GuardCD <- need_guard_cd(GuardSpec)
        ]),
    ets:insert(?HIT_TAB,
        [ {{Module, File, Line, {cd, CDID, ClauseNum, FalseMatch, []}}, 0}
        || FalseMatch <- all_false_cd(ArgSpec, [])
        ]),
    cd_spec(Module, CDID, ClauseNum + 1, ClauseSpecTail).

need_guard_cd([]) ->
    [[]];
need_guard_cd(GuardSpec) ->
    TrueGuardCD = true_guard_cd(GuardSpec, []),
    FalseGuardCD = false_guard_cd(GuardSpec),
    lists:append(TrueGuardCD, FalseGuardCD).

true_guard_cd([AndGuardList | OrGuardList], Acc) ->
    [lists:reverse([[true || _ <- AndGuardList] | Acc])
    | true_guard_cd(OrGuardList, [false | Acc])];
true_guard_cd([], _) ->
    [].

false_guard_cd([AndGuardList | OrGuardTail]) ->
    FalseGuardCD = false_guard_cd(OrGuardTail),
    [ [AndFalseGuard | TailGuard]
    || AndFalseGuard <- all_false_cd(AndGuardList, [])
    , TailGuard <- FalseGuardCD
    ];
false_guard_cd([]) ->
    [[]].

all_false_cd([], _) ->
    [];
all_false_cd([{var, _, _}], _) ->
    [];
all_false_cd([_], Acc) ->
    [lists:reverse([false | Acc])];
all_false_cd([{var, _, _} | ArgMatchTail], Acc) ->
    all_false_cd(ArgMatchTail, [any | Acc]);
all_false_cd([_ | ArgMatchTail], Acc) ->
    [lists:reverse([false | Acc]) | all_false_cd(ArgMatchTail, [true | Acc])].

true_arg_cd([{var, _, _} | ArgSpecTail]) ->
    [any | true_arg_cd(ArgSpecTail)];
true_arg_cd([_ | ArgSpecTail]) ->
    [true | true_arg_cd(ArgSpecTail)];
true_arg_cd([]) ->
    [].
