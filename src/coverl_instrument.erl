-module(coverl_instrument).

-export([parse_transform/2]).

parse_transform(FormList, OptionList) ->
    process_form(FormList, init_state(OptionList, state())).

state() ->
    #{module => unknown, declare => #{}, export => false
    , line => [], cd_id_list => []
    , data_module => coverl_data}.

init_state([{d, 'COVERL_DATA_MODULE', DataModule} | OptionList], State) ->
    init_state(OptionList, State#{data_module => DataModule});
init_state([_ | OptionList], State) ->
    init_state(OptionList, State);
init_state([], State) ->
    State.

process_form(
    [{function, Line, FunctionName, Arity, ClauseList} | FormTail],
    #{cd_id_list := CDIDList} = State)
->
    CDID = cd_id(),
    S = State#{cd_id => CDID, cd_id_list => [CDID | CDIDList]},
    {ProcessedClauseList, S1} =
        lists:mapfoldl(fun process_clause/2, S, ClauseList),
    [ {function, Line, FunctionName, Arity, ProcessedClauseList}
    | process_form(FormTail, S1)];
process_form([{attribute, Line, export, ExportFunction} = Form | FormTail],
    #{export := false} = State)
->
    case lists:member({'$match_spec', 1}, ExportFunction)
    of true ->
        [Form | FormTail]
    ; false ->
        [ {attribute, Line, export,
            [{'$match_spec', 1}, {'$line_info', 0}, {'$cd_id_list', 0}]}
        , Form
        | process_form(FormTail, State#{export => true})]
    end;
process_form([{attribute, _, module, Module} = Form | FormTail], State) ->
    [Form | process_form(FormTail, State#{module => Module})];
process_form(
    [{attribute, Anno, file, {FileName, FileLine}} = Form | FormTail], State)
->
    Diff = case erl_anno:generated(Anno)
    of true ->
        FileLine - erl_anno:location(Anno)
    ; false ->
        0
    end,
    [Form | process_form(FormTail, State#{file => {FileName, Diff}})];
process_form([Form | FormTail], State) ->
    [Form | process_form(FormTail, State)];
process_form([],
    #{declare := Declare, line := LineList, cd_id_list := CDIDList})
->
    [ create_declare(maps:to_list(Declare))
    , line_info(LineList), cd_id_list(CDIDList)].

cd_id() ->
    erlang:unique_integer([positive, monotonic]).

process_clause({clause, Line, ArgList, GuardList, ExprList},
    #{cd_id := ID} = State)
->
    {NewArgList, ArgMatchList} = lists:unzip(match_arg(ID, 0, ArgList)),
    ArgVarList = [ArgVar || Arg <- ArgList, ArgVar <- get_all_var(Arg)],
    GuardVarList =
        [GuardVar || OrGuardList <- GuardList, Guard <- OrGuardList
            , GuardVar <- get_all_var(Guard)
            , not lists:member(GuardVar, ArgVarList)
        ],
    CDHitCall = cd_hit(Line, ArgMatchList, GuardVarList, State),
    S = add_declare(Line, ArgList, GuardList, State),
    {InstrementedExprList, S1} = process_expr_list(ExprList, S),
    NewClauseList = [CDHitCall | InstrementedExprList],
    {{clause, Line, NewArgList, GuardList, NewClauseList}, S1}.

add_declare(Line, ArgList, GuardList,
    #{cd_id := ID, declare := Declare, file := {File, Diff}} = State)
->
    LineList = maps:get(ID, Declare, []),
    NewLineList = [{File, Line + Diff, ArgList, GuardList} | LineList],
    State#{declare => maps:put(ID, NewLineList, Declare)}.

line_hit([Expr | ExprTail], State) when
    element(1, Expr) == 'try';
    element(1, Expr) == 'case';
    element(1, Expr) == 'if';
    element(1, Expr) == 'block'
->
    [Expr | line_hit(ExprTail, State)];
line_hit([Expr | ExprTail],
    #{module := Module, data_module := DataModule
    , file := {File, Diff}} = State)
->
    Line = element(2, Expr),
    [ {call, Line,
        {remote, Line, {atom, Line, DataModule}, {atom, Line, line_hit}},
        [ {atom, Line, Module}, {string, Line, File}
        , {integer, Line, Line + Diff}
        ]}
    , Expr
    | line_hit(ExprTail, State)];
line_hit([], _) ->
    [].

cd_hit(Line, ArgMatchList, GuardVarList,
    #{module := Module, cd_id := ID, data_module := DataModule})
->
    {call, Line,
        {remote, Line, {atom, Line, DataModule}, {atom, Line, cd_hit}},
        [{atom, Line, Module}, {integer, Line, ID}, to_list(ArgMatchList)
        , to_list(GuardVarList)
        ]}.

process_expr_list(ExprList, State) ->
    {PExprList, S} = lists:mapfoldl(fun process_expr/2, State, ExprList),
    {line_hit(PExprList, S), S}.

process_expr({'case', Line, Expr, ClauseList}, #{cd_id := ID} = State) ->
    {ProcessedExpr, NextState} = process_expr(Expr, State),
    {ProcessedClauseList, S} =
        process_cd(fun process_clause/2, NextState, ClauseList),
    PE = line_hit([ProcessedExpr], S),
    {{'case', Line, {block, Line, PE}, ProcessedClauseList}, S#{cd_id => ID}};
process_expr({'if', Line, ClauseList}, #{cd_id := ID} = State) ->
    {ProcessedClauseList, S} =
        process_cd(fun process_clause/2, State, ClauseList),
    {{'if', Line, ProcessedClauseList}, S#{cd_id => ID}};
process_expr(
    {'try', Line, TryExprList, TryClauseList, CatchClauseList, AfterExprList},
    #{cd_id := ID} = State)
->
    {InstrTryExprList, S} = process_expr_list(TryExprList, State),
    {ProcessedTryClauseList, S1} =
        process_cd(fun process_clause/2, S, TryClauseList),
    {ProcessedCatchClauseList, S2} =
        process_cd(fun process_try_clause/2, S1, CatchClauseList),
    {InstAfterExprList, S3} = process_expr_list(AfterExprList, S2),
    ProcessedTry =
        {'try', Line, InstrTryExprList, ProcessedTryClauseList,
        ProcessedCatchClauseList, InstAfterExprList},
    {ProcessedTry, S3#{cd_id => ID}};
% TODO: receive _ end
process_expr({block, Line, ExprList}, State) ->
    {InstrExprList, S1} = process_expr_list(ExprList, State),
    {{block, Line, InstrExprList}, S1};
% TODO: nested expr
% TODO: [ _ || E <- List]
% TODO: << _ || E <= List >>
% TODO: fun() -> ... end
process_expr(Expr, State) ->
    {Expr, add_line(element(2, Expr), State)}.

process_cd(_, State, []) ->
    {[], State};
process_cd(ProcessFun, State, ClauseList) ->
    lists:mapfoldl(ProcessFun, add_cd_id(cd_id(), State), ClauseList).

process_try_clause({clause, Line, CatchMatch, GuardList, ExprList},
    #{cd_id := ID} = State)
->
    [{tuple, Line1, [Type, Reason, Unknown]}] = CatchMatch,
    ArgList = [Type, Reason],
    {NewArgList, ArgMatchList} = lists:unzip(match_arg(ID, 0, ArgList)),
    ArgVarList = [ArgVar || Arg <- ArgList, ArgVar <- get_all_var(Arg)],
    GuardVarList =
        [GuardVar || OrGuardList <- GuardList, Guard <- OrGuardList
            , GuardVar <- get_all_var(Guard)
            , not lists:member(GuardVar, ArgVarList)
        ],
    CDHitCall = cd_hit(Line, ArgMatchList, GuardVarList, State),
    S = add_declare(Line, ArgList, GuardList, State),
    {ProcessedExprList, S1} = lists:mapfoldl(fun process_expr/2, S, ExprList),
    InstrementedExprList = line_hit(ProcessedExprList, S1),
    NewClauseList = [CDHitCall | InstrementedExprList],
    NewCatchMatch = [{tuple, Line1, NewArgList ++ [Unknown]}],
    {{clause, Line, NewCatchMatch, GuardList, NewClauseList}, S1}.

add_line(Line, #{line := LineList, file := {File, Diff}} = State) ->
    State#{line => [{File, Line + Diff} | LineList]}.

add_cd_id(ID, #{cd_id_list := CDIDList} = State) ->
    State#{cd_id => ID, cd_id_list => [ID | CDIDList]}.

match_arg(ID, N, [{match, _, {var, Line, VarName}, _} = Arg | ArgTail]) ->
    [{Arg, {var, Line, VarName}} | match_arg(ID, N + 1, ArgTail)];
match_arg(ID, N, [{match, _, _, {var, Line, VarName}} = Arg | ArgTail]) ->
    [{Arg, {var, Line, VarName}} | match_arg(ID, N + 1, ArgTail)];
match_arg(ID, N, [{var, _, Name} = Arg | ArgTail]) when Name =/= '_' ->
    [{Arg, Arg} | match_arg(ID, N + 1, ArgTail)];
match_arg(ID, N, [Arg | ArgTail]) ->
    VarName = list_to_atom(
        "Arg_" ++ integer_to_list(ID) ++ "_" ++ integer_to_list(N)),
    Var = {var, 0, VarName},
    [{{match, 0, Arg, Var}, Var} | match_arg(ID, N + 1, ArgTail)];
match_arg(_, _, []) ->
    [].

get_all_var({var, _, '_'}) ->
    [];
get_all_var({var, Line, Name}) ->
    [{tuple, 0, [{atom, 0, Name}, {var, Line, Name}]}];
get_all_var({op, _, _, Op1, Op2}) ->
    get_all_var(Op1) ++ get_all_var(Op2);
get_all_var({call, _, _, ArgList}) ->
    [ArgVar || Arg <- ArgList, ArgVar <- get_all_var(Arg)];
get_all_var(_F) ->
    [].

to_list([Arg | ArgTail]) ->
    {cons, 0, Arg, to_list(ArgTail)};
to_list([]) ->
    {nil, 0}.

create_declare(DeclareList) ->
    {function, 0, '$match_spec', 1, process_declare(DeclareList)}.

process_declare([{ID, DeclareLine} | DeclareTail]) ->
    Declare =
        [ erl_parse:abstract({File, Line, ArgList, GuardList})
        || {File, Line, ArgList, GuardList} <- lists:reverse(DeclareLine)],
    [ {clause, 0, [{integer, 0, ID}], [], [to_list(Declare)]}
    | process_declare(DeclareTail)];
process_declare([]) ->
    [].

line_info(LineList) ->
    {function, 0, '$line_info', 0,
        [{clause, 0, [], [], [erl_parse:abstract(LineList)]}]}.

cd_id_list(CDIDList) ->
    {function, 0, '$cd_id_list', 0,
        [{clause, 0, [], [], [to_list([{integer, 0, L} || L <- CDIDList])]}]}.
