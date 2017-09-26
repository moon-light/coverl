-module(coverl_data_test).

-include_lib("eunit/include/eunit.hrl").

one_false_arg_test_() ->
    Var = {var, 0, test},
    [ ?_assertEqual([], coverl_data:all_false_cd([Var], []))
    , ?_assertEqual([], coverl_data:all_false_cd([Var, Var], []))
    , ?_assertEqual([[false]], coverl_data:all_false_cd([true], []))
    , ?_assertEqual(
        [[false], [true, false]],
        coverl_data:all_false_cd([true, true], []))
    , ?_assertEqual([[any, false]], coverl_data:all_false_cd([Var, true], []))
    , ?_assertEqual([[false]], coverl_data:all_false_cd([true, Var], []))
    , ?_assertEqual(
        [[any, false]], coverl_data:all_false_cd([Var, true, Var], []))
    , ?_assertEqual(
        [[false], [true, false], [true, true, false]],
        coverl_data:all_false_cd([true, true, true], []))
    ].

true_arg_cd_test_() ->
    Var = {var, 0, test},
    [ ?_assertEqual([any], coverl_data:true_arg_cd([Var]))
    , ?_assertEqual([any, any], coverl_data:true_arg_cd([Var, Var]))
    , ?_assertEqual([true], coverl_data:true_arg_cd([true]))
    , ?_assertEqual(
        [true, true], coverl_data:true_arg_cd([true, true]))
    , ?_assertEqual([any, true], coverl_data:true_arg_cd([Var, true]))
    , ?_assertEqual([true, any], coverl_data:true_arg_cd([true, Var]))
    , ?_assertEqual(
        [any, true, any], coverl_data:true_arg_cd([Var, true, Var]))
    , ?_assertEqual(
        [true, true, true], coverl_data:true_arg_cd([true, true, true]))
    ].

need_guard_cd_test_() ->
    Var = {var, 0, test},
    [ ?_assertEqual([[]], coverl_data:need_guard_cd([]))
    , ?_assertEqual([[[true]], [[false]]], coverl_data:need_guard_cd([[Var]]))
    , ?_assertEqual([[[true, true]], [[false]], [[true, false]]],
        coverl_data:need_guard_cd([[Var, Var]]))
    , ?_assertEqual(
        % True or _ = True
        [ [[true]]
        % False or True and True = True
        , [false, [true, true]]
        % False or A and B = False
        , [[false], [false]], [[false], [true, false]]
        ],
        coverl_data:need_guard_cd([[Var], [Var, Var]]))
    , ?_assertEqual(
        % True and True or _ = True
        [ [[true, true]]
        % _ and _ or True and True = True
        , [false, [true, true]]
        % F and - or F and - = False
        , [[false], [false]]
        % F and - or T and F = False
        , [[false], [true, false]]
        % T and F or F and - = False
        , [[true, false], [false]]
        % T and F or T and F = False
        , [[true, false], [true, false]]
        ],
        coverl_data:need_guard_cd([[Var, Var], [Var, Var]]))
    ].

% TODO test last clause has only true branch
