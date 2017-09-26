-module(coverl).

-export([compile_beam_directory/1]).
-export([compile_module/1]).
-export([export/1]).
-export([export/2]).
-export([import/1]).
-export([report/0]).
-export([report/1]).
-export([report/2]).
-export([report/3]).
-export([reset/0]).
-export([reset/1]).

-spec export(file:name()) -> ok.
export(FileName) ->
    coverl_data:export(FileName, coverl_data:get_all_module()).

-spec export(file:name(), [module()]) -> ok.
export(FileName, ModuleList) ->
    ok = filelib:ensure_dir(FileName),
    coverl_data:export(FileName, ModuleList).

-spec import(file:name()) -> ok.
import(FileName) ->
    coverl_data:import(FileName).

-spec report() -> ok.
report() ->
    report(coverl_data:get_all_module(), coverl_lcov, #{}).

-spec report([module()] | map()) -> ok.
report(Opt) when is_map(Opt) ->
    report(coverl_data:get_all_module(), coverl_lcov, Opt);
report(ModuleList) ->
    report(ModuleList, coverl_lcov, #{}).

-spec report([module()], map()) -> ok.
report(ModuleList, Opt) ->
    report(ModuleList, coverl_lcov, Opt).

-spec report([module()], module(), map()) -> ok.
report(ModuleList, Module, Opt) ->
    InfoList = coverl_data:get_all_hits(),
    Data = group_data(InfoList, #{}),
    FilterOutModuleData = maps:with(ModuleList, Data),
    Module:output(FilterOutModuleData, Opt).

group_data([{{Module, _, _, _}, _} = Hit | HitListTail], Data) ->
    group_by_file(Module, HitListTail, add_hit(Hit, #{}), Data);
group_data([], Data) ->
    Data.

add_hit({{_, File, Line, line}, Add}, Data) ->
    FileInfo = maps:get(File, Data, #{}),
    LineInfo = maps:get(line, FileInfo, #{}),
    Count = maps:get(Line, LineInfo, 0),
    Data#{File => FileInfo#{line => LineInfo#{Line => Count + Add}}};
add_hit({{_, File, Line, {cd, ID, Num, Arg, Guard}}, Add}, Data) ->
    BranchID = {Line, ID, Num, Arg, Guard},
    FileInfo = maps:get(File, Data, #{}),
    BranchInfo = maps:get(branch, FileInfo, #{}),
    Count = maps:get(BranchID, BranchInfo, 0),
    Data#{File => FileInfo#{branch => BranchInfo#{BranchID => Count + Add}}}.

group_by_file(Module,
    [{{Module, _, _, _}, _} = Hit | HitTail], ModuleData, Data)
->
    group_by_file(Module, HitTail, add_hit(Hit, ModuleData), Data);
group_by_file(Module, HitList, ModuleData, Data) ->
    group_data(HitList, Data#{Module => ModuleData}).

-spec reset() -> ok.
reset() ->
    coverl_data:reset().

-spec reset(module()) -> ok.
reset(Module) ->
    coverl_data:reset(Module).

-spec compile_beam_directory(file:name()) ->
    [{module(), ok | term()}] | {error, term()}.
compile_beam_directory(Dir) ->
    case file:list_dir(Dir)
    of {ok, Files} ->
        ModBeamList =
            [ get_mod_and_beam(filename:join(Dir, File))
            || File <- Files, filename:extension(File) =:= ".beam"],
        compile_beams(ModBeamList, #{})
    ; Error ->
        Error
    end.

get_mod_and_beam(File) ->
    AbsFile = filename:absname(File),
    Module = list_to_atom(filename:basename(File, ".beam")),
    {Module, AbsFile}.

compile_beams([{Module, AbsFile} | ModBeamTail], ModuleMap) ->
    case maps:get(Module, ModuleMap, none)
    of none ->
        compile_beams(ModBeamTail, ModuleMap#{Module => AbsFile})
    ; AbsFile ->
        compile_beams(ModBeamTail, ModuleMap)
    ; _ ->
        {error, {duplicate, Module}}
    end;
compile_beams([], ModuleMap) ->
    do_compile(maps:to_list(ModuleMap)).

do_compile([{Module, BeamFile} | CompileList]) ->
    case beam_lib:chunks(BeamFile, [abstract_code, compile_info])
    of {ok, {Module,
            [{abstract_code, AbstractCode}, {compile_info, CompileInfo}]}}
    ->
        BeamOptions = get_compile_options(CompileInfo),
        CoverOptions = [{parse_transform, coverl_instrument} | BeamOptions],
        [ do_compile_abscode(Module, AbstractCode, CoverOptions)
        | do_compile(CompileList)]
    ; {error, beam_lib, {key_missing_or_invalid, _, _}} ->
        {error, encrypted_abstract_code}
    ; Error ->
        Error
    end;
do_compile([]) ->
    [].

do_compile_abscode(Module, no_abstract_code, _) ->
    {Module, no_abstract_code};
do_compile_abscode(Module, {raw_abstract_v1, AbstractCode}, Options) ->
    case compile:forms(AbstractCode, [return | Options])
    of {ok, Module, Binary, []} ->
        case code:load_binary(Module, cover_compiled, Binary)
        of {module, Module} ->
            coverl_data:set_empty([Module]),
            {Module, ok}
        ; Error ->
            {Module, Error}
        end
    ; Error ->
        {Module, Error}
    end.

get_compile_options(CompileInfo) ->
    case lists:keyfind(options, 1, CompileInfo)
    of {options, Options } ->
        filter_options(Options)
    ; false ->
        []
    end.

filter_options([{i, Dir} | OptionList]) ->
    [{i, Dir} | filter_options(OptionList)];
filter_options([{d, Macro} | OptionList]) ->
    [{d, Macro} | filter_options(OptionList)];
filter_options([{d, Macro, Value} | OptionList]) ->
    [{d, Macro, Value} | filter_options(OptionList)];
filter_options([export_all | OptionList]) ->
    [export_all | filter_options(OptionList)];
filter_options([debug_info | OptionList]) ->
    [debug_info | filter_options(OptionList)];
filter_options([_ | OptionList]) ->
    filter_options(OptionList);
filter_options([]) ->
    [].

-spec compile_module(module()) ->
    [{module(), ok | term()}] | {error, term()}.
compile_module(Module) ->
    case code:which(Module)
    of cover_compiled -> [{Module, ok}]
    ; preloaded -> {error, preloaded}
    ; non_existing -> {error, non_existing}
    ; File -> compile_beams([{Module, File}], #{})
    end.
