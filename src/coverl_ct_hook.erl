-module(coverl_ct_hook).

-export([id/1]).
-export([init/2]).
-export([terminate/1]).

id(_) ->
    coverl.

init(_, OptionList) ->
    State = configure(OptionList, #{}),
    #{app := App} = State,
    {ok, _} = application:ensure_all_started(coverl),
    case code:lib_dir(App)
    of {error, bad_name} ->
        ct:log("coverl_ct_hook: unknown app ~p~n", [App])
    ; AppDir ->
        AppBeamDir = filename:join([AppDir, "ebin"]),
        case [{Module, Error}
            || {Module, Error} <- coverl:compile_beam_directory(AppBeamDir),
                Error =/= ok]
        of [] -> ok
        ; Error ->
            ct:log("coverl_compile:~n~p~n", [Error])
        end
    end,
    {ok, State}.

configure([{export, File} | OptionList], State) ->
    configure(OptionList, State#{export => File});
configure([{cover, {application, App}} | OptionList], State) ->
    configure(OptionList, State#{app => App});
configure([], State) ->
    State.

terminate(State) ->
    export(State),
    ok.

export(#{app := App, export := File}) ->
    {ok, ModuleList} = application:get_key(App, modules),
    coverl:export(File, ModuleList);
export(_) ->
    ok.
