-module(coverl_app).

-behavior(application).
-behavior(supervisor).

-export([init/1]).
-export([start/2]).
-export([stop/1]).

start(_, _) ->
    supervisor:start_link(?MODULE, top).

stop(_) ->
    ok.

init(top) ->
    MFA = {coverl_server, start_link, []},
    {ok, {#{}, [#{id => coverl_server, start => MFA}]}}.
