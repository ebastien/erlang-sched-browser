-module(sched_server_app).
-behaviour(application).
-export([start/2, stop/1]).

%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for sched_server.
start(_Type, _StartArgs) ->
    sched_server_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for sched_server.
stop(_State) ->
    ok.
