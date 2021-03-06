-module(sched_server_sup).
-behaviour(supervisor).

%% External exports
-export([start_link/0, upgrade/0]).

%% supervisor callbacks
-export([init/1]).

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
  {ok, {_, Specs}} = init([]),

  Old = sets:from_list(
    [Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
  New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
  Kill = sets:subtract(Old, New),

  sets:fold(fun (Id, ok) ->
        supervisor:terminate_child(?MODULE, Id),
        supervisor:delete_child(?MODULE, Id),
        ok
      end, ok, Kill),

  [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
  ok.

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
  Geography = {
    sched_geography,
    {sched_geography, start_link, []},
    permanent, 5000, worker, dynamic
  },
  Schedules = {
    sched_schedules_db,
    {sched_schedules_db, start_link, []},
    permanent, 5000, worker, dynamic
  },
  Network = {
    sched_network_db,
    {sched_network_db, start_link, []},
    permanent, 5000, worker, dynamic
  },
  Processes = [Geography, Schedules, Network],
  {ok, {{one_for_one, 10, 10}, Processes}}.
