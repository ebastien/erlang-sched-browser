-module(sched_server).

-export([start/0, stop/0]).

%% --------------------

start() ->
  application:start(sched_server).

stop() ->
  application:stop(sched_server).
