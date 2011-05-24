-module(sched_nif).

-export([init/0, orthodromic_distance/2, orthodromic_distance/4]).

-define(APPLICATION, sched_server).

init() ->
  Lib = filename:join([code:priv_dir(?APPLICATION), "sched_server_drv"]),
  ok = erlang:load_nif(Lib, []).

orthodromic_distance({LonA, LatA}, {LonB, LatB}) ->
  sched_nif:orthodromic_distance(LonA, LatA, LonB, LatB).

orthodromic_distance(_, _, _, _) ->
  erlang:error(not_implemented).
