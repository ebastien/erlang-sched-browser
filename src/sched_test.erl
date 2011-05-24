-module(sched_test).
-compile([inline, native, {hipe, [o3]}]).

%% Public interface
-export([start/0]).

%% Callbacks interface
-export([bench_loop/1, async_bench_loop/2]).

-include("sched_records.hrl").

%% --------------------

benchmark(Mod, Fun, Args) ->
  {Time, Result} = timer:tc(Mod, Fun, Args),
  io:format("Elapsed on ~p:~p = ~p microseconds~n", [Mod, Fun, Time]),
  Result.

%% --------------------

test_index(Origin, Destination, AllPathsIndex) ->
  TestStart = sched_parser:port_to_integer(Origin),
  TestEnd = sched_parser:port_to_integer(Destination),
  TestPaths = ets:lookup_element(AllPathsIndex, {TestStart, TestEnd}, 2),
  lists:foreach(
    fun(P) ->
      lists:foreach(fun(S) -> io:format("~s ", [binary_to_list(sched_parser:integer_to_port(S))]) end, [TestStart] ++ P ++ [TestEnd]),
      io:format("~n")
    end,
    TestPaths
  ).

%% --------------------

check_reachability(AllPathsIndex, SegmentsIndex) ->
  BoardNodes = lists:usort(lists:flatten(ets:match(SegmentsIndex, {{'$1', '_'}, '_'}))),
  OffNodes = lists:usort(lists:flatten(ets:match(SegmentsIndex, {{'_', '$1'}, '_'}))),
  UnconnectedNodes = check_reachability(AllPathsIndex, BoardNodes, OffNodes, []),
  lists:foreach(
    fun({A, B}) ->
      io:format("[~s ~s]", [binary_to_list(sched_parser:integer_to_port(A)), binary_to_list(sched_parser:integer_to_port(B))])
    end,
    lists:sublist(UnconnectedNodes, 10)
  ),
  io:format("~n"),
  io:format("Number of origins         : ~p~n", [length(BoardNodes)]),
  io:format("Number of destinations    : ~p~n", [length(OffNodes)]),
  io:format("Number of unconnected OnDs: ~p~n", [length(UnconnectedNodes)]),
  ok.

check_reachability(_, [], _, Acc) -> Acc;
check_reachability(AllPathsIndex, [BoardNode|NextBoardNodes], OffNodes, Acc) ->
  NewAcc = check_reachability_from(AllPathsIndex, BoardNode, OffNodes, Acc),
  check_reachability(AllPathsIndex, NextBoardNodes, OffNodes, NewAcc).

check_reachability_from(_, _, [], Acc) -> Acc;
check_reachability_from(AllPathsIndex, Node, [Node|NextOffNodes], Acc) ->
  check_reachability_from(AllPathsIndex, Node, NextOffNodes, Acc);
check_reachability_from(AllPathsIndex, BoardNode, [OffNode|NextOffNodes], Acc) ->
  Key = {BoardNode, OffNode},
  NewAcc = case ets:member(AllPathsIndex, Key) of
    false -> [Key|Acc];
    _     -> Acc
  end,
  check_reachability_from(AllPathsIndex, BoardNode, NextOffNodes, NewAcc).

%% --------------------

print_all_trips([]) -> ok;
print_all_trips([{Path, Trips}|NextAllTrips]) ->
  io:format("Trips trough:"),
  lists:foreach(
    fun(A) -> io:format(" ~s", [binary_to_list(sched_parser:integer_to_port(A))]) end,
    Path
  ),
  io:format("~n"),
  ok = print_trips(Trips),
  print_all_trips(NextAllTrips).

print_trips([]) -> ok;
print_trips([Trip|NextTrips]) ->
  io:format("  Connections:~n"),
  % Drop the pseudo connection used for the preferred departure date/time
  [_|InOrderTrip] = lists:reverse(Trip),
  ok = print_connections(InOrderTrip),
  print_trips(NextTrips).

print_connections([]) -> ok;
print_connections([{SegmentDateTime, _}|NextSegmentDateTimes]) ->
  ok = print_segment_datetime(SegmentDateTime),
  print_connections(NextSegmentDateTimes).

print_segment_datetime(SegmentDateTime) ->
  {Airline, Board, Off, DepDateTime, ArrDateTime, FlightNumber} = SegmentDateTime,
  {{DepYear, DepMonth, DepDay}, {DepHour, DepMin, _}} = calendar:gregorian_seconds_to_datetime(DepDateTime),
  {{ArrYear, ArrMonth, ArrDay}, {ArrHour, ArrMin, _}} = calendar:gregorian_seconds_to_datetime(ArrDateTime),
  io:format("    ~2s ~5..0B ~3s ~4..0B-~2..0B-~2..0B ~2..0B:~2..0B ~3s ~4..0B-~2..0B-~2..0B ~2..0B:~2..0B~n", [
    sched_parser:integer_to_airline(Airline), FlightNumber,
    sched_parser:integer_to_port(Board), DepYear, DepMonth, DepDay, DepHour, DepMin,
    sched_parser:integer_to_port(Off), ArrYear, ArrMonth, ArrDay, ArrHour, ArrMin
  ]).

%% --------------------

add_segment(SegmentPeriod, Counter) ->
  case is_fully_operated(SegmentPeriod) of
    true ->
      ok = sched_network_db:add_segment(SegmentPeriod),
      ok = sched_schedules_db:add_segment(SegmentPeriod),
      Counter+1;
    _ ->
      Counter
  end.

is_fully_operated(SegmentPeriod) ->
  #segment_period{
    segment = #segment{
      segment_legs = #segment_legs{
        board_leg = BoardLeg,
        via_legs = ViaLegs,
        off_leg = OffLeg
      }
    }
  } = SegmentPeriod,
  SegmentLegs = if
    BoardLeg#leg.sequence == OffLeg#leg.sequence -> [BoardLeg];
    true -> [BoardLeg] ++ ViaLegs ++ [OffLeg]
  end,
  %% A fully operated segment has no marketed leg
  not lists:any(
    fun(Leg) -> orddict:is_key(operating_flight, Leg#leg.data_elements) end,
    SegmentLegs
  ).

%% --------------------

start() ->
  {ok, CachedIndexes} = application:get_env(sched_server, cache),
  case CachedIndexes of
    true ->
      ok = sched_schedules_db:load(),
      ok = sched_network_db:load();
    _    ->
      {ok, File} = file:open("oag.raw.test.new", [read, read_ahead, binary, raw]),
      {ok, Periods} = benchmark(sched_oag, parse_ssim7, [File]),
      file:close(File),
      io:format("Number of flight periods                 : ~p~n", [length(Periods)]),
      OperatingSegments = benchmark(sched_browser, browse_all_segments, [fun add_segment/2, Periods, 0]),
      io:format("Number of fully operated segment periods : ~p~n", [OperatingSegments]),
      ok = sched_network_db:index_paths(),
      ok = sched_schedules_db:save(),
      ok = sched_network_db:save()
  end,

  {ok, AllTrips} = bench(),
  ok = print_all_trips(lists:reverse(AllTrips)),

%   N = 1000,
%   fprof:apply(sched_test, bench_loop, [AllPathsIndex, SegmentsIndex, SchedulesIndex, N]),
%   ok = fprof:profile(),
%   ok = fprof:analyse(),
%   init:stop().

%   ok = percept:profile("percept.dat", {?MODULE, async_bench_loop, [50, 100 div 50]}, [procs]),

%   lcnt:apply(fun() -> async_bench_loop(1000) end),
%   lcnt:save("lcnt.dat"),
%   lcnt:conflicts([{combine, false}, {print, [name, id, tries, ratio, time]}]),

  N = 1000,
  io:format("Start~n", []),
  {Time, _} = timer:tc(sched_test, async_bench_loop, [50, N div 50]),
  io:format("Total elapsed time = ~p microseconds~nAverage iteration time = ~p microseconds~n", [Time, Time/N]),
  init:stop().

%% --------------------

bench() ->
  sched_network_db:all_trips(
    sched_parser:port_to_integer(<<"NCE">>),
    sched_parser:port_to_integer(<<"SIN">>),
    calendar:datetime_to_gregorian_seconds({{2010, 10, 5}, {0, 0, 0}})
  ).

bench_loop(0) -> ok;
bench_loop(N) ->
  {ok, _} = bench(),
  bench_loop(N-1).

%% --------------------

async_bench_loop(_, 0) -> ok;
async_bench_loop(N, T) ->
  async_bench(N),
  async_bench_loop(N, T-1).

async_bench(N) ->
  async_bench(N, 0).

async_bench(0, 0) -> ok;
async_bench(0, M) ->
  receive
    {async_bench, ok} -> async_bench(0, M-1)
  end;
async_bench(N, M) ->
  Pid = self(),
  spawn(fun() ->
    {ok, _} = bench(),
    Pid ! {async_bench, ok}
  end),
  async_bench(N-1, M+1).
