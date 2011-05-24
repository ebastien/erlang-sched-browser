-module(sched_parser).
-compile([inline, native, {hipe, [o3]}]).

%% Public interface
-export([parse_periods/1, start_bench/0]).
-export([airline_to_integer/1, integer_to_airline/1, port_to_integer/1, integer_to_port/1, dow_to_integer/1]).

%% Callbacks interface
-export([tokenize_period/1, bench_loop/2]).

%%TODO: Migrate to new segment and leg record structure

%% --------------------

%% Convert an alphanumeric uppercase ([0-9A-Z ]*) binary to an integer
alphanum_to_integer(B) -> alphanum_to_integer(B, 0).
alphanum_to_integer(<<>>, Acc) -> Acc;
alphanum_to_integer(<<32, Rest/binary>>, Acc) ->
  alphanum_to_integer(Rest, Acc*37);
alphanum_to_integer(<<A, Rest/binary>>, Acc) when (A >= $0) andalso (A =< $9) ->
  alphanum_to_integer(Rest, Acc*37 + A - $0 + 1);
alphanum_to_integer(<<A, Rest/binary>>, Acc) when (A >= $A) andalso (A =< $Z) ->
  alphanum_to_integer(Rest, Acc*37 + A - $A + 11).

%% Convert an integer to an uppercase alphanumeric ([0-9A-Z ]*) binary
integer_to_alphanum(N) -> integer_to_alphanum(N, []).
integer_to_alphanum(0, Acc) -> list_to_binary(Acc);
integer_to_alphanum(N, Acc) ->
  Rem = N rem 37,
  Char = if
    Rem == 0  -> 32;
    Rem >= 11 -> Rem - 11 + $A;
    true      -> Rem - 1 + $0
  end,
  integer_to_alphanum(N div 37, [Char|Acc]).

%% Convert a pseudo integer representing a bitarray (e.g 1001101) to a true bitarray
base2_to_bitarray(0) -> 0;
base2_to_bitarray(Integer) ->
  (base2_to_bitarray(Integer div 10) bsl 1) + (Integer rem 10).

%% --------------------

airline_to_integer(Airline) -> alphanum_to_integer(Airline).

integer_to_airline(N) -> integer_to_alphanum(N).

port_to_integer(Port) -> alphanum_to_integer(Port).

integer_to_port(N) -> integer_to_alphanum(N).

dow_to_integer(Dow) -> base2_to_bitarray(Dow).

%% --------------------

%% Tokenize a single period from the given ssim7 file
tokenize_period(File) ->
  io:request(File, {get_until, prompt, sched_leex, tokens, [1]}).

%% Return all periods from the given ssim7 file
parse_periods(File) ->
  sched_yecc:parse_and_scan({sched_parser, tokenize_period, [File]}).

%% --------------------

start_bench() ->
  %S1 = "BA; 9; 2007-04-20; 2007-06-30; 1111100; LHR; BKK; 22:00; 15:15 / +1; 11:15; F; 5; J; 12; W; 20; Y; 300; BKK; SYD; 18:10 / +1; 06:05 / +2; 08:55; F; 5; J; 12; W; 20; Y; 300; 1; LHR; BKK; F; FA; J; JCDI; W; WT; Y; YBHKMLSQ; BKK; SYD; F; FA; J; JCDI; W; WT; Y; YBHKMLSQ; LHR; SYD; F; FA; J; JCDI; W; W; Y; YBHKMLSQ;\n",
  %S2 = "AA; 101; 2007-04-20; 2007-06-30; 1111111; LHR; JFK; 09:55; 12:50; 07:55; G; 300; 0; G; GHQKLMVSOWN;\n",
  S3 = "BA; 117; 2009-01-15; 2009-01-28; 1000000; LHR; JFK; 08:20; 11:00; 07:40; Y; 300; 0; Y; YM;\n",
  N = 100000,
  io:format("Start~n", []),
  {Time, _} = timer:tc(sched_parser, bench_loop, [S3, N]),
  io:format("Total elapsed time = ~p microseconds~nAverage iteration time = ~p microseconds~n", [Time, Time/N]),
  ok.

bench_loop(_, 0) -> ok;
bench_loop(S, N) ->
  ok = bench(S),
  bench_loop(S, N-1).

bench(S) ->
  {ok, Tokens, _} = sched_leex:string(S),
  {ok, _} = sched_yecc:parse(Tokens),
  ok.
