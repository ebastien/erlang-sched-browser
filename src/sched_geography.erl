-module(sched_geography).
-compile([inline, native, {hipe, [o3]}]).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([orthodromic_distance/2, coordinates/1, is_valid/1]).

-define(GEOGRAPHY_ETS, geography_index).
-define(APPLICATION, sched_server).

%% --------------

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
  %% FIXME: NIF reloading does not work
  ok = sched_nif:init(),
  GeographyIndex = ets:new(?GEOGRAPHY_ETS, [set, protected, named_table]),
  {ok, File} = file:open(airports_path(), [read, read_ahead, raw]),
  eof = parse_airports(File, GeographyIndex),
  file:close(File),
  {ok, undefined}.

handle_call(_Request, _From, State) ->
  {noreply, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% --------------

airports_path() ->
  filename:join([code:lib_dir(?APPLICATION), "data", "airports.csv"]).

parse_airports(File, GeographyIndex) ->
  case file:read_line(File) of
    {ok, Line} -> Fields = decode_line(Line),
                  NewGeographyIndex = index_airport(Fields, GeographyIndex),
                  parse_airports(File, NewGeographyIndex);
    _          -> eof
  end.

decode_line(Chars) ->
  decode_line(Chars, [], []).
decode_line([Char], Elements, Acc) when Char == 10 ->
  Element = lists:reverse(Acc),
  lists:reverse([Element|Elements]);
decode_line([Char|NextChars], Elements, Acc) when Char == $, ->
  Element = lists:reverse(Acc),
  decode_line(NextChars, [Element|Elements], []);
decode_line([Char|NextChars], Elements, Acc) ->
  decode_line(NextChars, Elements, [Char|Acc]).

index_airport(Fields, GeographyIndex) ->
  AirportDetails = list_to_tuple(Fields),
  {Airport, Longitude, Latitude, _City, _Contry, _Zone} = AirportDetails,
  AirportKey = sched_parser:port_to_integer(list_to_binary(Airport)),
  Coordinates = {safe_float(Longitude)*math:pi()/180, safe_float(Latitude)*math:pi()/180},
  true = ets:insert(GeographyIndex, {AirportKey, Coordinates}),
  GeographyIndex.

safe_float(S) ->
   try list_to_float(S) catch error:badarg -> list_to_integer(S) end.

%% --------------

orthodromic_distance(CoordA, CoordB) ->
  sched_nif:orthodromic_distance(CoordA, CoordB).

is_valid(Node) ->
  ets:member(?GEOGRAPHY_ETS, Node).

coordinates(Node) ->
  ets:lookup_element(?GEOGRAPHY_ETS, Node, 2).

%% --------------
