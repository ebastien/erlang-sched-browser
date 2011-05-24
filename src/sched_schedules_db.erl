-module(sched_schedules_db).
-compile([inline, native, {hipe, [o3]}]).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([
  add_segment/1,
  save/0,
  load/0,
  all_trips/5,
  airlines/0
]).

-include("sched_records.hrl").

-record(state, {segments_table, schedules_tables}).

-define(APPLICATION, sched_server).

-define(DEFAULT_MIN_CONNECTION_TIME, (30*60)).   % Default MCT is 30 minutes
-define(WAIT_TIME_DISTANCE_RATIO, 180).

% --------------------

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
  State = #state{
    segments_table = ets:new(undefined, [bag, protected]),
    schedules_tables = dict:new()
  },
  {ok, State}.

handle_call({add_segment, SegmentPeriod}, _From, State) ->
  {ok, NewState} = add_segment(SegmentPeriod, State),
  {reply, ok, NewState};

handle_call(save, _From, State) ->
  ok = dump_all_tables(State),
  {reply, ok, State};

handle_call(load, _From, State) ->
  {ok, NewState} = restore_all_tables(State),
  {reply, ok, NewState};

handle_call(state, _From, State) ->
  {reply, {ok, State}, State};

handle_call(airlines, _From, State) ->
  Airlines = dict:fetch_keys(State#state.schedules_tables),
  {reply, {ok, Airlines}, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

% --------------------

add_segment(SegmentPeriod) ->
  gen_server:call(?MODULE, {add_segment, SegmentPeriod}).

save() ->
  gen_server:call(?MODULE, save, infinity).

load() ->
  gen_server:call(?MODULE, load, infinity).

all_trips(Origin, Destination, Departure, AirlinesFilter, Paths) ->
  {ok, State} = gen_server:call(?MODULE, state),
  OrgCoord = sched_geography:coordinates(Origin),
  DstCoord = sched_geography:coordinates(Destination),
  Distance = sched_geography:orthodromic_distance(OrgCoord, DstCoord),
  MaxWait = ?WAIT_TIME_DISTANCE_RATIO*math:sqrt(Distance),
  all_trips(Origin, Destination, Departure, AirlinesFilter, Paths, State, MaxWait, []).

airlines() ->
  gen_server:call(?MODULE, airlines).

% --------------------

new_table() ->
  ets:new(undefined, [ordered_set, protected]).

get_table(Airline, ScheduleTables) ->
  [Table] = dict:fetch(Airline, ScheduleTables), Table.

put_table(Airline, Table, ScheduleTables) ->
  dict:append(Airline, Table, ScheduleTables).

segments_cache_path() ->
  filename:join([code:lib_dir(?APPLICATION), "cache", "segmentairlines"]).

schedules_cache_path() ->
  schedules_cache_path("*").

schedules_cache_path(Airline) when is_integer(Airline) ->
  schedules_cache_path(integer_to_list(Airline));

schedules_cache_path(AirlineFile) ->
  filename:join([code:lib_dir(?APPLICATION), "cache", "schedules", AirlineFile]).

dump_all_tables(State) ->
  #state{
    segments_table = SegmentsTable,
    schedules_tables = ScheduleTables
  } = State,
  ok = ets:tab2file(SegmentsTable, segments_cache_path()),
  dict:fold(fun(Airline, [Table], _) -> dump_table(Airline, Table) end, 0, ScheduleTables).

dump_table(Airline, Table) ->
  FileName = schedules_cache_path(Airline),
  ok = filelib:ensure_dir(FileName),
  ok = ets:tab2file(Table, FileName).

restore_all_tables(State) ->
  {ok, SegmentsTable} = ets:file2tab(segments_cache_path()),
  ScheduleTables = State#state.schedules_tables,
  FileNames = filelib:wildcard(schedules_cache_path()),
  NewScheduleTables = lists:foldl(fun(F, S) -> restore_table(F, S) end, ScheduleTables, FileNames),
  NewState = State#state{
    segments_table = SegmentsTable,
    schedules_tables = NewScheduleTables
  },
  {ok, NewState}.

restore_table(FileName, ScheduleTables) ->
  {ok, Table} = ets:file2tab(FileName),
  Airline = list_to_integer(filename:basename(FileName)),
  put_table(Airline, Table, ScheduleTables).
  
%% --------------------

add_segment(SegmentPeriod, State) ->
  #segment_period{
    flight_key = #flight_key{
      airline_code = Airline,
      begin_date = BeginDate,
      end_date = EndDate
    },
    segment = #segment{
      segment_legs = #segment_legs{
        board_leg = #leg{leg_key=#leg_key{board_point=BoardNode}},
        off_leg = #leg{leg_key=#leg_key{off_point=OffNode}}
      }
    }
  } = SegmentPeriod,
  #state{
    segments_table = SegmentsTable,
    schedules_tables = ScheduleTables
  } = State,
  %% Index the airline providing this segment
  true = ets:insert(SegmentsTable, {{BoardNode, OffNode}, Airline}),
  %% Get an existing schedules table for this airline or create a new one
  {Table, NewScheduleTables} = try get_table(Airline, ScheduleTables) of
    ExistingTable -> {ExistingTable, ScheduleTables}
  catch
    error:badarg  -> NewTable = new_table(),
                     {NewTable, put_table(Airline, NewTable, ScheduleTables)}
  end,
  %% Index all the segment dates of this period
  ok = index_segment_dates(Table, SegmentPeriod, BeginDate, EndDate),
  NewState = State#state{schedules_tables=NewScheduleTables},
  {ok, NewState}.

index_segment_dates(_, _, BeginDate, EndDate) when BeginDate > EndDate -> ok;

index_segment_dates(Table, SegmentPeriod, BeginDate, EndDate) ->
  Dow = SegmentPeriod#segment_period.flight_key#flight_key.dow,
  ok = case is_date_in_dow(BeginDate, Dow) of
    true -> index_segment_date(Table, SegmentPeriod, BeginDate);
    _    -> ok
  end,
  index_segment_dates(Table, SegmentPeriod, BeginDate+1, EndDate).

index_segment_date(Table, SegmentPeriod, BeginDate) ->
  #segment_period{
    flight_key = #flight_key{
      airline_code = Airline,
      flight_number = FlightNumber
    },
    segment = #segment{
      segment_legs = #segment_legs{
        board_leg = #leg{
          leg_key = #leg_key{board_point=Board},
          leg_details = #leg_details{dep_time=DepTime}
        },
        off_leg = #leg{
          leg_key = #leg_key{off_point=Off},
          leg_details = #leg_details{arr_time=ArrTime}
        }
      }
    }
  } = SegmentPeriod,
  {DepDayTime, DepDayOffset} = DepTime,
  {ArrDayTime, ArrDayOffset} = ArrTime,
  DepDateTime = (BeginDate + DepDayOffset)*86400 + DepDayTime,
  ArrDateTime = (BeginDate + ArrDayOffset)*86400 + ArrDayTime,
  ScheduleKey = {Airline, Board, Off, DepDateTime, ArrDateTime, FlightNumber},
  true = ets:insert(Table, {ScheduleKey}),
  ok.

%% --------------------

is_date_in_dow(Date, Dow) ->
  Dow band (1 bsl (7 - calendar:day_of_the_week(calendar:gregorian_days_to_date(Date)))) /= 0.

%% --------------

all_trips(_, _, _, _, [], _, _, AllTrips) ->
  {ok, AllTrips};
all_trips(Origin, Destination, Departure, AirlinesFilter, [Path|NextPaths], State, MaxWait, AllTrips) ->
  %% Continue searching for solutions
  #state{
    segments_table = SegmentsTable,
    schedules_tables = ScheduleTables
  } = State,
  Tables = {ScheduleTables, SegmentsTable, AirlinesFilter},
  %% Initiate the list of inbounds connection with the preferred deprature date/time as a pseudo connection
  PseudoSegment = {undefined, undefined, undefined, undefined, Departure, undefined},
  Inbounds = [[{PseudoSegment, MaxWait}]],
  Trips = connect_path(Tables, Origin, Destination, Inbounds, Path),
  NewAllTrips = [{Path, Trips}|AllTrips],
  all_trips(Origin, Destination, Departure, AirlinesFilter, NextPaths, State, MaxWait, NewAllTrips).

%% --------------

%% The last iteration over the segments of a path
connect_path(Tables, BoardNode, EndNode, Inbounds, []) ->
  connect_segment(Tables, BoardNode, EndNode, Inbounds);

%% Walk a path, connecting all valid segments
connect_path(Tables, BoardNode, EndNode, Inbounds, [OffNode|NextNodes]) ->
  %% Stop walking the path as soon as no valid connection is found
  case connect_segment(Tables, BoardNode, OffNode, Inbounds) of
    []          -> [];
    NewInbounds -> connect_path(Tables, OffNode, EndNode, NewInbounds, NextNodes)
  end.

%% --------------

%% Connect a segment with all inbounds
connect_segment(Tables, BoardNode, OffNode, Inbounds) ->
  {ScheduleTables, SegmentsTable, AirlinesFilter} = Tables,
  %% TODO: Catch the exception if no airline can be found
  Airlines = ets:lookup_element(SegmentsTable, {BoardNode, OffNode}, 2),
  FilteredAirlines = case AirlinesFilter of
    undefined -> Airlines;
    _         -> %% TODO: sort the list of airlines in advance
                 SortedAirlines = ordsets:from_list(Airlines),
                 ordsets:intersection(SortedAirlines, AirlinesFilter)
  end,
  connect_segment(ScheduleTables, BoardNode, OffNode, Inbounds, FilteredAirlines, []).

connect_segment(_, _, _, [], _, Acc) -> Acc;
connect_segment(ScheduleTables, BoardNode, OffNode, [Inbound|NextInbounds], FilteredAirlines, Acc) ->
  %% Get the last arrival time and the max wait time from the inbound connection
  [{{LastAirline, _, _, _, Arrival, LastFlightNumber}, MaxWait}|_] = Inbound,
  %% Handle MCT and max wait time only in case of connecting segment
  {MinDeparture, MaxDeparture} = case LastAirline of 
    %% This is the first segment => lookup until 24 hours after preferred departure datetime
    undefined -> {Arrival, Arrival + 86399};
    %% This is a connecting segment
    _         -> {Arrival + ?DEFAULT_MIN_CONNECTION_TIME, Arrival + MaxWait}
  end,
  Segments = if
    MinDeparture < MaxDeparture ->
      lookup_airlines(ScheduleTables, BoardNode, OffNode, MinDeparture, MaxDeparture, FilteredAirlines);
    true -> []
  end,
  %% NOTE: Extending inbound connections could be done while fetching schedules to avoid iterating again.
  NewAcc = extend_inbound(LastAirline, LastFlightNumber, Arrival, MaxWait, Inbound, Segments, Acc),
  connect_segment(ScheduleTables, BoardNode, OffNode, NextInbounds, FilteredAirlines, NewAcc).

%% --------------

%% Extend inbound segments with candidate connections
extend_inbound(_, _, _, _, _, [], Acc) -> Acc;
extend_inbound(LastAirline, LastFlightNumber, Arrival, MaxWait, Inbound, [Segment|NextSegments], Acc) ->
  {Airline, _, _, Departure, _, FlightNumber} = Segment,
  NewAcc = if
    %% Deny connection on the same flight
    Airline == LastAirline andalso FlightNumber == LastFlightNumber ->
      Acc;
    true ->
      NewMaxWait = case LastAirline of
        %% Do not decrement wait time on the first segment
        undefined -> MaxWait;
        _         -> MaxWait - (Departure - Arrival)
      end,
      [[{Segment, NewMaxWait} | Inbound] | Acc]
  end,
  extend_inbound(LastAirline, LastFlightNumber, Arrival, MaxWait, Inbound, NextSegments, NewAcc).

%% --------------------

%% Lookup segments in airlines' schedules
lookup_airlines(ScheduleTables, BoardNode, OffNode, MinDeparture, MaxDeparture, Airlines) ->
  lookup_airlines(ScheduleTables, BoardNode, OffNode, MinDeparture, MaxDeparture, Airlines, []).

%% Loop over selected airlines
lookup_airlines(_, _, _, _, _, [], Acc) -> Acc;
lookup_airlines(ScheduleTables, BoardNode, OffNode, MinDeparture, MaxDeparture, [Airline|NextAirlines], Acc) ->
  Table = get_table(Airline, ScheduleTables),
  ScheduleKey = {Airline, BoardNode, OffNode, MinDeparture, 0, 0},
  NewAcc = lookup_schedules(Table, MaxDeparture, ScheduleKey, Acc),
  lookup_airlines(ScheduleTables, BoardNode, OffNode, MinDeparture, MaxDeparture, NextAirlines, NewAcc).

%% Lookup all schedules for a given key
lookup_schedules(Table, MaxDeparture, ScheduleKey, Acc) ->
  NextScheduleKey = next_schedule_key(Table, ScheduleKey, MaxDeparture),
  lookup_next_schedules(Table, MaxDeparture, NextScheduleKey, Acc).

%% Fetch the next valid schedule key
next_schedule_key(Table, ScheduleKey, MaxDeparture) ->
  {Airline, BoardNode, OffNode, _, _, _} = ScheduleKey,
  NextScheduleKey = ets:next(Table, ScheduleKey),
  case NextScheduleKey of
    {Airline, BoardNode, OffNode, Departure, _, _} when Departure < MaxDeparture ->
      NextScheduleKey;
    _ -> not_found
  end.

%% Loop over valid schedules
lookup_next_schedules(_, _, not_found, Acc) -> Acc;
lookup_next_schedules(Table, MaxDeparture, ScheduleKey, Acc) ->
  NextScheduleKey = next_schedule_key(Table, ScheduleKey, MaxDeparture),
  lookup_next_schedules(Table, MaxDeparture, NextScheduleKey, [ScheduleKey|Acc]).
