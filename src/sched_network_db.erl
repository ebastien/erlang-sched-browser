-module(sched_network_db).
-compile([inline, native, {hipe, [o3]}]).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([
  add_segment/1,
  index_paths/0,
  save/0,
  load/0,
  all_trips/3,
  all_trips/4,
  all_paths/2
]).

-include("sched_records.hrl").

-record(state, {outbound_segments, inbound_adjency, network_paths}).

-define(APPLICATION, sched_server).

% --------------------

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
  State = #state{
    outbound_segments = array:new({default, {undefined, []}}),
    inbound_adjency = array:new({default, {undefined, []}})
  },
  {ok, State}.

handle_call(save, _From, State) ->
  ok = dump(State),
  {reply, ok, State};

handle_call(load, _From, State) ->
  {ok, NewState} = restore(State),
  {reply, ok, NewState};

handle_call({add_segment, SegmentPeriod}, _From, State) ->
  {ok, NewState} = add_segment(SegmentPeriod, State),
  {reply, ok, NewState};

handle_call(index_paths, _From, State) ->
  {ok, NewState} = index_paths(State),
  {reply, ok, NewState};

handle_call(state, _From, State) ->
  {reply, {ok, State}, State}.

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

index_paths() ->
  gen_server:call(?MODULE, index_paths, infinity).

save() ->
  gen_server:call(?MODULE, save, infinity).

load() ->
  gen_server:call(?MODULE, load, infinity).

all_trips(Origin, Destination, Departure) ->
  all_trips(Origin, Destination, Departure, undefined).

all_trips(Origin, Destination, Departure, AirlinesFilter) ->
  {ok, State} = gen_server:call(?MODULE, state),
  all_trips(Origin, Destination, Departure, AirlinesFilter, State).

all_paths(Origin, Destination) ->
  {ok, State} = gen_server:call(?MODULE, state),
  all_paths(Origin, Destination, State).

% --------------------

network_cache_path() ->
  filename:join([code:lib_dir(?APPLICATION), "cache", "network"]).

dump(State) ->
  ok = ets:tab2file(State#state.network_paths, network_cache_path()).

restore(State) ->
  {ok, NetworkPaths} = ets:file2tab(network_cache_path()),
  {ok, State#state{network_paths=NetworkPaths}}.

%% --------------

add_segment(SegmentPeriod, State) ->
  #segment_period{
    segment = #segment{
      segment_legs = #segment_legs{
        board_leg = #leg{leg_key=#leg_key{board_point=BoardNode}},
        off_leg = #leg{leg_key=#leg_key{off_point=OffNode}}
      }
    }
  } = SegmentPeriod,
  #state{
    outbound_segments = OutboundSegments,
    inbound_adjency = InboundAdjacency,
    network_paths = undefined
  } = State,
  %% Build a new segment
  BoardCoord = sched_geography:coordinates(BoardNode),
  BoardPoint = {BoardNode, BoardCoord},
  OffCoord = sched_geography:coordinates(OffNode),
  Distance = sched_geography:orthodromic_distance(BoardCoord, OffCoord),
  NewSegment = {
    [],                     % List of stop nodes between start and end nodes
    OffNode,                % Designator of end node
    [{Distance, OffCoord}]  % Lengths and coordinates of segments between start and end points
  },
  %% Update the data structures with the new segment
  {_, ExistingSegments} = array:get(BoardNode, OutboundSegments),
  NewOutboundSegments = case lists:member(NewSegment, ExistingSegments) of
    false -> array:set(BoardNode, {BoardPoint, [NewSegment|ExistingSegments]}, OutboundSegments);
    _     -> OutboundSegments
  end,
  {_, AdjNodes} = array:get(OffNode, InboundAdjacency),
  NewInboundAdjacency = case lists:member(BoardPoint, AdjNodes) of
    false -> array:set(OffNode, {OffNode, [BoardPoint|AdjNodes]}, InboundAdjacency);
    _     -> InboundAdjacency
  end,
  NewState = State#state{
    outbound_segments = NewOutboundSegments,
    inbound_adjency = NewInboundAdjacency
  },
  {ok, NewState}.

%% --------------

index_paths(State) ->
  #state{
    outbound_segments = OutboundSegments,
    inbound_adjency = InboundAdjacency,
    network_paths = undefined
  } = State,
  OutboundSegmentsList = array:sparse_to_list(OutboundSegments),
  InboundAdjacencyList = array:sparse_to_list(InboundAdjacency),
  NetworkPaths = ets:new(undefined, [bag, protected]),
  ok = sched_pathfinder:index_segment_paths(NetworkPaths, OutboundSegmentsList),
  ok = sched_pathfinder:index_all_paths(NetworkPaths, 3, OutboundSegmentsList, InboundAdjacencyList),
  NewState = State#state{
    outbound_segments = undefined,
    inbound_adjency = undefined,
    network_paths = NetworkPaths
  },
  {ok, NewState}.

%% --------------

all_paths(Origin, Destination, State) ->
  NetworkPaths = State#state.network_paths,
  try ets:lookup_element(NetworkPaths, {Origin, Destination}, 2) of
    Paths -> {ok, lists:sort(fun(A,B) -> length(A) =< length(B) end, Paths)}
  catch
    error:badarg -> {ok, []}
  end.
  
all_trips(Origin, Destination, Departure, AirlinesFilter, State) ->
  {ok, Paths} = all_paths(Origin, Destination, State),
  sched_schedules_db:all_trips(Origin, Destination, Departure, AirlinesFilter, Paths).
