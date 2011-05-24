-module(sched_pathfinder).
-compile([inline, native, {hipe, [o3]}]).
-export([index_segment_paths/2, index_all_paths/4]).

-define(MIN_INTER_POINTS_DISTANCE_RATIO, 0.6).

%% --------------

%% Index a direct segment between two nodes of the network
index_path(NetworkPaths, FromPoint, Segment) ->
  {FromNode, _} = FromPoint,
  {_, EndNode, _} = Segment,
  true = ets:insert(NetworkPaths, {{FromNode, EndNode}, []}),
  ok.

%% Index an indirect path between two nodes of the network
index_path(NetworkPaths, AddedDistance, FromPoint, ToPoint, LowerPath) ->
  {LowerStops, EndNode, LowerSegments} = LowerPath,
  {FromNode, FromCoord} = FromPoint,
  {ToNode, ToCoord} = ToPoint,
  ExtendedStops = [ToNode|LowerStops],
  ExtendedSegments = [{AddedDistance, ToCoord}|LowerSegments],
  PathKey = {FromNode, EndNode},
  case is_extended_path_valid(FromCoord, LowerSegments, AddedDistance) of
    true -> true = ets:insert(NetworkPaths, {PathKey, ExtendedStops}),
            NewPath = {ExtendedStops, EndNode, ExtendedSegments},
            {ok, NewPath};
    _    -> skip
  end.

%% --------------

is_extended_path_valid(FromCoord, [{SegmentDistance, SegmentCoord}|NextSegments], AccDistance) ->
  NewAccDistance = AccDistance + SegmentDistance,
  DirectDistance = sched_geography:orthodromic_distance(FromCoord, SegmentCoord),
  (NewAccDistance*?MIN_INTER_POINTS_DISTANCE_RATIO < DirectDistance) andalso
  is_extended_path_valid(FromCoord, NextSegments, NewAccDistance);

is_extended_path_valid(_, [], _) ->
  true.

%% --------------

index_segment_paths(_, []) -> ok;
index_segment_paths(NetworkPaths, [{FromPoint, NodeSegments}|NextSegments]) ->
  ok = index_segment_paths_from_node(NetworkPaths, FromPoint, NodeSegments),
  index_segment_paths(NetworkPaths, NextSegments).

%% --------------

index_segment_paths_from_node(_, _, []) -> ok;
index_segment_paths_from_node(NetworkPaths, FromPoint, [NodeSegment|NextNodeSegments]) ->
  ok = index_path(NetworkPaths, FromPoint, NodeSegment),
  index_segment_paths_from_node(NetworkPaths, FromPoint, NextNodeSegments).

%% --------------

index_all_paths(NetworkPaths, MaxLength, LowerPaths, InboundAdjacency) ->
  index_all_paths(NetworkPaths, MaxLength, LowerPaths, InboundAdjacency, 1).

%% End of iteration on the path length
index_all_paths(_, MaxLength, _, _, PathLength) when PathLength >= MaxLength -> ok;

%% Index paths by extending lower paths with the given adjacency, no more than the given path length
index_all_paths(NetworkPaths, MaxLength, LowerPaths, InboundAdjacency, PathLength) ->
  %% Extend all the lower paths with the given adjacency list
  io:format("Current PathLength:~p, NetworkPaths size:~p~n", [PathLength+1, ets:info(NetworkPaths, size)]),
  ExtendedPaths = if
    (PathLength+1 == MaxLength) -> index_last_extended_paths(NetworkPaths, LowerPaths, InboundAdjacency);
    true                        -> index_extended_paths(NetworkPaths, LowerPaths, InboundAdjacency)
  end,
  io:format("~n"),
  %% Iterate over existing paths to create longer paths
  index_all_paths(NetworkPaths, MaxLength, ExtendedPaths, InboundAdjacency, PathLength+1).

%% --------------

%% Index the extended paths from the given list of lower paths and inbound adjacency
index_extended_paths(NetworkPaths, LowerPaths, InboundAdjacency) ->
  index_extended_paths(NetworkPaths, LowerPaths, InboundAdjacency, array:new({default, {undefined, []}})).

%% Fast-path for the last iteration on path lengths
index_last_extended_paths(NetworkPaths, LowerPaths, InboundAdjacency) ->
  index_extended_paths(NetworkPaths, LowerPaths, InboundAdjacency, undefined).

%% It is basically a merge intersection between the two node-ordered lists
index_extended_paths(NetworkPaths, 
    [{Point={Node, _}, NodeLowerPaths}|NextLowerPaths],
    [{Node, AdjNodes}|NextInboundAdjacency],
    ExtendedPaths) ->
  %% Extend the paths starting from a given node with segments to all adjacent nodes
  NewExtendedPaths = index_all_extended_paths_to_node(NetworkPaths, Point, NodeLowerPaths, AdjNodes, ExtendedPaths),
  io:format("."),
  index_extended_paths(NetworkPaths, NextLowerPaths, NextInboundAdjacency, NewExtendedPaths);

%% Skip a lower path having a start node not in the adjacency list (i.e unreachable node)
index_extended_paths(NetworkPaths, 
    [{{StartNode, _}, _}|NextLowerPaths],
    InboundAdjacency=[{EndNode, _}|_],
    ExtendedPaths) when StartNode < EndNode ->
  io:format("U"),
  index_extended_paths(NetworkPaths, NextLowerPaths, InboundAdjacency, ExtendedPaths);

%% Skip a node in the adjacency list that has no outbound lower path (i.e dead end node)
index_extended_paths(NetworkPaths, 
    LowerPaths=[{{StartNode, _}, _}|_],
    [{EndNode, _}|NextInboundAdjacency],
    ExtendedPaths) when StartNode > EndNode ->
  io:format("D"),
  index_extended_paths(NetworkPaths, LowerPaths, NextInboundAdjacency, ExtendedPaths);

%% End of iteration over the lower paths
index_extended_paths(_, [], _, ExtendedPaths) ->
  post_process_paths(ExtendedPaths);

%% End of iteration over the adjacency list
index_extended_paths(_, _, [], ExtendedPaths) ->
  post_process_paths(ExtendedPaths).

%% Convert the array of extended paths to a list, to be used by the next iteration
%% TODO: Benchmark keeping the array representation
post_process_paths(undefined) -> undefined;
post_process_paths(ExtendedPaths) -> array:sparse_to_list(ExtendedPaths).

%% --------------

%% Extend the paths starting at a given node with all the segments targeting this node
%% Fast-path for the last iteration on path lengths
index_all_extended_paths_to_node(NetworkPaths, ToPoint, NodeLowerPaths, [FromPoint|NextAdjNodes], undefined) ->
  %% Compute the distance the would be added by the segment [FromPoint, ToPoint] to existing lower paths
  AddedDistance = sched_geography:orthodromic_distance(element(2, FromPoint), element(2, ToPoint)),
  %% Complete the list of extended paths with the given lower paths and adjacent node
  index_extended_paths_with_segment(NetworkPaths, AddedDistance, FromPoint, ToPoint, NodeLowerPaths, undefined),
  %% Iterate over the next adjacent nodes
  index_all_extended_paths_to_node(NetworkPaths, ToPoint, NodeLowerPaths, NextAdjNodes, undefined);

%% Normal path
index_all_extended_paths_to_node(NetworkPaths, ToPoint, NodeLowerPaths, [FromPoint|NextAdjNodes], ExtendedPaths) ->
  {FromNode, FromCoord} = FromPoint,
  %% Get the list of paths starting at the given adjacent node
  {_, FromNodePaths} = array:get(FromNode, ExtendedPaths),
  %% Compute the distance the would be added by the segment [FromPoint, ToPoint] to existing lower paths
  {_, ToCoord} = ToPoint,
  AddedDistance = sched_geography:orthodromic_distance(FromCoord, ToCoord),
  %% Complete the list of extended paths with the given lower paths and adjacent node
  NewFromNodePaths = index_extended_paths_with_segment(NetworkPaths, AddedDistance, FromPoint, ToPoint, NodeLowerPaths, FromNodePaths),
  %% Complete the array of extended paths with the new list
  NewExtendedPaths = array:set(FromNode, {FromPoint, NewFromNodePaths}, ExtendedPaths),
  %% Iterate over the next adjacent nodes
  index_all_extended_paths_to_node(NetworkPaths, ToPoint, NodeLowerPaths, NextAdjNodes, NewExtendedPaths);

%% End of iteration over the adjacent nodes
index_all_extended_paths_to_node(_, _, _, [], ExtendedPaths) ->
  ExtendedPaths.

%% --------------

%% Extend all paths starting at a given node with the given segment
index_extended_paths_with_segment(NetworkPaths, AddedDistance, FromPoint, ToPoint, [NodeLowerPath|NextPaths], FromNodePaths) ->
  NewFromNodePaths =
    case index_path(NetworkPaths, AddedDistance, FromPoint, ToPoint, NodeLowerPath) of
      %% The extended path is valid and successfully indexed
      {ok, NewPath} -> case FromNodePaths of
                         undefined -> undefined;
                         _         -> [NewPath|FromNodePaths]
                       end;
      %% The extended path is not valid
      _             -> FromNodePaths
    end,
  %% Iterate over the next lower paths
  index_extended_paths_with_segment(NetworkPaths, AddedDistance, FromPoint, ToPoint, NextPaths, NewFromNodePaths);

%% End of iteration over the lower paths
index_extended_paths_with_segment(_, _, _, _, [], FromNodePaths) ->
  FromNodePaths.
