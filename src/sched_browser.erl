-module(sched_browser).
-compile([inline, native, {hipe, [o3]}]).

%% Public interface
-export([
  browse_all_legs/3,
  browse_all_segments/3
]).

-include("sched_records.hrl").

%% --------------------

browse_all_legs(Function, [Period|NextPeriods], Acc) ->
  #flight_period{flight_key=FlightKey, legs=Legs} = Period,
  NewAcc = browse_flight_legs(Function, FlightKey, Legs, Acc),
  browse_all_legs(Function, NextPeriods, NewAcc);
browse_all_legs(_, [], Acc) ->
  Acc.

browse_flight_legs(Function, FlightKey, [Leg|NextLegs], Acc) ->
  NewAcc = browse_leg(Function, FlightKey, Leg, Acc),
  browse_flight_legs(Function, FlightKey, NextLegs, NewAcc);
browse_flight_legs(_, _, [], Acc) ->
  Acc.

browse_leg(Function, FlightKey, Leg, Acc) ->
  LegPeriod = #leg_period{
    flight_key = FlightKey,
    leg = Leg
  },
  Function(LegPeriod, Acc).

%% --------------------

browse_all_segments(Function, [Period|NextPeriods], Acc) ->
  #flight_period{flight_key=FlightKey, legs=Legs, segments=Segments} = Period,
  NewAcc = browse_flight_segments(Function, FlightKey, Legs, Segments, Acc),
  browse_all_segments(Function, NextPeriods, NewAcc);
browse_all_segments(_, [], Acc) ->
  Acc.

browse_flight_segments(Function, FlightKey, Legs, {generic_segments, _}, Acc) ->
  browse_generic_segments(Function, FlightKey, Legs, Legs, Acc);
browse_flight_segments(Function, FlightKey, _, {specific_segments, Segments}, Acc) ->
  browse_specific_segments(Function, FlightKey, Segments, Acc).

browse_specific_segments(Function, FlightKey, [Segment|NextSegments], Acc) ->
  NewAcc = browse_segment(Function, FlightKey, Segment, Acc),
  browse_specific_segments(Function, FlightKey, NextSegments, NewAcc);
browse_specific_segments(_, _, [], Acc) ->
  Acc.

browse_generic_segments(Function, FlightKey, [BoardLeg|NextBoardLegs], OffLegs, Acc) ->
  NewAcc = browse_segments_from(Function, FlightKey, BoardLeg, OffLegs, Acc),
  [_|NextOffLegs] = OffLegs,
  browse_generic_segments(Function, FlightKey, NextBoardLegs, NextOffLegs, NewAcc);
browse_generic_segments(_, _, [], [], Acc) ->
  Acc.

browse_segments_from(Function, FlightKey, BoardLeg, [OffLeg|NextOffLegs], Acc) ->
  BoardPoint = BoardLeg#leg.leg_key#leg_key.board_point,
  OffPoint = OffLeg#leg.leg_key#leg_key.off_point,
  NewAcc = if
    %% Skip cycle segment
    BoardPoint == OffPoint -> Acc;
    true ->
      Segment = #segment{
        %% TODO: Get cabin details from the generic spec
        segment_cabin_details = [],
        data_elements = [],
        segment_legs = #segment_legs{board_leg=BoardLeg, off_leg=OffLeg}
      },
      browse_segment(Function, FlightKey, Segment, Acc)
  end,
  browse_segments_from(Function, FlightKey, BoardLeg, NextOffLegs, NewAcc);
browse_segments_from(_, _, _, [], Acc) ->
  Acc.

browse_segment(Function, FlightKey, Segment, Acc) ->
  SegmentPeriod = #segment_period{
    flight_key = FlightKey,
    segment = Segment
  },
  Function(SegmentPeriod, Acc).

