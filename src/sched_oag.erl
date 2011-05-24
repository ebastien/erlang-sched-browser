-module(sched_oag).
-compile([inline, native, {hipe, [o3]}]).

-include("sched_records.hrl").

%% Public interface
-export([start/0, parse_ssim7/1]).

%% --------------

start() ->
  FileName = "oag.dei",
  {ok, File} = file:open(FileName, [read, read_ahead, binary, raw]),
  {ok, Periods} = parse_ssim7(File),
  file:close(File),
  ok = sched_browser:browse_all_segments(
    fun(SegmentPeriod, _) ->
      io:format("~p~n",[SegmentPeriod])
    end,
    Periods, ok
  ),
  %io:format("~p~n",[Periods]),
  ok.

%% --------------

%% Parse a ssim7 file and return the list of flight periods
parse_ssim7(File) ->
  FirstRecord = parse_record(File),
  {PeriodKey, _} = FirstRecord,
  Periods = [],
  Legs = [],
  DataElements = orddict:new(),
  parse_next_records(File, PeriodKey, FirstRecord, Periods, Legs, DataElements).

%% The leg is for the same flight period
parse_next_records(File, PeriodKey, {PeriodKey, {leg, Leg}}, Periods, Legs, DataElements) ->
  NewLegs = [Leg|Legs],
  NextRecord = parse_record(File),
  parse_next_records(File, PeriodKey, NextRecord, Periods, NewLegs, DataElements);

%% A data element always follows a record with the same period key
parse_next_records(File, PeriodKey, {PeriodKey, {dei, DataElement}}, Periods, Legs, DataElements) ->
  {SegmentKey, SegmentData} = DataElement,
  NewDataElements = case SegmentData of
    unknown -> DataElements;
    _       -> orddict:append(SegmentKey, SegmentData, DataElements)
  end,
  NextRecord = parse_record(File),
  parse_next_records(File, PeriodKey, NextRecord, Periods, Legs, NewDataElements);

%% The leg is for a new flight period
parse_next_records(File, _, {PeriodKey, {leg, Leg}}, Periods, Legs, DataElements) ->
  NewPeriods = append_period(Periods, Legs, DataElements),
  NextRecord = parse_record(File),
  parse_next_records(File, PeriodKey, NextRecord, NewPeriods, [Leg], []);

%% The file is empty
parse_next_records(_, _, eof, [], [], []) ->
  {ok, []};

%% There is no more record
parse_next_records(_, _, eof, Periods, Legs, DataElements) ->
  {ok, append_period(Periods, Legs, DataElements)}.

%% Validate and append a new period to the list
append_period(Periods, Legs, DataElements) ->
  case assemble_flight_period(Legs, DataElements) of
    {ok, NewPeriod} -> [NewPeriod|Periods];
    _               -> Periods
  end.

%% --------------

%% Check whether the leg has valid board and off points according to the known geography
is_leg_valid(Board, Off) ->
  sched_geography:is_valid(Board) andalso sched_geography:is_valid(Off).

%% --------------

%% Assemble a list of legs and segments into a flight period
assemble_flight_period(Legs, DataElements) ->
  case assemble_legs(Legs, DataElements) of
    {ok, AssembledLegs} ->
      AssembledSegments = assemble_segments(AssembledLegs, DataElements),
      %% Only the first leg period has the correct days of operation
      FirstLeg = lists:last(Legs),
      {ok, #flight_period{
        flight_key = FirstLeg#leg_period.flight_key,
        legs = AssembledLegs,
        segments = {specific_segments, AssembledSegments}
      }};
    Error -> Error
  end.

assemble_legs(Legs, DataElements) ->
  assemble_legs(Legs, DataElements, []).
assemble_legs([], _, Acc) ->
  {ok, Acc};
assemble_legs([LegPeriod|NextLegs], DataElements, Acc) ->
  #leg_period{
    leg = #leg{
      leg_key = #leg_key{
        board_point = Board,
        off_point = Off
      }
    }
  } = LegPeriod,
  case is_leg_valid(Board, Off) of
    true ->
      MatchingElements = case orddict:find({Board, Off}, DataElements) of
        {ok, Elements} -> Elements;
        _              -> []
      end,
      NewLeg = LegPeriod#leg_period.leg#leg{data_elements=MatchingElements},
      assemble_legs(NextLegs, DataElements, [NewLeg|Acc]);
    _ ->
      {error, {invalid_leg, LegPeriod}}
  end.

%% --------------

%% Assemble segments from legs and data elements
assemble_segments(AssembledLegs, DataElements) ->
  assemble_segments(AssembledLegs, DataElements, []).

assemble_segments([], _, Acc) -> Acc;
assemble_segments(Legs=[Leg|NextLegs], DataElements, Acc) ->
  NewAcc = assemble_segments_from(Leg, Legs, [], DataElements, Acc),
  assemble_segments(NextLegs, DataElements, NewAcc).

assemble_segments_from(_, [], _, _, Acc) -> Acc;
assemble_segments_from(BoardLeg, [OffLeg|NextOffLegs], PrevLegs, DataElements, Acc) ->
  BoardPoint = BoardLeg#leg.leg_key#leg_key.board_point,
  OffPoint = OffLeg#leg.leg_key#leg_key.off_point,
  NewAcc = if
    %% Skip cycle segment
    BoardPoint == OffPoint -> Acc;
    true ->
      %% Get the list of legs between the board and off legs
      ViaLegs = case lists:reverse(PrevLegs) of
        [_|SomeLegs] -> SomeLegs;
        _            -> []
      end,
      %% Get the data elements
      MatchingElements = case orddict:find({BoardPoint, OffPoint}, DataElements) of
        {ok, Elements} -> Elements;
        _              -> []
      end,
      Segment = #segment{
        %% TODO: decode real segment cabin details
        segment_cabin_details = [#segment_cabin_detail{cabin_code=$Y, classes= <<"YHKMVQ">>}],
        data_elements = orddict:from_list(MatchingElements),
        segment_legs = #segment_legs{
          board_leg = BoardLeg,
          via_legs = ViaLegs,
          off_leg = OffLeg
        }
      },
      [Segment|Acc]
  end,
  assemble_segments_from(BoardLeg, NextOffLegs, [OffLeg|PrevLegs], DataElements, NewAcc).

%% --------------

%% Parse the next record from the file
parse_record(File) ->
  case file:read_line(File) of
    {ok, Data} -> case decode_record(Data) of
                    {ok, Record} -> Record;
                    _            -> parse_record(File)
                  end;
    _ -> eof
  end.

%% --------------

%% Decode a Segment Data Record
decode_record(<<$4, Data/binary>>) ->
  <<
    _Suffix, Airline:3/binary, FlightNumber:4/binary, IVI:2/binary, _LSN:2/binary, _ServiceType,
    _:13/binary, IVI2:1/binary, _BoardID, _OffID, DEI:3/binary, BoardPoint:3/binary, OffPoint:3/binary,
    RawDataElement:155/binary, _Serial:6/binary, "\n"
  >> = Data,
  DEIInt = binary_to_integer(DEI),
  SegmentData = if
    DEIInt == 10 ->
      {marketing_flights, decode_flights_list(RawDataElement)};
    DEIInt == 50 ->
      {operating_flight, decode_flight(RawDataElement)};
    true -> unknown
  end,
  AirlineInt = sched_parser:airline_to_integer(Airline),
  FlightNumberInt = binary_to_integer(FlightNumber),
  DataElement = {
    {sched_parser:port_to_integer(BoardPoint), sched_parser:port_to_integer(OffPoint)},
    SegmentData
  },
  PeriodNb = binary_to_integer(IVI) + 100*binary_to_integer(IVI2),
  Record = {
    {AirlineInt, FlightNumberInt, PeriodNb},  % The period key
    {dei, DataElement}
  },
  {ok, Record};

%% --------------

%% Decode a Flight Leg Record
decode_record(<<$3, Data/binary>>) ->
  <<
    _Suffix, Airline:3/binary, FlightNumber:4/binary, IVI:2/binary, LSN:2/binary, _ServiceType,
    BeginDate:7/binary, EndDate:7/binary, Days:7/binary, _FrequencyRate,
    DepStation:3/binary, _PassengerSTD:4/binary, STD:4/binary, DepUTC:5/binary, _DepTerminal:2/binary,
    ArrStation:3/binary, STA:4/binary, _PassengerSTA:4/binary, ArrUTC:5/binary, _ArrTerminal:2/binary,
    _Aircraft:3/binary, _PRBD:20/binary, _PRBM:5/binary, _Meal:10/binary, _JointOp:9/binary, _MCT:2/binary,
    _:6/binary, IVI2:1/binary, _Owner:3/binary, _CockpitEmployer:3/binary, _CabinEmployer:3/binary, _OnwardFlight:9/binary,
    _, _FTL, _Codeshare, _TRC:11/binary, _TRCLegOF, _:11/binary, _ACV:20/binary, DateVar1:1/binary, DateVar2:1/binary,
    _Serial:6/binary, "\n"
  >> = Data,
  DepTime = binary_to_time(STD),
  ArrTime = binary_to_time(STA),
  DepDateVar = binary_to_datevar(DateVar1),
  ArrDateVar = binary_to_datevar(DateVar2),
  Elapsed = (ArrTime - binary_to_offset(ArrUTC) + ArrDateVar*86400) - (DepTime - binary_to_offset(DepUTC) + DepDateVar*86400),
  AirlineInt = sched_parser:airline_to_integer(Airline),
  FlightNumberInt = binary_to_integer(FlightNumber),
  LegPeriod = #leg_period{
    flight_key = #flight_key{
      airline_code = AirlineInt,
      flight_number = FlightNumberInt,
      begin_date = binary_to_date(BeginDate),
      end_date = binary_to_date(EndDate),
      dow = binary_to_dow(Days)
    },
    leg = #leg{
      sequence = binary_to_integer(LSN),
      leg_key = #leg_key{
        board_point = sched_parser:port_to_integer(DepStation),
        off_point = sched_parser:port_to_integer(ArrStation)
      },
      leg_details  = #leg_details{
        dep_time = {DepTime, DepDateVar},
        arr_time = {ArrTime, ArrDateVar},
        ela_time = Elapsed
      },
      %% TODO: Decode real leg cabin details
      leg_cabin_details = [#leg_cabin_detail{cabin_code=$Y, capacity=1}]
    }
  },
  PeriodNb = binary_to_integer(IVI) + 100*binary_to_integer(IVI2),
  Record = {
    {AirlineInt, FlightNumberInt, PeriodNb},  % The period key
    {leg, LegPeriod}
  },
  {ok, Record};

%% --------------

decode_record(_) ->
  {error, "unknown record"}.

%% --------------

decode_flight(<<Airline:3/binary, FlightNumber:4/binary, _Suffix, _/binary>>) ->
  {sched_parser:airline_to_integer(Airline), binary_to_integer(FlightNumber)}.

decode_flights_list(RawDataElement) ->
  decode_flights_list(RawDataElement, []).

decode_flights_list(<<>>, Acc) -> Acc;
decode_flights_list(<<32, _/binary>>, Acc) -> Acc;
decode_flights_list(<<$/, Rest/binary>>, Acc) ->
  decode_flights_list(Rest, Acc);
decode_flights_list(<<Flight:8/binary, Rest/binary>>, Acc) ->
  decode_flights_list(Rest, [decode_flight(Flight)|Acc]).

%% --------------

binary_to_integer(B) ->
  binary_to_integer(B, 0).
binary_to_integer(<<>>, Acc) -> Acc;
binary_to_integer(<<B, Rest/binary>>, Acc) when (B >= $0) andalso (B =< $9) ->
  binary_to_integer(Rest, Acc*10 + B - $0);
binary_to_integer(<<_, Rest/binary>>, Acc) ->
  binary_to_integer(Rest, Acc).

binary_to_date(<<D:2/binary, M:3/binary, Y:2/binary>>) ->
  case binary_to_month(M) of
    % FIXME: Handle proper open period
    {ok, open}  -> calendar:date_to_gregorian_days(2011, 12, 31);
    {ok, Month} -> calendar:date_to_gregorian_days(binary_to_integer(Y) + 2000, Month, binary_to_integer(D));
    Error       -> Error
  end.

binary_to_month(B) ->
  case B of
    <<"XXX">> -> {ok, open};
    <<"JAN">> -> {ok, 1};
    <<"FEB">> -> {ok, 2};
    <<"MAR">> -> {ok, 3};
    <<"APR">> -> {ok, 4};
    <<"MAY">> -> {ok, 5};
    <<"JUN">> -> {ok, 6};
    <<"JUL">> -> {ok, 7};
    <<"AUG">> -> {ok, 8};
    <<"SEP">> -> {ok, 9};
    <<"OCT">> -> {ok, 10};
    <<"NOV">> -> {ok, 11};
    <<"DEC">> -> {ok, 12};
    _         -> {error, "unknown month code", binary_to_list(B)}
  end.

binary_to_dow(<<A,B,C,D,E,F,G>>) ->
  Dow = case A of $1 -> 1000000; _ -> 0 end +
        case B of $2 -> 0100000; _ -> 0 end +
        case C of $3 -> 0010000; _ -> 0 end +
        case D of $4 -> 0001000; _ -> 0 end +
        case E of $5 -> 0000100; _ -> 0 end +
        case F of $6 -> 0000010; _ -> 0 end +
        case G of $7 -> 0000001; _ -> 0 end,
  sched_parser:dow_to_integer(Dow).

binary_to_time(<<H:2/binary, M:2/binary>>) ->
  calendar:time_to_seconds({binary_to_integer(H), binary_to_integer(M), 0}).

binary_to_offset(<<S, T:4/binary>>) ->
  case S of
    $- -> -1;
    $+ -> +1
  end * binary_to_time(T).

binary_to_datevar(Binary= <<Char>>) ->
  case Char of
    $A -> -1;
    _  -> binary_to_integer(Binary)
  end.
