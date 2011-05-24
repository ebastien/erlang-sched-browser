Nonterminals
flight_period_list flight_period flight_key
airline_code flight_number begin_date end_date dow
legs leg leg_key board_point off_point
leg_details leg_time elapsed_time leg_cabin_details
leg_cabin_detail cabin_code capacity segments
segment_cabin_details segment_cabin_detail
classes family_cabin_details family_cabin_detail
family_code segment_details segment_detail segment_key.

Terminals
slash letter twoletters threeletters letters integer
date time offset eol.

Rootsymbol flight_period_list.

flight_period_list -> flight_period : ['$1'].
flight_period_list -> flight_period flight_period_list : ['$1' | '$2'].

flight_period -> flight_key legs segments eol: #flight_period{flight_key='$1', legs='$2', segments='$3'}.

flight_key -> airline_code flight_number begin_date end_date dow : #flight_key{airline_code='$1', flight_number='$2', begin_date='$3', end_date='$4', dow='$5'}.

airline_code -> twoletters : sched_parser:airline_to_integer(token_to_term('$1')).
airline_code -> threeletters : sched_parser:airline_to_integer(token_to_term('$1')).

flight_number -> integer : token_to_term('$1').

begin_date -> date : token_to_term('$1').

end_date -> date : token_to_term('$1').

dow -> integer : sched_parser:dow_to_integer(token_to_term('$1')).

legs -> leg : ['$1'].
legs -> leg legs : ['$1' | '$2'].

leg -> leg_key leg_details leg_cabin_details : #leg{leg_key='$1', leg_details='$2', leg_cabin_details='$3'}.

leg_key -> board_point off_point : #leg_key{board_point='$1', off_point='$2'}.

board_point -> threeletters : sched_parser:port_to_integer(token_to_term('$1')).

off_point -> threeletters : sched_parser:port_to_integer(token_to_term('$1')).

leg_details -> leg_time leg_time elapsed_time : #leg_details{dep_time='$1', arr_time='$2', ela_time='$3'}.

leg_time -> time : {token_to_term('$1'), 0}.
leg_time -> time slash offset : {token_to_term('$1'), token_to_term('$3')}.

elapsed_time -> time : token_to_term('$1').

leg_cabin_details -> leg_cabin_detail : ['$1'].
leg_cabin_details -> leg_cabin_detail leg_cabin_details : ['$1' | '$2'].

leg_cabin_detail -> cabin_code capacity : #leg_cabin_detail{cabin_code='$1', capacity='$2'}.

cabin_code -> letter : token_to_term('$1').

capacity -> integer : token_to_term('$1').

segments -> integer segment_cabin_details : {generic_segments, '$2'}.
segments -> integer segment_details : {specific_segments, '$2'}.

segment_cabin_details -> segment_cabin_detail : ['$1'].
segment_cabin_details -> segment_cabin_detail segment_cabin_details : ['$1' | '$2'].

segment_cabin_detail -> cabin_code classes : #segment_cabin_detail{cabin_code='$1', classes='$2'}.
segment_cabin_detail -> cabin_code classes family_cabin_details : #segment_cabin_detail{cabin_code='$1', classes='$2', family_cabin_details='$3'}.

classes -> letter : C = token_to_term('$1'), <<C>>.
classes -> twoletters : token_to_term('$1').
classes -> threeletters : token_to_term('$1').
classes -> letters : token_to_term('$1').

family_cabin_details -> family_cabin_detail : ['$1'].
family_cabin_details -> family_cabin_detail family_cabin_details : ['$1' | '$2'].

family_cabin_detail -> family_code classes : #family_cabin_detail{family_code='$1', classes='$2'}.

family_code -> integer : token_to_term('$1').

segment_details -> segment_detail : ['$1'].
segment_details -> segment_detail segment_details : ['$1' | '$2'].

segment_detail -> segment_key segment_cabin_details : #segment{segment_key='$1', segment_cabin_details='$2'}.

segment_key -> board_point off_point : #segment_key{board_point='$1', off_point='$2'}.
 
Erlang code.
-compile([native, {hipe, [o3]}]).
-include("sched_records.hrl").

token_to_term({_,_,T}) -> T.
