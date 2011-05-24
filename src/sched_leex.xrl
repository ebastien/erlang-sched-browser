Definitions.

D = [0-9]
L = [A-Z]
WS = ([\t\s;]|//.*\n)

Rules.

/             : {token, {slash, TokenLine}}.
{L}           : {token, {letter, TokenLine, token_to_char(TokenChars)}}.
{L}{L}        : {token, {twoletters, TokenLine, token_to_string(TokenChars)}}.
{L}{L}{L}     : {token, {threeletters, TokenLine, token_to_string(TokenChars)}}.
{L}{L}{L}{L}+ : {token, {letters, TokenLine, token_to_string(TokenChars)}}.
{D}+          : {token, {integer, TokenLine, token_to_integer(TokenChars)}}.
(\+|-){D}     : {token, {offset, TokenLine, token_to_integer(TokenChars)}}.
{D}{D}{D}{D}-{D}{D}-{D}{D} : {token, {date, TokenLine, token_to_date(TokenChars)}}.
{D}{D}:{D}{D}              : {token, {time, TokenLine, token_to_time(TokenChars)}}.

(\n|\r)+ : {end_token, {eol, TokenLine}}.
{WS}+    : skip_token.

Erlang code.
-compile([native, {hipe, [o3]}]).

token_to_char([C|_]) -> C.

token_to_string(T) -> list_to_binary(T).

token_to_integer(T) -> list_to_integer(T).

token_to_date(T) ->
  calendar:date_to_gregorian_days(
    list_to_integer(string:substr(T, 1, 4)),
    list_to_integer(string:substr(T, 6, 2)),
    list_to_integer(string:substr(T, 9, 2))).

token_to_time(T) ->
  calendar:time_to_seconds({
    list_to_integer(string:substr(T, 1, 2)),
    list_to_integer(string:substr(T, 4, 2)),
    0}).
