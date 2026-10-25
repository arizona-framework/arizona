-module(arizona_sse).
-moduledoc """
Server-Sent Events framing, independent of any server: what a controller `loop`
reply pushes (`arizona_controller`).

Field values that must stay on one line (an event name, an id, a comment) raise
`{sse_line_break, Field, Value}` on a CR or LF rather than splitting the event: a
value echoed from user input could otherwise inject an `event:`, `id:` or
`retry:` field. `data` may span lines; it is split on every line break a client
recognises (CR, LF, CRLF) into one `data:` line each.
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([event/2]).
-export([event/3]).
-export([comment/1]).

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc "A named event with a data payload.".
-spec event(Name, Data) -> iodata() when
    Name :: binary(),
    Data :: binary().
event(Name, Data) when is_binary(Name), is_binary(Data) ->
    [event_line(Name), data_lines(Data), $\n].

-doc "A named event with an id (what a reconnecting client resumes from).".
-spec event(Name, Data, Id) -> iodata() when
    Name :: binary(),
    Data :: binary(),
    Id :: binary().
event(Name, Data, Id) when is_binary(Name), is_binary(Data), is_binary(Id) ->
    ok = single_line(Id, event_id),
    [event_line(Name), ~"id: ", Id, $\n, data_lines(Data), $\n].

-doc "A comment line, unseen by the client's handlers: a keep-alive through proxies.".
-spec comment(Text) -> iodata() when
    Text :: binary().
comment(Text) when is_binary(Text) ->
    ok = single_line(Text, comment),
    [~": ", Text, ~"\n\n"].

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

event_line(Name) ->
    ok = single_line(Name, event_name),
    [~"event: ", Name, $\n].

single_line(Value, Field) ->
    case binary:match(Value, [~"\r", ~"\n"]) of
        nomatch -> ok;
        _Found -> error({sse_line_break, Field, Value})
    end.

data_lines(Data) ->
    [[~"data: ", Line, $\n] || Line <- binary:split(Data, [~"\r\n", ~"\r", ~"\n"], [global])].
