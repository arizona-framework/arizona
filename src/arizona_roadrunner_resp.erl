-module(arizona_roadrunner_resp).
-moduledoc """
Response-side roadrunner adapter -- the counterpart of `arizona_roadrunner_req`.

A middleware stashes response headers and cookies on the `arizona_req:request()`
(`put_resp_header/3`, `put_resp_cookie/4`) because it has no response to write
them on yet. Every roadrunner dispatcher (`arizona_roadrunner_http`,
`arizona_roadrunner_ws`, `arizona_roadrunner_controller`) therefore ends by
folding that stash onto whatever response it is about to return -- which is what
`flush/2` does, for every response shape a handler may return, not just the
buffered `{Status, Headers, Body}` triple. What goes on the wire is decided
server-neutrally by `arizona_req:flush/1` and `arizona_req:halt_reply/1`; this
module only maps it onto roadrunner's response shapes.

`halt/1` builds the whole response for a middleware `{halt, Request}` the same
way for all three, so the status a denied request sees does not depend on which
dispatcher happened to run it.
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([halt/1]).
-export([flush/2]).

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc """
Builds the roadrunner response for a middleware `{halt, Request}` from
`arizona_req:halt_reply/1` (stashed headers and cookies included, session
committed), so the status a denied request sees does not depend on which
dispatcher happened to run it.
""".
-spec halt(Request) -> Resp when
    Request :: arizona_req:request(),
    Resp :: roadrunner_resp:buffered_response().
halt(HaltReq) ->
    {Status, Headers} = arizona_req:halt_reply(HaltReq),
    add_headers(roadrunner_resp:status(Status), Headers).

-doc """
Folds `arizona_req:flush/1`'s header lines onto `Resp`, which also commits a
written server-side session: the flush is the commit-on-success point, so the
store write happens only once a response has actually been built.

Accepts every `t:roadrunner_handler:response/0` shape: the buffered triple, the
`stream`/`loop`/`sendfile` quadruples, and a `websocket` upgrade, whose headers
ride the `101 Switching Protocols` (a bare `{websocket, Module, State}` grows the
headers element). Flushing an empty stash leaves any shape unchanged.
""".
-spec flush(Request, Resp) -> Resp when
    Request :: arizona_req:request(),
    Resp :: roadrunner_handler:response().
flush(ArzReq, Resp) ->
    add_headers(Resp, arizona_req:flush(ArzReq)).

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

add_headers(Resp, Headers) ->
    lists:foldl(fun({Name, Value}, R) -> add_header(R, Name, Value) end, Resp, Headers).

%% Prepend one header onto a handler response. The buffered triple goes through
%% roadrunner's own builder (it guards the name and flattens an iodata value);
%% the streaming shapes carry their headers in the third position, a websocket
%% upgrade in the fourth.
add_header({Status, _Headers, _Body} = Resp, Name, Value) when is_integer(Status) ->
    roadrunner_resp:add_header(Resp, Name, Value);
add_header({Kind, Status, Headers, Payload}, Name, Value) when
    Kind =:= stream; Kind =:= loop; Kind =:= sendfile
->
    {Kind, Status, [{Name, iolist_to_binary(Value)} | Headers], Payload};
add_header({websocket, Module, State}, Name, Value) ->
    add_header({websocket, Module, State, []}, Name, Value);
add_header({websocket, Module, State, Headers}, Name, Value) ->
    {websocket, Module, State, [{Name, iolist_to_binary(Value)} | Headers]}.
