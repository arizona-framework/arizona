-module(arizona_roadrunner_resp).
-moduledoc """
Response-side roadrunner adapter -- the counterpart of `arizona_roadrunner_req`.

A middleware stashes response headers and cookies on the `arizona_req:request()`
(`put_resp_header/3`, `put_resp_cookie/4`) because it has no response to write
them on yet. Every roadrunner dispatcher (`arizona_roadrunner_http`,
`arizona_roadrunner_ws`, `arizona_roadrunner_controller`) therefore ends by
folding that stash onto whatever response it is about to return -- which is what
`flush/2` does, for every response shape a handler may return, not just the
buffered `{Status, Headers, Body}` triple.

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
Builds the response for a middleware `{halt, Request}`, stashed headers and
cookies included.

A halting middleware says what the client should see by stashing a redirect
(`arizona_req:redirect/2,3`) or a status (`arizona_req:put_resp_status/2`) --
`check_origin/2`'s `403` is the built-in example. A halt carrying neither is a
refusal with no reason disclosed, which is exactly `403 Forbidden` (RFC 9110
§15.5.4): `204` would report a denial as success (a browser sits on the page
with no feedback, a `fetch` sees `ok` and resets the form), and `400` would
blame request syntax the framework knows nothing about.
""".
-spec halt(Request) -> Resp when
    Request :: arizona_req:request(),
    Resp :: roadrunner_resp:buffered_response().
halt(HaltReq) ->
    Resp =
        case arizona_req:halted_redirect(HaltReq) of
            {Status, Location} ->
                roadrunner_resp:redirect(Status, Location);
            undefined ->
                case arizona_req:resp_status(HaltReq) of
                    undefined -> roadrunner_resp:forbidden();
                    Status -> roadrunner_resp:status(Status)
                end
        end,
    flush(HaltReq, Resp).

-doc """
Folds the request's stashed response headers and cookies onto `Resp`, then
commits a written server-side session (`arizona_req:commit_session/1`) -- the
flush is the commit-on-success point, so the store write happens only once a
response has actually been built, never as a side effect of reading
`arizona_req:resp_cookies/1`.

Accepts every `t:roadrunner_handler:response/0` shape: the buffered triple, the
`stream`/`loop`/`sendfile` quadruples, and a `websocket` upgrade, whose headers
ride the `101 Switching Protocols` (a bare `{websocket, Module, State}` grows the
headers element). Flushing an empty stash leaves any shape unchanged.
""".
-spec flush(Request, Resp) -> Resp when
    Request :: arizona_req:request(),
    Resp :: roadrunner_handler:response().
flush(ArzReq, Resp0) ->
    Resp1 = lists:foldl(
        fun({Name, Value}, R) -> add_header(R, Name, Value) end,
        Resp0,
        arizona_req:resp_headers(ArzReq)
    ),
    Resp = lists:foldl(
        fun({Name, Value, Opts}, R) ->
            add_header(R, ~"set-cookie", roadrunner_cookie:serialize(Name, Value, Opts))
        end,
        Resp1,
        arizona_req:resp_cookies(ArzReq)
    ),
    ok = arizona_req:commit_session(ArzReq),
    Resp.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

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
