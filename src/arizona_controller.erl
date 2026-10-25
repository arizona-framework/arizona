-module(arizona_controller).
-moduledoc """
Controller routes: plain HTTP actions that drive the page without a full reload,
written against Arizona's request and reply types only, so the same controller
runs on any `arizona_server` adapter.

A controller route (a verb tag like `{post, ...}`, or `{match, ...}`) dispatches to an
action function, `Handler:Action/2` (the `action` option, default `handle`), that the
browser reaches via `arizona_js:fetch/2`. Because it is a real HTTP request, the
response may carry a `Set-Cookie` (HttpOnly honored) the WebSocket transport can't --
so it suits flows that rotate a session cookie while the page stays put.

## The action

The action receives the `arizona_req:request()` the route's middleware pipeline
produced and a context, `#{state := State, bindings := Bindings}` -- the route's
`state` option (`#{}` when unset) and the bindings the pipeline produced
(`session` from `fetch_session`, ...) -- and returns `{Reply, Request}`:

```erlang
handle(Req0, _Ctx) ->
    {ok, Body, Req1} = arizona_req:body(Req0),
    %% ...parse Body, validate, derive UserId...
    Req2 = arizona_req:clear_session(Req1),
    Req3 = arizona_req:put_session(Req2, ~"user_id", UserId),
    %% Show server-computed content by broadcasting to a topic the view
    %% subscribed to; the response effects are for request-local UI only.
    arizona_pubsub:broadcast({account, UserId}, {account_updated, Body}),
    {arizona_controller:reply_effects([arizona_js:set_attr(~"#error", ~"hidden", ~"")]), Req3}.
```

Return the request you wrote to: what it carries -- session and flash writes,
`arizona_req:put_resp_cookie/4` and `put_resp_header/3` -- is what reaches the
response (and, in store mode, the session store). Thread it through
`arizona_req:body/1` too, since reading the body advances the request.

## Replies

| Kind | Shape |
|------|-------|
| buffered | `{Status, Headers, Body}` |
| stream | `{stream, Status, Headers, Fun}` -- `Fun(Send)`; `Send(Data, nofin \| fin)` |
| sendfile | `{sendfile, Status, Headers, {Path, Offset, Length}}` |
| loop | `{loop, Status, Headers, State}` -- the controller exports `handle_info/3` |

Every adapter answers a buffered reply. The others are a capability the
adapter declares (`c:arizona_server:capabilities/0`): `dispatch/3` refuses one it
lacks with `{unsupported_reply, Kind, Adapter}` before the session commits,
rather than the reply degrading silently.

A loop keeps the response open and hands every message the connection receives
to `Handler:handle_info(Msg, Push, State)`, where `Push(Data)` writes one chunk;
return `{ok, State}` to keep going or `{stop, State}` to end the response (the
Server-Sent Events shape). When the client goes away it gets one final
`{arizona_disconnect, Reason}`, after which the loop ends whatever it returns.

`reply_effects/1,2` and `reply_redirect/1` build the buffered reply the `fetch`
command expects: the same `{"e": [...]}` effects wire payload the WebSocket
sends. To re-render the live view after the request, broadcast over
`arizona_pubsub`; the effects returned here are for request-local UI (inline
error/success) or a `navigate`.
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([dispatch/3]).
-export([reply_effects/1]).
-export([reply_effects/2]).
-export([reply_redirect/1]).
-export([format_error/2]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

%% Called by the server adapters, which live in their own packages.
-ignore_xref([dispatch/3]).

-ignore_xref([reply_effects/1]).
-ignore_xref([reply_effects/2]).
-ignore_xref([reply_redirect/1]).
%% Called by `erl_error` via the `error_info` annotation, not directly.
-ignore_xref([format_error/2]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([route/0]).
-export_type([context/0]).
-export_type([headers/0]).
-export_type([sendfile_spec/0]).
-export_type([reply/0]).
-export_type([reply_kind/0]).
-export_type([send_fun/0]).
-export_type([push_fun/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

%% What a controller route resolved to: the action to run and what it runs with.
-nominal route() :: #{
    handler := module(),
    action := atom(),
    state := term(),
    middlewares := [arizona_middleware:middleware()]
}.

%% What an action is handed beside the request.
-nominal context() :: #{
    state := term(),
    bindings := az:bindings()
}.

-nominal headers() :: [{binary(), iodata()}].

-nominal sendfile_spec() :: {file:filename_all(), non_neg_integer(), non_neg_integer()}.

-nominal reply() ::
    {arizona_req:resp_status(), headers(), iodata()}
    | {stream, arizona_req:resp_status(), headers(), fun((send_fun()) -> term())}
    | {sendfile, arizona_req:resp_status(), headers(), sendfile_spec()}
    | {loop, arizona_req:resp_status(), headers(), State :: term()}.

-nominal reply_kind() :: buffered | stream | sendfile | loop.

-nominal send_fun() :: fun((iodata(), nofin | fin) -> ok | {error, term()}).

-nominal push_fun() :: fun((iodata()) -> ok | {error, term()}).

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc """
Runs a controller route against a request for the server adapter `Adapter`: the
route's middleware pipeline
(the CSRF `check_origin` step included by default), then the action, handed the
route `state` and the pipeline's bindings as its `t:context/0`.

Returns the reply with `arizona_req:flush/1`'s lines ahead of the action's own
headers (so a written session is committed and its cookie sent), and the request
the action returned, whose raw value is what the adapter hands back to its
server. A middleware halt returns its `arizona_req:halt_reply/1` as a buffered
reply, without running the action.

A reply kind `Adapter` lacks raises `{unsupported_reply, Kind, Adapter}`, like a
malformed reply, before the flush. The adapter maps the rest onto its server;
for a loop it routes the connection's messages to
`Handler:handle_info/3`, translating its server's disconnect signal into
`{arizona_disconnect, Reason}`.
""".
-spec dispatch(Route, Request, Adapter) -> {reply(), Request} when
    Route :: route(),
    Request :: arizona_req:request(),
    Adapter :: module().
dispatch(#{middlewares := Middlewares} = Route, ArzReq, Adapter) ->
    case arizona_middleware:apply_middlewares(Middlewares, ArzReq, #{}) of
        {halt, HaltReq} ->
            {Status, Headers} = arizona_req:halt_reply(HaltReq),
            {{Status, Headers, <<>>}, HaltReq};
        {cont, ArzReq1, Bindings} ->
            #{handler := Handler, action := Action, state := State} = Route,
            Ctx = #{state => State, bindings => Bindings},
            {Reply, ArzReq3} = dispatch_action(Handler, Action, ArzReq1, Ctx),
            %% Checked before the flush, which commits a written session: a
            %% reply the adapter cannot send must not leave a write behind.
            ok = check_reply(Handler, Reply),
            ok = check_supported(Reply, Adapter),
            {with_flush(Reply, arizona_req:flush(ArzReq3)), ArzReq3}
    end.

-doc """
Builds a `200 application/json` reply whose body is the `{"e": [...]}` effects wire
payload the `arizona_js:fetch/2` command applies on the page (the same effects
`handle_event/3` returns). 200 is the **success** leg -- a fetch form with `az-form-reset`
clears only on a 2xx. For an error leg that keeps the typed fields, use `reply_effects/2`
with a non-2xx status (the effects still apply).
""".
-spec reply_effects(Effects) -> reply() when
    Effects :: arizona_effect:cmds().
reply_effects(Effects) ->
    reply_effects(200, Effects).

-doc """
Like `reply_effects/1` but with an explicit HTTP status. The `fetch` command applies the
effects on **any** status, so a controller can return a real `422` for a validation error
-- keeping the form's typed fields (a fetch form only resets on a 2xx) while still
rendering the error, e.g.
`reply_effects(422, [arizona_js:push_event(~"invalid", #{~"field" => ~"password"})])`.

A `navigate`/`patch` with a `flash` opt is refused (`flash_outside_live_effect`):
that opt is carried by the WebSocket, for an effect a live handler returns, and a
SPA navigate cannot see a cookie set after its socket opened. To show a flash on
the page a controller sends the user to, set it on the request and navigate in
full, so the browser's next GET carries the cookie:

```erlang
handle(Req, _Ctx) ->
    Reply = arizona_controller:reply_effects([arizona_js:navigate(~"/done", #{full => true})]),
    {Reply, arizona_req:put_flash(Req, info, ~"Saved.")}.
```
""".
-spec reply_effects(Status, Effects) -> reply() when
    Status :: arizona_req:resp_status(),
    Effects :: arizona_effect:cmds().
reply_effects(Status, Effects) ->
    {Status, [{~"content-type", ~"application/json"}],
        json:encode(#{~"e" => arizona_effect:json_value(Effects)})}.

-doc """
Builds a reply that sends the client to `Location` via a SPA navigation. Sugar for
`reply_effects([arizona_js:navigate(Location)])`: a fetch-followed HTTP 3xx can't drive
a SPA navigation (its `Location` is unreadable), so a redirect is delivered as a
`navigate` effect instead.

A `Location` that is not a path on this origin (`arizona_req:location_kind/1`) -- an
identity provider, an absolute URL -- cannot be SPA-navigated, so it is sent with the
`full` option and the browser loads it for real. A relative one (`~"login"`) loads too,
resolved against the current URL exactly as an HTTP `Location` would be. A scheme other
than `http`/`https` (`javascript:`, `data:`) raises `{unsafe_redirect, Location}`: the
client would run it in the page, so a `Location` taken from the request (a `next`
parameter) would be script injection.

This is a SPA navigation over the existing WebSocket, so it suits same-identity
redirects. After an **identity change** (login/logout) use `arizona_js:reload/0`
instead, so the socket re-handshakes with the new session.
""".
-spec reply_redirect(Location) -> reply() when
    Location :: binary().
reply_redirect(Location) ->
    case arizona_req:location_kind(Location) of
        local ->
            reply_effects([arizona_js:navigate(Location)]);
        external ->
            reply_effects([arizona_js:navigate(Location, #{full => true})]);
        unsafe ->
            erlang:error({unsafe_redirect, Location}, [Location], [
                {error_info, #{module => ?MODULE}}
            ])
    end.

-doc """
Renders this module's errors. Picked up by `erl_error` via the `error_info`
annotation.
""".
-spec format_error(Reason, Stacktrace) -> ErrorInfo when
    Reason :: term(),
    Stacktrace :: [tuple()],
    ErrorInfo :: #{general := iolist()}.
format_error({unsafe_redirect, Location}, _ST) ->
    #{
        general => io_lib:format(
            "refusing to redirect to ~0tp: only a path or an http(s) URL may be "
            "navigated to. Any other scheme (javascript:, data:, ...) would run in "
            "the page; validate a request-supplied redirect target before using it.",
            [Location]
        )
    };
format_error({missing_action, Handler, Action, Arity}, _ST) ->
    #{
        general => io_lib:format(
            "controller ~s does not export the action ~s/~b. "
            "Check the route's `action` option (default `handle`), or define the function.",
            [Handler, Action, Arity]
        )
    };
format_error({missing_loop_callback, Handler}, _ST) ->
    #{
        general => io_lib:format(
            "controller ~s returned a {loop, ...} reply but does not export "
            "handle_info/3, which receives the loop's messages.",
            [Handler]
        )
    };
format_error({bad_reply, Reply}, _ST) ->
    #{
        general => io_lib:format(
            "a controller action returned ~0tp as its reply. A reply is "
            "{Status, Headers, Body}, {stream, Status, Headers, Fun}, "
            "{sendfile, Status, Headers, {Path, Offset, Length}}, or "
            "{loop, Status, Headers, State}.",
            [Reply]
        )
    };
format_error({unsupported_reply, Kind, Adapter}, _ST) ->
    #{
        general => io_lib:format(
            "the ~s server adapter cannot send a ~s reply. Use a reply kind it "
            "supports, or serve this route through an adapter that has it.",
            [Adapter, Kind]
        )
    }.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% Dispatch to the controller's action (Handler:Action/2, `action` defaulting to
%% `handle` at compile time). A missing/unexported action is re-tagged into a clear
%% `{missing_action, ...}` error (rendered by format_error/2); an `undef` from
%% *inside* the action body propagates untagged.
dispatch_action(Handler, Action, ArzReq, Ctx) ->
    try
        Handler:Action(ArzReq, Ctx)
    catch
        error:undef:ST ->
            arizona_error:raise_or_propagate(
                undef,
                ST,
                {Handler, Action, 2},
                {missing_action, Handler, Action, 2},
                [Handler, Action, ArzReq],
                ?MODULE
            )
    end.

check_reply(_Handler, {Status, _Headers, _Body}) when is_integer(Status) ->
    ok;
check_reply(_Handler, {stream, Status, _Headers, _Fun}) when is_integer(Status) ->
    ok;
check_reply(_Handler, {sendfile, Status, _Headers, _Spec}) when is_integer(Status) ->
    ok;
check_reply(Handler, {loop, Status, _Headers, _State}) when is_integer(Status) ->
    case erlang:function_exported(Handler, handle_info, 3) of
        true ->
            ok;
        false ->
            erlang:error({missing_loop_callback, Handler}, none, [
                {error_info, #{module => ?MODULE}}
            ])
    end;
check_reply(_Handler, Reply) ->
    erlang:error({bad_reply, Reply}, none, [{error_info, #{module => ?MODULE}}]).

check_supported({Kind, _Status, _Headers, _Payload}, Adapter) ->
    #{replies := Supported} = arizona_server_registry:capabilities(Adapter),
    case lists:member(Kind, Supported) of
        true -> ok;
        false -> unsupported_reply(Kind, Adapter)
    end;
check_supported(_Buffered, _Adapter) ->
    ok.

%% The flushed lines go ahead of the action's own headers, every reply kind.
with_flush({Status, Headers, Body}, Flush) when is_integer(Status) ->
    {Status, Flush ++ Headers, Body};
with_flush({Kind, Status, Headers, Payload}, Flush) ->
    {Kind, Status, Flush ++ Headers, Payload}.

%% A reply kind the adapter lacks: the controller fails where it asked for it
%% instead of the client receiving something else.
-spec unsupported_reply(Kind, Adapter) -> no_return() when
    Kind :: reply_kind(),
    Adapter :: module().
unsupported_reply(Kind, Adapter) ->
    erlang:error({unsupported_reply, Kind, Adapter}, [Kind, Adapter], [
        {error_info, #{module => ?MODULE}}
    ]).
