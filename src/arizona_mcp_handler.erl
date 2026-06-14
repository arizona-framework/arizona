-module(arizona_mcp_handler).
-moduledoc """
Roadrunner handler for an MCP (Model Context Protocol) route.

Implements MCP's Streamable HTTP transport for a tools server: a client
POSTs a single JSON-RPC 2.0 message and receives a buffered
`application/json` reply; with sessions enabled, `GET` opens the SSE
channel and `DELETE` tears a session down. Before any dispatch the request
passes the security gate: the `Origin` allowlist (DNS-rebinding defense)
and the route's optional `auth` hook.

The MCP method dispatch (`dispatch/3`) is a pure function over a decoded
request and the app's `arizona_mcp` module -- it is exported so it can be
unit-tested without a transport, mirroring `arizona_stateful`'s `call_*`
helpers. `handle/1` owns the transport: reading the body, the `Origin`
gate, and mapping a dispatch crash to a JSON-RPC `-32603` error so the
protocol contract holds even when a tool raises.
""".

-behaviour(roadrunner_handler).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([handle/1]).
-export([handle_info/3]).
-export([dispatch/3]).
-export([handle_method/4]).
-export([run_streaming_tool/6]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([session/0]).
-export_type([auth_hook/0]).
-export_type([auth_result/0]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

%% `dispatch/3` is called by `handle/1` and exercised directly from the
%% test suite; it has no cross-module caller in the release.
-ignore_xref([dispatch/3]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

%% The dispatch context: the handler module, its state, the capabilities
%% negotiated at initialize, the list page size, and the minimum log severity
%% the client set (via `logging/setLevel`). `arizona_mcp_session` holds one
%% across a stateful connection; `dispatch/3` builds an ephemeral one per
%% request.
-type session() :: #{
    mod := module(),
    state := arizona_mcp:state(),
    caps := arizona_mcp:capabilities(),
    page_size := pos_integer(),
    log_min_severity := 0..7
}.

%% An optional per-route auth hook, run after the Origin check and before any
%% dispatch. `{reject, Status}` short-circuits with that HTTP status.
-type auth_hook() :: fun((roadrunner_req:request()) -> auth_result()) | {module(), atom()}.
-type auth_result() :: ok | {reject, roadrunner_http:status()}.

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------

%% Protocol revisions this server speaks. The client's requested version
%% is echoed when supported, else the preferred one is offered.
-define(PREFERRED_VERSION, ~"2025-11-25").
-define(SUPPORTED_VERSIONS, [~"2025-11-25", ~"2025-06-18", ~"2025-03-26"]).

%% Idle teardown for a stateful session, overridable via the route's
%% `session_ttl_ms` opt. Five minutes is generous for an interactive agent.
-define(DEFAULT_TTL_MS, 300000).

%% Resumability buffer depth per session, overridable via the route's
%% `session_buffer_max` opt.
-define(DEFAULT_BUFFER_MAX, 256).

%% Page size for the `*/list` methods, overridable via the route's `page_size`
%% opt. A list at or under this size returns no cursor; a larger one paginates.
-define(DEFAULT_PAGE_SIZE, 50).

%% The minimum log level before the client sends `logging/setLevel`: `info`, so
%% operational logs flow but `debug` is suppressed until requested.
-define(DEFAULT_LOG_LEVEL, info).

%% How long a buffered (non-streaming) request waits on its session before the
%% client is freed with a timeout, overridable via the route's
%% `request_timeout_ms` opt. Generous, since a tool may legitimately run a while.
-define(DEFAULT_REQUEST_TIMEOUT_MS, 60000).

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc """
Roadrunner `handle/1` callback. Serves the MCP endpoint: a JSON-RPC POST
returns a buffered `application/json` reply. When sessions are enabled,
`DELETE` tears a session down; otherwise non-POST methods return `405`.
""".
-spec handle(Req) -> {Response, Req} when
    Req :: roadrunner_req:request(),
    Response :: roadrunner_handler:response().
handle(Req) ->
    #{arizona := Opts} = roadrunner_req:state(Req),
    {serve(roadrunner_req:method(Req), Req, Opts), Req}.

%% POST is always served. DELETE tears a session down when sessions are
%% enabled. Everything else is method-not-allowed (the server-to-client GET
%% stream is a later phase).
serve(~"POST", Req, Opts) ->
    with_gate(Req, Opts, fun() -> handle_post(Req, Opts) end);
serve(~"GET", Req, Opts) ->
    session_verb(Req, Opts, fun() -> handle_get(Req) end);
serve(~"DELETE", Req, Opts) ->
    session_verb(Req, Opts, fun() -> handle_delete(Req) end);
serve(_Method, _Req, Opts) ->
    method_not_allowed(Opts).

sessions_enabled(Opts) ->
    maps:get(sessions, Opts, false).

%% GET (the SSE channel) and DELETE (teardown) exist only in session mode;
%% like POST they pass the security gate first.
session_verb(Req, Opts, Fun) ->
    case sessions_enabled(Opts) of
        true -> with_gate(Req, Opts, Fun);
        false -> method_not_allowed(Opts)
    end.

%% The security gate every verb passes: the DNS-rebinding `Origin` check,
%% then the optional auth hook. Either may short-circuit the request.
with_gate(Req, Opts, Fun) ->
    case check_origin(Req, Opts) of
        forbidden ->
            roadrunner_resp:forbidden();
        ok ->
            case check_auth(Req, Opts) of
                ok -> Fun();
                {reject, Status} -> roadrunner_resp:status(Status)
            end
    end.

%% Run the route's optional `auth` hook (a `fun/1` or `{Module, Function}`)
%% against the request. Absent => allow.
check_auth(Req, Opts) ->
    case maps:get(auth, Opts, undefined) of
        undefined -> ok;
        Hook when is_function(Hook, 1) -> Hook(Req);
        {Mod, Fun} -> Mod:Fun(Req)
    end.

method_not_allowed(Opts) ->
    Allow =
        case sessions_enabled(Opts) of
            true -> ~"POST, GET, DELETE";
            false -> ~"POST"
        end,
    roadrunner_resp:add_header(roadrunner_resp:status(405), ~"allow", Allow).

-doc """
Roadrunner loop `handle_info/3` callback for the two SSE loops:

- the **GET channel** (`#{session := Pid}`) -- forwards server-initiated events
  from the session; on client disconnect it detaches from the session (which
  keeps the session alive) and stops.
- the **streaming POST** (`#{mcp_post_stream := true}`) -- pushes the running
  tool's `notifications/progress` as they arrive, then the final result, then
  stops; a client disconnect just stops the loop.
""".
-spec handle_info(Info, Push, State) -> {ok, State} | {stop, State} when
    Info :: term(),
    Push :: roadrunner_handler:push_fun(),
    State :: #{session := pid()} | #{mcp_post_stream := true, _ => term()}.
handle_info({mcp_event, Frame}, Push, State) ->
    _ = Push(Frame),
    {ok, State};
handle_info({mcp_progress, Notification}, Push, State) ->
    _ = Push(message_frame(Notification)),
    {ok, State};
handle_info({mcp_result, Object}, Push, State) ->
    _ = Push(message_frame(Object)),
    {stop, State};
handle_info(mcp_cancelled, _Push, State) ->
    %% The session cancelled this streaming request; close the stream, no result.
    {stop, State};
%% The streaming-POST disconnect is checked first: its state may also carry
%% `session`, which would otherwise match the GET-channel clause below.
handle_info({roadrunner_disconnect, _Reason}, _Push, #{mcp_post_stream := true} = State) ->
    cancel_stream(State),
    {stop, State};
handle_info({roadrunner_disconnect, _Reason}, _Push, #{session := SessionPid} = State) ->
    ok = arizona_mcp_session:detach_channel(SessionPid, self()),
    {stop, State};
handle_info(_Info, _Push, State) ->
    {ok, State}.

%% On client disconnect, stop the running streaming tool: kill the stateless
%% worker, or ask the session to cancel the request it tracks.
cancel_stream(#{worker := Worker}) ->
    true = exit(Worker, kill),
    ok;
cancel_stream(#{session := SessionPid, id := Id}) ->
    arizona_mcp_session:cancel(SessionPid, Id);
cancel_stream(_State) ->
    ok.

%% A JSON-RPC message (a progress notification or the final result) framed as a
%% single SSE `message` event. The streaming POST is request-scoped, so -- unlike
%% the resumable GET channel -- the events carry no id.
message_frame(Object) ->
    roadrunner_sse:event(~"message", iolist_to_binary(json:encode(Object))).

-doc """
Pure MCP method dispatch. Maps a decoded JSON-RPC request and the app's
`arizona_mcp` module to one of:

- `{reply, Object}` -- a JSON-RPC response object to encode and return,
- `notification` -- an accepted notification with no reply (HTTP 202),
- `{error, Object}` -- a JSON-RPC error object to encode and return.

Exported for unit testing; `handle/1` calls it inside a crash guard.
""".
-spec dispatch(Mod, Request, Ctx) -> {reply, map()} | notification | {error, map()} when
    Mod :: module(),
    Request :: arizona_jsonrpc:request(),
    Ctx :: map().
dispatch(Mod, #{method := Method, params := Params, id := Id}, Ctx) ->
    PageSize = maps:get(page_size, Ctx, ?DEFAULT_PAGE_SIZE),
    case Id of
        %% A JSON-RPC notification (no id) is accepted and never answered,
        %% whatever its method -- `notifications/initialized` included.
        undefined ->
            notification;
        _ when Method =:= ~"initialize" ->
            {ServerInfo, #{caps := Caps}} = open_session(Mod, Params, PageSize),
            {reply, arizona_jsonrpc:result(Id, initialize_result(ServerInfo, Caps, Params))};
        _ ->
            %% Stateless: a fresh per-request session sourced from `init/1`.
            %% Nothing outlives the request, so the threaded-back session is
            %% discarded -- the next request rebuilds from `init/1`.
            {_ServerInfo, Session} = open_session(Mod, #{}, PageSize),
            {Outcome, _Session1} = handle_method(Method, Params, Id, Session),
            Outcome
    end.

%% Build a session from the handler's `init/1`: the server identity plus the
%% state, negotiated capabilities, and list page size the methods run against.
%% The session map is what `arizona_mcp_session` holds for a stateful
%% (`Mcp-Session-Id`) connection; `dispatch/3` builds an ephemeral one per
%% request.
-spec open_session(Mod, InitParams, PageSize) -> {ServerInfo, Session} when
    Mod :: module(),
    InitParams :: arizona_mcp:init_params(),
    PageSize :: pos_integer(),
    ServerInfo :: arizona_mcp:server_info(),
    Session :: session().
open_session(Mod, InitParams, PageSize) ->
    {ok, ServerInfo, Caps, State} = Mod:init(InitParams),
    Session = #{
        mod => Mod,
        state => State,
        caps => Caps,
        page_size => PageSize,
        log_min_severity => arizona_mcp:level_severity(?DEFAULT_LOG_LEVEL)
    },
    {ServerInfo, Session}.

%% Build the `initialize` result: negotiated protocol version, advertised
%% capabilities, and the server identity.
-spec initialize_result(ServerInfo, Caps, Params) -> map() when
    ServerInfo :: arizona_mcp:server_info(),
    Caps :: arizona_mcp:capabilities(),
    Params :: map().
initialize_result(ServerInfo, Caps, Params) ->
    #{
        ~"protocolVersion" => negotiate_version(Params),
        ~"capabilities" => Caps,
        ~"serverInfo" => server_info_json(ServerInfo)
    }.

%% --------------------------------------------------------------------
%% Internal functions - transport
%% --------------------------------------------------------------------

handle_post(Req, Opts) ->
    Body = iolist_to_binary(roadrunner_req:body(Req)),
    case arizona_jsonrpc:decode(Body) of
        {error, parse_error} ->
            json(arizona_jsonrpc:error(null, -32700, ~"Parse error"));
        {error, invalid_request} ->
            json(arizona_jsonrpc:error(null, -32600, ~"Invalid Request"));
        {ok, Request} ->
            serve_request(Request, Req, Opts)
    end.

%% A `tools/call` carrying a `_meta.progressToken` streams its POST as SSE (the
%% progress notifications, then the result, then close) -- but only when the
%% client accepts `text/event-stream`, the Streamable HTTP precondition for an
%% SSE response. Everything else (and a token-bearing call from a client that
%% does not accept SSE) is a buffered JSON reply. The streaming decision owns the
%% response *shape*, so it is made here before the buffered dispatch.
serve_request(Request, Req, Opts) ->
    case streaming_progress(Request) of
        {stream, Call} ->
            case accepts_sse(Req) of
                true -> serve_streaming(Call, Req, Opts);
                false -> serve_buffered(Request, Req, Opts)
            end;
        none ->
            serve_buffered(Request, Req, Opts)
    end.

%% The client opts into a streamed response by listing `text/event-stream` in
%% its `Accept`. Absent it, the server must answer with a buffered body.
accepts_sse(Req) ->
    case roadrunner_req:header(~"accept", Req) of
        undefined -> false;
        Accept -> binary:match(Accept, ~"text/event-stream") =/= nomatch
    end.

%% Recognise a well-formed token-bearing `tools/call`. A missing/non-binary
%% name or absent token falls through to the buffered path (which produces the
%% matching error); declared-ness is checked later, where the handler module is
%% in hand.
streaming_progress(#{method := ~"tools/call", id := Id, params := Params}) when
    Id =/= undefined
->
    case Params of
        #{~"name" := Name, ~"_meta" := #{~"progressToken" := Token}} when is_binary(Name) ->
            {stream, #{
                name => Name, args => maps:get(~"arguments", Params, #{}), token => Token, id => Id
            }};
        _ ->
            none
    end;
streaming_progress(_Request) ->
    none.

serve_buffered(Request, Req, Opts) ->
    case sessions_enabled(Opts) of
        false -> reply_stateless(Request, Opts);
        true -> reply_session(Request, Req, Opts)
    end.

%% A present `Origin` must be in the allowlist; an absent one is allowed
%% (non-browser clients send no Origin, and the rebinding attack is
%% browser-only). An empty allowlist rejects every browser origin -- an
%% app opts its origins in explicitly.
check_origin(Req, Opts) ->
    case roadrunner_req:header(~"origin", Req) of
        undefined ->
            ok;
        Origin ->
            case lists:member(Origin, maps:get(origins, Opts, [])) of
                true -> ok;
                false -> forbidden
            end
    end.

%% Stateless mode: every request is self-contained -- dispatch against a fresh
%% per-request session, encode. No `Mcp-Session-Id`.
reply_stateless(Request, #{handler := Mod} = Opts) ->
    reply_dispatch(Mod, Request, page_size(Opts)).

%% The configured list page size, defaulting when the route does not set it. A
%% `page_size` of zero/negative would make the cursor never advance (an empty
%% page that always re-issues the same cursor -- an infinite client loop), so a
%% misconfigured route fails loudly (a `function_clause`) rather than looping.
page_size(Opts) ->
    valid_page_size(maps:get(page_size, Opts, ?DEFAULT_PAGE_SIZE)).

valid_page_size(N) when is_integer(N), N >= 1 ->
    N.

%% Session mode: `initialize` opens a session and returns its id; every
%% other request is routed to the session named by `Mcp-Session-Id`.
reply_session(#{method := Method, params := Params, id := Id} = Request, Req, Opts) ->
    case Id of
        undefined ->
            session_notification(Method, Params, Req),
            roadrunner_resp:status(202);
        _ when Method =:= ~"initialize" ->
            session_initialize(Params, Id, Opts);
        _ ->
            session_request(Method, Params, Id, Request, Req, request_timeout(Opts))
    end.

%% A `notifications/cancelled {requestId}` cancels the in-flight streaming
%% request on the addressed session; any other notification is accepted with no
%% action. (Stateless mode has no session to route a cancel to.)
session_notification(~"notifications/cancelled", #{~"requestId" := RequestId}, Req) ->
    case roadrunner_req:header(~"mcp-session-id", Req) of
        undefined ->
            ok;
        SessionId ->
            case arizona_mcp_session_registry:lookup(SessionId) of
                {ok, Pid} -> arizona_mcp_session:cancel(Pid, RequestId);
                error -> ok
            end
    end;
session_notification(_Method, _Params, _Req) ->
    ok.

%% The configured buffered-request timeout.
request_timeout(Opts) ->
    maps:get(request_timeout_ms, Opts, ?DEFAULT_REQUEST_TIMEOUT_MS).

session_initialize(Params, Id, #{handler := Mod} = Opts) ->
    SessionId = generate_session_id(),
    %% A crashing `init/1` is guarded the same way the stateless path guards
    %% dispatch: answer `-32603` rather than killing the connection.
    try
        %% The session id is handed to the handler so it can address its own
        %% session later via `arizona_mcp:notify/3`. An atom key can't collide
        %% with the client's binary-keyed JSON params.
        InitParams = Params#{mcp_session_id => SessionId},
        {ServerInfo, #{caps := Caps} = Session} = open_session(Mod, InitParams, page_size(Opts)),
        SessionOpts = #{
            ttl_ms => maps:get(session_ttl_ms, Opts, ?DEFAULT_TTL_MS),
            buffer_max => maps:get(session_buffer_max, Opts, ?DEFAULT_BUFFER_MAX)
        },
        {ok, _Pid} = arizona_mcp_sup:start_session(SessionId, Session, SessionOpts),
        Result = arizona_jsonrpc:result(Id, initialize_result(ServerInfo, Caps, Params)),
        roadrunner_resp:add_header(json(Result), ~"mcp-session-id", SessionId)
    catch
        Class:Reason:Stacktrace ->
            logger:error("MCP initialize crashed: ~ts:~tp~n~tp", [Class, Reason, Stacktrace]),
            json(arizona_jsonrpc:error(Id, -32603, ~"Internal error"))
    end.

session_request(Method, Params, Id, Request, Req, Timeout) ->
    case roadrunner_req:header(~"mcp-session-id", Req) of
        undefined ->
            json(arizona_jsonrpc:error(reply_id(Request), -32600, ~"Missing Mcp-Session-Id"));
        SessionId ->
            with_session(
                SessionId,
                fun(Pid) ->
                    {_Tag, Object} = arizona_mcp_session:dispatch(Pid, Method, Params, Id, Timeout),
                    json(Object)
                end,
                fun() -> unknown_session(Id) end
            )
    end.

%% The 404 a session-scoped request gets when its `Mcp-Session-Id` names no live
%% session -- the client should re-initialize.
unknown_session(Id) ->
    roadrunner_resp:json(404, arizona_jsonrpc:error(Id, -32600, ~"Unknown or expired session")).

handle_delete(Req) ->
    case roadrunner_req:header(~"mcp-session-id", Req) of
        undefined ->
            roadrunner_resp:bad_request();
        SessionId ->
            with_session(
                SessionId,
                fun(Pid) ->
                    ok = arizona_mcp_session:stop(Pid),
                    roadrunner_resp:no_content()
                end,
                fun roadrunner_resp:not_found/0
            )
    end.

%% GET opens the server-to-client SSE channel: attach this loop process to
%% the named session, then stream events from it.
handle_get(Req) ->
    case roadrunner_req:header(~"mcp-session-id", Req) of
        undefined ->
            roadrunner_resp:bad_request();
        SessionId ->
            with_session(
                SessionId,
                fun(Pid) -> attach_or_reject(Pid, roadrunner_req:header(~"last-event-id", Req)) end,
                fun roadrunner_resp:not_found/0
            )
    end.

%% Resolve a session id and call into it. The registry lookup and the
%% subsequent gen_server call are not atomic, so a session that dies in the
%% gap surfaces as a `noproc`/`normal` exit -- treat that, like an unknown id,
%% as not-found rather than crashing the connection.
with_session(SessionId, Found, NotFound) ->
    case arizona_mcp_session_registry:lookup(SessionId) of
        {ok, Pid} ->
            try
                Found(Pid)
            catch
                exit:{noproc, _} -> NotFound();
                exit:{normal, _} -> NotFound()
            end;
        error ->
            NotFound()
    end.

%% Become the session's SSE channel (replaying any events newer than
%% `Last-Event-ID`), or `409` if one is already attached -- the spec allows a
%% single server-to-client stream per session.
attach_or_reject(Pid, LastEventId) ->
    case arizona_mcp_session:attach_channel(Pid, self(), LastEventId) of
        ok ->
            {loop, 200, sse_headers(), #{session => Pid}};
        {error, already_attached} ->
            roadrunner_resp:status(409)
    end.

sse_headers() ->
    [
        {~"content-type", ~"text/event-stream"},
        {~"cache-control", ~"no-cache"}
    ].

%% Stream a token-bearing `tools/call` over its POST as SSE. The tool runs off
%% this connection process (a worker in stateless mode, the session process in
%% session mode); it relays `notifications/progress` and the final result back
%% as messages, which the loop's `handle_info/3` pushes -- progress events
%% first, then the result, then stop.
serve_streaming(#{name := Name, args := Args, token := Token, id := Id}, Req, Opts) ->
    ConnPid = self(),
    case sessions_enabled(Opts) of
        false ->
            #{handler := Mod} = Opts,
            %% Stateless: a fresh per-request session; its threaded-back state
            %% is per-request and discarded. The worker frees the loop to push.
            %% page_size is irrelevant to a tools/call -- the default suffices.
            {_ServerInfo, Session} = open_session(Mod, #{}, ?DEFAULT_PAGE_SIZE),
            Ctx = #{token => Token, to => ConnPid},
            Worker = spawn(fun() ->
                _Session1 = run_streaming_tool(Session, Name, Args, Id, Ctx, ConnPid)
            end),
            %% Track the worker so a client disconnect kills it.
            stream_loop(#{mcp_post_stream => true, worker => Worker});
        true ->
            case roadrunner_req:header(~"mcp-session-id", Req) of
                undefined ->
                    json(arizona_jsonrpc:error(Id, -32600, ~"Missing Mcp-Session-Id"));
                SessionId ->
                    with_session(
                        SessionId,
                        fun(Pid) ->
                            ok = arizona_mcp_session:start_streaming_tool(
                                Pid, Name, Args, Id, Token, ConnPid
                            ),
                            %% Track the session + request id so a disconnect
                            %% cancels it (a `notifications/cancelled` arrives on
                            %% its own POST instead).
                            stream_loop(#{mcp_post_stream => true, session => Pid, id => Id})
                        end,
                        fun() -> unknown_session(Id) end
                    )
            end
    end.

%% The streaming-POST loop: a request-scoped stream (no resumability), tagged so
%% `handle_info/3` distinguishes it from the GET channel. The state also carries
%% what to cancel on disconnect.
stream_loop(LoopState) ->
    {loop, 200, sse_headers(), LoopState}.

-doc """
Run a streaming `tools/call` against `Session`, relaying the result (and, via
`Ctx`, any `notifications/progress`) to `ConnPid`, and return the session
carrying the tool's threaded-back state. Shared by the stateless worker and the
session process; exported for the latter's cross-module call.
""".
-spec run_streaming_tool(Session, Name, Args, Id, Ctx, ConnPid) -> Session when
    Session :: session(),
    Name :: binary(),
    Args :: map(),
    Id :: arizona_jsonrpc:id(),
    Ctx :: arizona_mcp:progress_ctx(),
    ConnPid :: pid().
run_streaming_tool(#{state := State} = Session, Name, Args, Id, Ctx, ConnPid) ->
    %% Guard the whole run (the `tools/1` read included), since the session
    %% process calls this in `handle_cast`: a crash must answer -32603 and leave
    %% the prior state intact, never kill the session.
    {Object, NewState} =
        try
            run_tool(Session, Name, Args, Id, Ctx)
        catch
            Class:Reason:Stacktrace ->
                logger:error(
                    "MCP streaming tool crashed: ~ts:~tp~n~tp", [Class, Reason, Stacktrace]
                ),
                {arizona_jsonrpc:error(Id, -32603, ~"Internal error"), State}
        end,
    ConnPid ! {mcp_result, Object},
    Session#{state := NewState}.

%% Validate the tool name, run it, and shape the outcome into a
%% `{ResultObject, NewState}` pair. Unknown name -> a -32602 result with the
%% prior state; a declared tool -> its result and threaded-back state.
run_tool(#{mod := Mod, state := State}, Name, Args, Id, Ctx) ->
    case lists:member(Name, [N || #{name := N} <- Mod:tools(State)]) of
        false ->
            {arizona_jsonrpc:error(Id, -32602, <<"Unknown tool: ", Name/binary>>), State};
        true ->
            stream_tool_outcome(Mod:handle_tool(Name, Args, Ctx, State), Id)
    end.

stream_tool_outcome({reply, Result, NewState}, Id) ->
    {arizona_jsonrpc:result(Id, tool_content(Result, false)), NewState};
stream_tool_outcome({error, ToolError, NewState}, Id) ->
    {arizona_jsonrpc:result(Id, tool_content(ToolError, true)), NewState}.

%% A URL-safe, unguessable session id: 128 bits of CSPRNG output as hex.
generate_session_id() ->
    binary:encode_hex(crypto:strong_rand_bytes(16), lowercase).

reply_dispatch(Mod, Request, PageSize) ->
    try dispatch(Mod, Request, #{page_size => PageSize}) of
        {reply, Object} -> json(Object);
        {error, Object} -> json(Object);
        notification -> roadrunner_resp:status(202)
    catch
        Class:Reason:Stacktrace ->
            logger:error("MCP dispatch crashed: ~ts:~tp~n~tp", [Class, Reason, Stacktrace]),
            json(arizona_jsonrpc:error(reply_id(Request), -32603, ~"Internal error"))
    end.

%% A crash mid-dispatch still owes the request a reply; a notification has
%% no id, so answer with the JSON-RPC `null` id.
reply_id(#{id := undefined}) -> null;
reply_id(#{id := Id}) -> Id.

%% JSON-RPC responses -- including error objects -- ride HTTP 200; the
%% fault is carried in the JSON-RPC envelope, not the HTTP status.
json(Object) ->
    roadrunner_resp:json(200, Object).

%% --------------------------------------------------------------------
%% Internal functions - MCP methods
%% --------------------------------------------------------------------

-doc """
Run one non-initialize MCP method against a session, returning the JSON-RPC
outcome paired with the (possibly updated) session. `arizona_mcp_session`
calls this with its held session and stores the returned session back, so a
stateful callback's new state (`handle_tool` / `read_resource` /
`get_prompt`) carries into the session's later requests; `dispatch/3`
(stateless) discards the returned session. Methods that run no stateful
callback (`ping`, the `*/list` reads, an unknown or capability-gated-off
method) return the session unchanged.
""".
-spec handle_method(Method, Params, Id, Session) -> {Outcome, Session} when
    Method :: binary(),
    Params :: map(),
    Id :: arizona_jsonrpc:id(),
    Session :: session(),
    Outcome :: {reply, map()} | {error, map()}.
handle_method(~"ping", _Params, Id, Session) ->
    {{reply, arizona_jsonrpc:result(Id, #{})}, Session};
handle_method(~"tools/list", Params, Id, #{mod := Mod, state := State} = Session) ->
    list_reply(Mod:tools(State), fun tool_json/1, ~"tools", Params, Id, Session);
handle_method(~"tools/call", Params, Id, Session) ->
    call_tool(Params, Id, Session);
handle_method(~"resources/list", Params, Id, Session) ->
    capability(resources, Id, Session, fun(#{mod := Mod, state := State} = S) ->
        list_reply(Mod:resources(State), fun resource_json/1, ~"resources", Params, Id, S)
    end);
handle_method(~"resources/read", Params, Id, Session) ->
    capability(resources, Id, Session, fun(S) -> read_resource(Params, Id, S) end);
handle_method(~"resources/templates/list", Params, Id, Session) ->
    capability(resources, Id, Session, fun(#{mod := Mod, state := State} = S) ->
        Templates = resource_templates(Mod, State),
        list_reply(Templates, fun resource_template_json/1, ~"resourceTemplates", Params, Id, S)
    end);
handle_method(~"resources/subscribe", Params, Id, Session) ->
    capability(resources, Id, Session, fun(S) -> subscribe_resource(Params, Id, S) end);
handle_method(~"resources/unsubscribe", Params, Id, Session) ->
    capability(resources, Id, Session, fun(S) -> unsubscribe_resource(Params, Id, S) end);
handle_method(~"prompts/list", Params, Id, Session) ->
    capability(prompts, Id, Session, fun(#{mod := Mod, state := State} = S) ->
        list_reply(Mod:prompts(State), fun prompt_json/1, ~"prompts", Params, Id, S)
    end);
handle_method(~"prompts/get", Params, Id, Session) ->
    capability(prompts, Id, Session, fun(S) -> get_prompt(Params, Id, S) end);
handle_method(~"logging/setLevel", Params, Id, Session) ->
    capability(logging, Id, Session, fun(S) -> set_level(Params, Id, S) end);
handle_method(~"completion/complete", Params, Id, Session) ->
    capability(completions, Id, Session, fun(S) -> complete(Params, Id, S) end);
handle_method(_Method, _Params, Id, Session) ->
    method_not_found(Id, Session).

%% Paginate a full list by the request's opaque `cursor`, map the page to wire
%% JSON under `Key`, and add `nextCursor` when more remain. A malformed cursor
%% is a -32602. The app's list callback is unchanged -- it returns the full
%% list and the transport owns the slicing.
list_reply(Full, ItemFun, Key, Params, Id, #{page_size := PageSize} = Session) ->
    case paginate(Full, Params, PageSize) of
        {ok, Page, Next} ->
            Items = [ItemFun(Item) || Item <- Page],
            Result = maybe_next_cursor(Next, #{Key => Items}),
            {{reply, arizona_jsonrpc:result(Id, Result)}, Session};
        error ->
            {{error, arizona_jsonrpc:error(Id, -32602, ~"Invalid cursor")}, Session}
    end.

%% Slice `Full` at the cursor's offset for `PageSize` items, returning the page
%% and the next cursor (`undefined` once exhausted). A bad cursor -> `error`. An
%% offset past the end (a stale or tampered cursor) yields an empty final page
%% rather than crashing `lists:sublist/3`.
paginate(Full, Params, PageSize) ->
    case cursor_offset(Params) of
        {ok, Offset} ->
            Len = length(Full),
            Page =
                case Offset < Len of
                    true -> lists:sublist(Full, Offset + 1, PageSize);
                    false -> []
                end,
            Next =
                case Offset + PageSize < Len of
                    true -> encode_cursor(Offset + PageSize);
                    false -> undefined
                end,
            {ok, Page, Next};
        error ->
            error
    end.

%% Absent cursor -> page from the start; present -> decode it (a non-binary or
%% malformed cursor is rejected).
cursor_offset(#{~"cursor" := Cursor}) when is_binary(Cursor) ->
    decode_cursor(Cursor);
cursor_offset(#{~"cursor" := _}) ->
    error;
cursor_offset(_Params) ->
    {ok, 0}.

maybe_next_cursor(undefined, Result) -> Result;
maybe_next_cursor(Next, Result) -> Result#{~"nextCursor" => Next}.

%% Cursors are opaque to the client: a base64 of the integer offset.
encode_cursor(Offset) ->
    base64:encode(integer_to_binary(Offset)).

decode_cursor(Cursor) ->
    try binary_to_integer(base64:decode(Cursor)) of
        Offset when Offset >= 0 -> {ok, Offset};
        _ -> error
    catch
        _:_ -> error
    end.

%% An optional capability (resources/prompts) is served only when the server
%% advertised it at initialize; otherwise the method does not exist. The gated
%% path runs no callback, so it returns the session unchanged.
capability(Capability, Id, #{caps := Caps} = Session, Fun) ->
    case Caps of
        #{Capability := _} -> Fun(Session);
        _ -> method_not_found(Id, Session)
    end.

%% The shared "method not found" outcome: an unknown method, or a method
%% gated behind an unadvertised capability. No callback ran, so the session
%% threads back unchanged.
method_not_found(Id, Session) ->
    {{error, arizona_jsonrpc:error(Id, -32601, ~"Method not found")}, Session}.

%% Resolve a required string param against the list of declared names, then
%% dispatch, threading the session through. Only the `Found` branch can mutate
%% state -- a missing param (`Missing`, a `-32602` producer) or an undeclared
%% value (`NotFound`) ran no callback, so they pair their error with the
%% unchanged `Session`. Shared by tools/call, resources/read, prompts/get.
with_member(Params, Key, Declared, Session, Missing, NotFound, Found) ->
    case Params of
        #{Key := Value} when is_binary(Value) ->
            case lists:member(Value, Declared) of
                true -> Found(Value);
                false -> {{error, NotFound(Value)}, Session}
            end;
        _ ->
            {{error, Missing()}, Session}
    end.

call_tool(Params, Id, #{mod := Mod, state := State} = Session) ->
    with_member(
        Params,
        ~"name",
        [N || #{name := N} <- Mod:tools(State)],
        Session,
        fun() -> arizona_jsonrpc:error(Id, -32602, ~"Invalid params: missing tool name") end,
        fun(Name) -> arizona_jsonrpc:error(Id, -32602, <<"Unknown tool: ", Name/binary>>) end,
        fun(Name) ->
            Args = maps:get(~"arguments", Params, #{}),
            tool_reply(Mod:handle_tool(Name, Args, inert_ctx(), State), Id, Session)
        end
    ).

%% The progress context for a buffered (non-streaming) `tools/call`: inert, so
%% `arizona_mcp:progress/2,3` is a no-op. Streaming calls build a live context
%% in `serve_streaming/3` instead.
inert_ctx() ->
    #{token => undefined, to => undefined}.

read_resource(Params, Id, #{mod := Mod, state := State} = Session) ->
    with_member(
        Params,
        ~"uri",
        [U || #{uri := U} <- Mod:resources(State)],
        Session,
        fun() -> arizona_jsonrpc:error(Id, -32602, ~"Invalid params: missing resource uri") end,
        fun(Uri) -> arizona_jsonrpc:error(Id, -32002, <<"Resource not found: ", Uri/binary>>) end,
        fun(Uri) -> resource_contents(Mod:read_resource(Uri, State), Uri, Id, Session) end
    ).

%% The optional `resource_templates/1` callback; absent means no parameterized
%% resources, so the transport advertises an empty template list.
resource_templates(Mod, State) ->
    case erlang:function_exported(Mod, resource_templates, 1) of
        true -> Mod:resource_templates(State);
        false -> []
    end.

%% `resources/subscribe` / `unsubscribe` join/leave the per-uri pubsub channel as
%% the calling process. In session mode that is the session process, so the
%% subscription persists and `arizona_mcp:resource_updated/1` broadcasts reach it
%% (forwarded as `notifications/resources/updated`); in stateless mode the request
%% process dies after the reply -- a harmless no-op, since stateless has no SSE
%% channel to deliver on. Both answer the MCP empty result.
subscribe_resource(Params, Id, Session) ->
    resource_subscription(Params, Id, Session, fun join_resource/1).

unsubscribe_resource(Params, Id, Session) ->
    resource_subscription(Params, Id, Session, fun leave_resource/1).

resource_subscription(#{~"uri" := Uri}, Id, Session, Fun) when is_binary(Uri) ->
    ok = Fun({mcp_resource, Uri}),
    {{reply, arizona_jsonrpc:result(Id, #{})}, Session};
resource_subscription(_Params, Id, Session, _Fun) ->
    {{error, arizona_jsonrpc:error(Id, -32602, ~"Invalid params: missing resource uri")}, Session}.

join_resource(Channel) ->
    case arizona_pubsub:subscribe(Channel, self()) of
        ok -> ok;
        {error, already_joined} -> ok
    end.

leave_resource(Channel) ->
    case arizona_pubsub:unsubscribe(Channel, self()) of
        ok -> ok;
        {error, not_joined} -> ok
    end.

get_prompt(Params, Id, #{mod := Mod, state := State} = Session) ->
    with_member(
        Params,
        ~"name",
        [N || #{name := N} <- Mod:prompts(State)],
        Session,
        fun() -> arizona_jsonrpc:error(Id, -32602, ~"Invalid params: missing prompt name") end,
        fun(Name) -> arizona_jsonrpc:error(Id, -32602, <<"Unknown prompt: ", Name/binary>>) end,
        fun(Name) ->
            Args = maps:get(~"arguments", Params, #{}),
            prompt_messages(Mod:get_prompt(Name, Args, State), Id, Session)
        end
    ).

%% `logging/setLevel` stores the client's minimum severity in the session, so a
%% later `log/3` below it is dropped. A bad or missing level is a -32602.
set_level(#{~"level" := LevelBin}, Id, Session) when is_binary(LevelBin) ->
    case arizona_mcp:parse_level(LevelBin) of
        {ok, Level} ->
            Session1 = Session#{log_min_severity := arizona_mcp:level_severity(Level)},
            {{reply, arizona_jsonrpc:result(Id, #{})}, Session1};
        error ->
            {
                {error,
                    arizona_jsonrpc:error(Id, -32602, <<"Invalid log level: ", LevelBin/binary>>)},
                Session
            }
    end;
set_level(_Params, Id, Session) ->
    {{error, arizona_jsonrpc:error(Id, -32602, ~"Invalid params: missing level")}, Session}.

%% `completion/complete` decodes the ref + argument, runs the app's `complete/3`,
%% caps the values at the MCP limit of 100, and reports `hasMore`. A malformed
%% ref or argument is a -32602.
complete(#{~"ref" := Ref, ~"argument" := Arg}, Id, #{mod := Mod, state := State} = Session) ->
    case {decode_ref(Ref), decode_arg(Arg)} of
        {{ok, DecRef}, {ok, DecArg}} ->
            {reply, Values, NewState} = Mod:complete(DecRef, DecArg, State),
            Result = completion_result(Values),
            {{reply, arizona_jsonrpc:result(Id, Result)}, Session#{state := NewState}};
        _ ->
            {
                {error, arizona_jsonrpc:error(Id, -32602, ~"Invalid completion ref or argument")},
                Session
            }
    end;
complete(_Params, Id, Session) ->
    {
        {error, arizona_jsonrpc:error(Id, -32602, ~"Invalid params: missing ref or argument")},
        Session
    }.

decode_ref(#{~"type" := ~"ref/prompt", ~"name" := Name}) when is_binary(Name) ->
    {ok, {prompt, Name}};
decode_ref(#{~"type" := ~"ref/resource", ~"uri" := Uri}) when is_binary(Uri) ->
    {ok, {resource, Uri}};
decode_ref(_Ref) ->
    error.

decode_arg(#{~"name" := Name, ~"value" := Value}) when is_binary(Name), is_binary(Value) ->
    {ok, {Name, Value}};
decode_arg(_Arg) ->
    error.

completion_result(Values) ->
    Capped = lists:sublist(Values, 100),
    #{~"completion" => #{~"values" => Capped, ~"hasMore" => length(Values) > 100}}.

%% The callback ran -- thread its returned state back into the session whether
%% it succeeded or failed in-band, since a failed-but-ran tool may have
%% legitimately mutated state. Both kinds become a `tools/call` result; the
%% MCP `isError` flag inside distinguishes them.
tool_reply({reply, Result, NewState}, Id, Session) ->
    reply_with_state(tool_content(Result, false), Id, Session, NewState);
tool_reply({error, ToolError, NewState}, Id, Session) ->
    reply_with_state(tool_content(ToolError, true), Id, Session, NewState).

%% Wrap a result object as a `{reply, _}` outcome paired with the session
%% carrying the callback's returned state.
reply_with_state(ResultObject, Id, Session, NewState) ->
    {{reply, arizona_jsonrpc:result(Id, ResultObject)}, Session#{state := NewState}}.

%% --------------------------------------------------------------------
%% Internal functions - wire shaping
%% --------------------------------------------------------------------

negotiate_version(#{~"protocolVersion" := Version}) when is_binary(Version) ->
    case lists:member(Version, ?SUPPORTED_VERSIONS) of
        true -> Version;
        false -> ?PREFERRED_VERSION
    end;
negotiate_version(_) ->
    ?PREFERRED_VERSION.

%% Copy an optional key from a source map onto the wire map under its wire
%% name, or leave the wire map unchanged when the source key is absent.
maybe_put(WireKey, SourceKey, Source, Map) ->
    case Source of
        #{SourceKey := Value} -> Map#{WireKey => Value};
        _ -> Map
    end.

server_info_json(#{name := Name, version := Version} = ServerInfo) ->
    Base = #{~"name" => Name, ~"version" => Version},
    maybe_put(~"title", title, ServerInfo, Base).

tool_json(#{name := Name, description := Description, input_schema := Schema} = Tool) ->
    Base = #{~"name" => Name, ~"description" => Description, ~"inputSchema" => Schema},
    maybe_put(~"title", title, Tool, Base).

%% A bare binary is one text content block. A map provides its own content
%% blocks (encoded verbatim) and may carry structured content. Either way
%% the MCP `isError` flag distinguishes success from an in-band tool error.
tool_content(Bin, IsError) when is_binary(Bin) ->
    #{~"content" => [#{~"type" => ~"text", ~"text" => Bin}], ~"isError" => IsError};
tool_content(#{content := Content} = Result, IsError) ->
    Base = #{~"content" => Content, ~"isError" => IsError},
    maybe_put(~"structuredContent", structured_content, Result, Base).

resource_json(#{uri := Uri, name := Name} = Resource) ->
    Base = #{~"uri" => Uri, ~"name" => Name},
    WithDescription = maybe_put(~"description", description, Resource, Base),
    maybe_put(~"mimeType", mime_type, Resource, WithDescription).

resource_template_json(#{uri_template := UriTemplate, name := Name} = Template) ->
    Base = #{~"uriTemplate" => UriTemplate, ~"name" => Name},
    WithDescription = maybe_put(~"description", description, Template, Base),
    maybe_put(~"mimeType", mime_type, Template, WithDescription).

%% A bare binary becomes one text entry keyed by the requested uri; a map
%% supplies its content entries verbatim. An `{error, _}` is a -32002. Either
%% way the callback ran, so its returned state threads back into the session.
resource_contents({reply, Text, NewState}, Uri, Id, Session) when is_binary(Text) ->
    Contents = [#{~"uri" => Uri, ~"text" => Text}],
    reply_with_state(#{~"contents" => Contents}, Id, Session, NewState);
resource_contents({reply, #{contents := Contents}, NewState}, _Uri, Id, Session) ->
    reply_with_state(#{~"contents" => Contents}, Id, Session, NewState);
resource_contents({error, Message, NewState}, _Uri, Id, Session) when is_binary(Message) ->
    {{error, arizona_jsonrpc:error(Id, -32002, Message)}, Session#{state := NewState}}.

prompt_json(#{name := Name} = Prompt) ->
    Base = #{~"name" => Name},
    WithDescription = maybe_put(~"description", description, Prompt, Base),
    maybe_put(~"arguments", arguments, Prompt, WithDescription).

prompt_messages({reply, #{messages := Messages} = Result, NewState}, Id, Session) ->
    Base = #{~"messages" => Messages},
    Object = maybe_put(~"description", description, Result, Base),
    reply_with_state(Object, Id, Session, NewState);
prompt_messages({error, Message, NewState}, Id, Session) when is_binary(Message) ->
    {{error, arizona_jsonrpc:error(Id, -32602, Message)}, Session#{state := NewState}}.
