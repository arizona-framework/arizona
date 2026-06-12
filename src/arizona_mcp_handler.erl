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

%% The dispatch context: the handler module, its state, and the capabilities
%% negotiated at initialize. `arizona_mcp_session` holds one across a
%% stateful connection; `dispatch/3` builds an ephemeral one per request.
-type session() :: #{
    mod := module(),
    state := arizona_mcp:state(),
    caps := arizona_mcp:capabilities()
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
Roadrunner loop `handle_info/3` callback for the GET SSE channel. Forwards
server-initiated events from the session to the wire; on client disconnect
it detaches the channel from the session (which keeps the session alive) and
stops the loop.
""".
-spec handle_info(Info, Push, State) -> {ok, State} | {stop, State} when
    Info :: term(),
    Push :: roadrunner_handler:push_fun(),
    State :: #{session := pid()}.
handle_info({mcp_event, Frame}, Push, State) ->
    _ = Push(Frame),
    {ok, State};
handle_info({roadrunner_disconnect, _Reason}, _Push, #{session := SessionPid} = State) ->
    ok = arizona_mcp_session:detach_channel(SessionPid, self()),
    {stop, State};
handle_info(_Info, _Push, State) ->
    {ok, State}.

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
dispatch(Mod, #{method := Method, params := Params, id := Id}, _Ctx) ->
    case Id of
        %% A JSON-RPC notification (no id) is accepted and never answered,
        %% whatever its method -- `notifications/initialized` included.
        undefined ->
            notification;
        _ when Method =:= ~"initialize" ->
            {ServerInfo, #{caps := Caps}} = open_session(Mod, Params),
            {reply, arizona_jsonrpc:result(Id, initialize_result(ServerInfo, Caps, Params))};
        _ ->
            %% Stateless: a fresh per-request session sourced from `init/1`.
            %% Nothing outlives the request, so the threaded-back session is
            %% discarded -- the next request rebuilds from `init/1`.
            {_ServerInfo, Session} = open_session(Mod, #{}),
            {Outcome, _Session1} = handle_method(Method, Params, Id, Session),
            Outcome
    end.

%% Build a session from the handler's `init/1`: the server identity plus the
%% state and negotiated capabilities the methods run against. The session map
%% is what `arizona_mcp_session` holds for a stateful (`Mcp-Session-Id`)
%% connection; `dispatch/3` builds an ephemeral one per request.
-spec open_session(Mod, InitParams) -> {ServerInfo, Session} when
    Mod :: module(),
    InitParams :: arizona_mcp:init_params(),
    ServerInfo :: arizona_mcp:server_info(),
    Session :: session().
open_session(Mod, InitParams) ->
    {ok, ServerInfo, Caps, State} = Mod:init(InitParams),
    {ServerInfo, #{mod => Mod, state => State, caps => Caps}}.

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
    case sessions_enabled(Opts) of
        false -> reply_stateless(Body, Opts);
        true -> reply_session(Body, Req, Opts)
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

%% Stateless mode: every request is self-contained -- decode, dispatch
%% against a fresh per-request session, encode. No `Mcp-Session-Id`.
reply_stateless(Body, Opts) ->
    case arizona_jsonrpc:decode(Body) of
        {error, parse_error} ->
            json(arizona_jsonrpc:error(null, -32700, ~"Parse error"));
        {error, invalid_request} ->
            json(arizona_jsonrpc:error(null, -32600, ~"Invalid Request"));
        {ok, Request} ->
            #{handler := Mod} = Opts,
            reply_dispatch(Mod, Request)
    end.

%% Session mode: `initialize` opens a session and returns its id; every
%% other request is routed to the session named by `Mcp-Session-Id`.
reply_session(Body, Req, Opts) ->
    case arizona_jsonrpc:decode(Body) of
        {error, parse_error} ->
            json(arizona_jsonrpc:error(null, -32700, ~"Parse error"));
        {error, invalid_request} ->
            json(arizona_jsonrpc:error(null, -32600, ~"Invalid Request"));
        {ok, #{method := Method, params := Params, id := Id} = Request} ->
            case Id of
                undefined -> roadrunner_resp:status(202);
                _ when Method =:= ~"initialize" -> session_initialize(Params, Id, Opts);
                _ -> session_request(Method, Params, Id, Request, Req)
            end
    end.

session_initialize(Params, Id, #{handler := Mod} = Opts) ->
    SessionId = generate_session_id(),
    %% A crashing `init/1` is guarded the same way the stateless path guards
    %% dispatch: answer `-32603` rather than killing the connection.
    try
        %% The session id is handed to the handler so it can address its own
        %% session later via `arizona_mcp:notify/3`. An atom key can't collide
        %% with the client's binary-keyed JSON params.
        InitParams = Params#{mcp_session_id => SessionId},
        {ServerInfo, #{caps := Caps} = Session} = open_session(Mod, InitParams),
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

session_request(Method, Params, Id, Request, Req) ->
    case roadrunner_req:header(~"mcp-session-id", Req) of
        undefined ->
            json(arizona_jsonrpc:error(reply_id(Request), -32600, ~"Missing Mcp-Session-Id"));
        SessionId ->
            with_session(
                SessionId,
                fun(Pid) ->
                    {_Tag, Object} = arizona_mcp_session:dispatch(Pid, Method, Params, Id),
                    json(Object)
                end,
                %% Unknown or expired session: the client should re-initialize.
                fun() ->
                    roadrunner_resp:json(
                        404, arizona_jsonrpc:error(Id, -32600, ~"Unknown or expired session")
                    )
                end
            )
    end.

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
            Headers = [
                {~"content-type", ~"text/event-stream"},
                {~"cache-control", ~"no-cache"}
            ],
            {loop, 200, Headers, #{session => Pid}};
        {error, already_attached} ->
            roadrunner_resp:status(409)
    end.

%% A URL-safe, unguessable session id: 128 bits of CSPRNG output as hex.
generate_session_id() ->
    binary:encode_hex(crypto:strong_rand_bytes(16), lowercase).

reply_dispatch(Mod, Request) ->
    try dispatch(Mod, Request, #{}) of
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
handle_method(~"tools/list", _Params, Id, #{mod := Mod, state := State} = Session) ->
    Tools = [tool_json(Tool) || Tool <- Mod:tools(State)],
    {{reply, arizona_jsonrpc:result(Id, #{~"tools" => Tools})}, Session};
handle_method(~"tools/call", Params, Id, Session) ->
    call_tool(Params, Id, Session);
handle_method(~"resources/list", _Params, Id, Session) ->
    capability(resources, Id, Session, fun(#{mod := Mod, state := State} = S) ->
        Resources = [resource_json(Resource) || Resource <- Mod:resources(State)],
        {{reply, arizona_jsonrpc:result(Id, #{~"resources" => Resources})}, S}
    end);
handle_method(~"resources/read", Params, Id, Session) ->
    capability(resources, Id, Session, fun(S) -> read_resource(Params, Id, S) end);
handle_method(~"prompts/list", _Params, Id, Session) ->
    capability(prompts, Id, Session, fun(#{mod := Mod, state := State} = S) ->
        Prompts = [prompt_json(Prompt) || Prompt <- Mod:prompts(State)],
        {{reply, arizona_jsonrpc:result(Id, #{~"prompts" => Prompts})}, S}
    end);
handle_method(~"prompts/get", Params, Id, Session) ->
    capability(prompts, Id, Session, fun(S) -> get_prompt(Params, Id, S) end);
handle_method(_Method, _Params, Id, Session) ->
    method_not_found(Id, Session).

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
            tool_reply(Mod:handle_tool(Name, Args, State), Id, Session)
        end
    ).

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
