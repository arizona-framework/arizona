-module(arizona_mcp_handler).
-moduledoc """
Roadrunner handler for an MCP (Model Context Protocol) route.

Implements MCP's Streamable HTTP transport for a tools server: a client
POSTs a single JSON-RPC 2.0 message and receives a buffered
`application/json` reply. `GET`/`DELETE` return `405` (no server-to-client
stream in this phase). Before any dispatch the request's `Origin` is
checked against the route's allowlist (DNS-rebinding defense).

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
-export([dispatch/3]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

%% `dispatch/3` is called by `handle/1` and exercised directly from the
%% test suite; it has no cross-module caller in the release.
-ignore_xref([dispatch/3]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------

%% Protocol revisions this server speaks. The client's requested version
%% is echoed when supported, else the preferred one is offered.
-define(PREFERRED_VERSION, ~"2025-11-25").
-define(SUPPORTED_VERSIONS, [~"2025-11-25", ~"2025-06-18", ~"2025-03-26"]).

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc """
Roadrunner `handle/1` callback. Serves the MCP endpoint: a JSON-RPC POST
returns a buffered `application/json` reply; other methods return `405`.
""".
-spec handle(Req) -> {Response, Req} when
    Req :: roadrunner_req:request(),
    Response :: roadrunner_handler:response().
handle(Req) ->
    #{arizona := Opts} = roadrunner_req:state(Req),
    case roadrunner_req:method(Req) of
        ~"POST" ->
            {handle_post(Req, Opts), Req};
        _ ->
            {roadrunner_resp:add_header(roadrunner_resp:status(405), ~"allow", ~"POST"), Req}
    end.

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
        undefined -> notification;
        _ -> request(Method, Params, Id, Mod)
    end.

%% --------------------------------------------------------------------
%% Internal functions - transport
%% --------------------------------------------------------------------

handle_post(Req, Opts) ->
    case check_origin(Req, Opts) of
        ok ->
            Body = iolist_to_binary(roadrunner_req:body(Req)),
            reply(Body, Opts);
        forbidden ->
            roadrunner_resp:forbidden()
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

reply(Body, Opts) ->
    case arizona_jsonrpc:decode(Body) of
        {error, parse_error} ->
            json(arizona_jsonrpc:error(null, -32700, ~"Parse error"));
        {error, invalid_request} ->
            json(arizona_jsonrpc:error(null, -32600, ~"Invalid Request"));
        {ok, Request} ->
            #{handler := Mod} = Opts,
            reply_dispatch(Mod, Request)
    end.

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

request(~"initialize", Params, Id, Mod) ->
    {ok, ServerInfo, Capabilities, _State} = Mod:init(Params),
    Result = #{
        ~"protocolVersion" => negotiate_version(Params),
        ~"capabilities" => Capabilities,
        ~"serverInfo" => server_info_json(ServerInfo)
    },
    {reply, arizona_jsonrpc:result(Id, Result)};
request(~"ping", _Params, Id, _Mod) ->
    {reply, arizona_jsonrpc:result(Id, #{})};
request(~"tools/list", _Params, Id, Mod) ->
    {ok, _ServerInfo, _Capabilities, State} = Mod:init(#{}),
    Tools = [tool_json(Tool) || Tool <- Mod:tools(State)],
    {reply, arizona_jsonrpc:result(Id, #{~"tools" => Tools})};
request(~"tools/call", Params, Id, Mod) ->
    call_tool(Params, Id, Mod);
request(~"resources/list", _Params, Id, Mod) ->
    with_capability(resources, Mod, Id, fun(State) ->
        Resources = [resource_json(Resource) || Resource <- Mod:resources(State)],
        {reply, arizona_jsonrpc:result(Id, #{~"resources" => Resources})}
    end);
request(~"resources/read", Params, Id, Mod) ->
    with_capability(resources, Mod, Id, fun(State) -> read_resource(Params, Id, Mod, State) end);
request(~"prompts/list", _Params, Id, Mod) ->
    with_capability(prompts, Mod, Id, fun(State) ->
        Prompts = [prompt_json(Prompt) || Prompt <- Mod:prompts(State)],
        {reply, arizona_jsonrpc:result(Id, #{~"prompts" => Prompts})}
    end);
request(~"prompts/get", Params, Id, Mod) ->
    with_capability(prompts, Mod, Id, fun(State) -> get_prompt(Params, Id, Mod, State) end);
request(_Method, _Params, Id, _Mod) ->
    {error, arizona_jsonrpc:error(Id, -32601, ~"Method not found")}.

%% An optional capability (resources/prompts) is served only when the
%% server advertised it in `init/1`; otherwise the method does not exist.
with_capability(Capability, Mod, Id, Fun) ->
    {ok, _ServerInfo, Capabilities, State} = Mod:init(#{}),
    case Capabilities of
        #{Capability := _} -> Fun(State);
        _ -> {error, arizona_jsonrpc:error(Id, -32601, ~"Method not found")}
    end.

%% Resolve a required string param against the list of declared names, then
%% dispatch. A missing or non-binary param runs `Missing` (a `-32602` error
%% producer); a value absent from `Declared` runs `NotFound`; a declared
%% value runs `Found`. Shared by tools/call, resources/read, prompts/get.
with_member(Params, Key, Declared, Missing, NotFound, Found) ->
    case Params of
        #{Key := Value} when is_binary(Value) ->
            case lists:member(Value, Declared) of
                true -> Found(Value);
                false -> {error, NotFound(Value)}
            end;
        _ ->
            {error, Missing()}
    end.

call_tool(Params, Id, Mod) ->
    {ok, _ServerInfo, _Capabilities, State} = Mod:init(#{}),
    with_member(
        Params,
        ~"name",
        [N || #{name := N} <- Mod:tools(State)],
        fun() -> arizona_jsonrpc:error(Id, -32602, ~"Invalid params: missing tool name") end,
        fun(Name) -> arizona_jsonrpc:error(Id, -32602, <<"Unknown tool: ", Name/binary>>) end,
        fun(Name) ->
            Args = maps:get(~"arguments", Params, #{}),
            tool_reply(Mod:handle_tool(Name, Args, State), Id)
        end
    ).

read_resource(Params, Id, Mod, State) ->
    with_member(
        Params,
        ~"uri",
        [U || #{uri := U} <- Mod:resources(State)],
        fun() -> arizona_jsonrpc:error(Id, -32602, ~"Invalid params: missing resource uri") end,
        fun(Uri) -> arizona_jsonrpc:error(Id, -32002, <<"Resource not found: ", Uri/binary>>) end,
        fun(Uri) -> resource_contents(Mod:read_resource(Uri, State), Uri, Id) end
    ).

get_prompt(Params, Id, Mod, State) ->
    with_member(
        Params,
        ~"name",
        [N || #{name := N} <- Mod:prompts(State)],
        fun() -> arizona_jsonrpc:error(Id, -32602, ~"Invalid params: missing prompt name") end,
        fun(Name) -> arizona_jsonrpc:error(Id, -32602, <<"Unknown prompt: ", Name/binary>>) end,
        fun(Name) ->
            Args = maps:get(~"arguments", Params, #{}),
            prompt_messages(Mod:get_prompt(Name, Args, State), Id)
        end
    ).

tool_reply({reply, Result, _State}, Id) ->
    {reply, arizona_jsonrpc:result(Id, tool_content(Result, false))};
tool_reply({error, ToolError, _State}, Id) ->
    {reply, arizona_jsonrpc:result(Id, tool_content(ToolError, true))}.

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
%% supplies its content entries verbatim. An `{error, _}` is a -32002.
resource_contents({reply, Text, _State}, Uri, Id) when is_binary(Text) ->
    Contents = [#{~"uri" => Uri, ~"text" => Text}],
    {reply, arizona_jsonrpc:result(Id, #{~"contents" => Contents})};
resource_contents({reply, #{contents := Contents}, _State}, _Uri, Id) ->
    {reply, arizona_jsonrpc:result(Id, #{~"contents" => Contents})};
resource_contents({error, Message, _State}, _Uri, Id) when is_binary(Message) ->
    {error, arizona_jsonrpc:error(Id, -32002, Message)}.

prompt_json(#{name := Name} = Prompt) ->
    Base = #{~"name" => Name},
    WithDescription = maybe_put(~"description", description, Prompt, Base),
    maybe_put(~"arguments", arguments, Prompt, WithDescription).

prompt_messages({reply, #{messages := Messages} = Result, _State}, Id) ->
    Base = #{~"messages" => Messages},
    Object = maybe_put(~"description", description, Result, Base),
    {reply, arizona_jsonrpc:result(Id, Object)};
prompt_messages({error, Message, _State}, Id) when is_binary(Message) ->
    {error, arizona_jsonrpc:error(Id, -32602, Message)}.
