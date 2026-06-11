-module(arizona_mcp_SUITE).
-include_lib("stdlib/include/assert.hrl").

-export([all/0]).
-export([
    dispatch_initialize/1,
    dispatch_initialize_unsupported_version/1,
    dispatch_ping/1,
    dispatch_tools_list/1,
    dispatch_tools_call_success/1,
    dispatch_tools_call_structured/1,
    dispatch_tools_call_tool_error/1,
    dispatch_tools_call_unknown_tool/1,
    dispatch_tools_call_missing_name/1,
    dispatch_unknown_method/1,
    dispatch_unknown_notification/1,
    dispatch_notifications_initialized/1,
    handle_get_405/1,
    handle_origin_absent_ok/1,
    handle_origin_allowed_ok/1,
    handle_origin_disallowed_403/1,
    handle_origin_empty_allowlist_403/1,
    handle_tool_crash_internal_error/1
]).

-define(SERVER, arizona_mcp_test_server).

all() ->
    [
        dispatch_initialize,
        dispatch_initialize_unsupported_version,
        dispatch_ping,
        dispatch_tools_list,
        dispatch_tools_call_success,
        dispatch_tools_call_structured,
        dispatch_tools_call_tool_error,
        dispatch_tools_call_unknown_tool,
        dispatch_tools_call_missing_name,
        dispatch_unknown_method,
        dispatch_unknown_notification,
        dispatch_notifications_initialized,
        handle_get_405,
        handle_origin_absent_ok,
        handle_origin_allowed_ok,
        handle_origin_disallowed_403,
        handle_origin_empty_allowlist_403,
        handle_tool_crash_internal_error
    ].

%% --------------------------------------------------------------------
%% dispatch/3 (pure)
%% --------------------------------------------------------------------

dispatch_initialize(_Config) ->
    Params = #{~"protocolVersion" => ~"2025-06-18", ~"capabilities" => #{}},
    {reply, #{~"result" := Result}} = dispatch(~"initialize", Params, 1),
    ?assertEqual(~"2025-06-18", maps:get(~"protocolVersion", Result)),
    ?assertEqual(#{tools => #{}}, maps:get(~"capabilities", Result)),
    ?assertEqual(
        #{~"name" => ~"arizona_test", ~"version" => ~"0.1.0"},
        maps:get(~"serverInfo", Result)
    ).

dispatch_initialize_unsupported_version(_Config) ->
    Params = #{~"protocolVersion" => ~"1999-01-01"},
    {reply, #{~"result" := Result}} = dispatch(~"initialize", Params, 1),
    ?assertEqual(~"2025-11-25", maps:get(~"protocolVersion", Result)).

dispatch_ping(_Config) ->
    ?assertEqual(
        {reply, #{~"jsonrpc" => ~"2.0", ~"id" => 2, ~"result" => #{}}},
        dispatch(~"ping", #{}, 2)
    ).

dispatch_tools_list(_Config) ->
    {reply, #{~"result" := #{~"tools" := Tools}}} = dispatch(~"tools/list", #{}, 3),
    ?assertEqual([~"add", ~"boom", ~"crash", ~"echo"], [maps:get(~"name", T) || T <- Tools]),
    [Add | _] = Tools,
    %% input_schema is renamed to the wire's inputSchema.
    ?assertMatch(#{~"inputSchema" := #{type := ~"object"}}, Add),
    ?assertNot(maps:is_key(~"input_schema", Add)),
    %% A tool's optional title passes through to the wire.
    Echo = lists:last(Tools),
    ?assertEqual(~"Echo", maps:get(~"title", Echo)).

dispatch_tools_call_success(_Config) ->
    Params = #{~"name" => ~"add", ~"arguments" => #{~"a" => 2, ~"b" => 3}},
    {reply, #{~"result" := Result}} = dispatch(~"tools/call", Params, 4),
    ?assertEqual(
        #{~"content" => [#{~"type" => ~"text", ~"text" => ~"5"}], ~"isError" => false},
        Result
    ).

dispatch_tools_call_structured(_Config) ->
    Params = #{~"name" => ~"echo", ~"arguments" => #{~"x" => 1}},
    {reply, #{~"result" := Result}} = dispatch(~"tools/call", Params, 9),
    ?assertEqual(
        #{
            ~"content" => [#{type => ~"text", text => ~"echo"}],
            ~"isError" => false,
            ~"structuredContent" => #{~"x" => 1}
        },
        Result
    ).

dispatch_tools_call_tool_error(_Config) ->
    Params = #{~"name" => ~"boom", ~"arguments" => #{}},
    {reply, #{~"result" := Result}} = dispatch(~"tools/call", Params, 5),
    ?assertEqual(
        #{~"content" => [#{~"type" => ~"text", ~"text" => ~"kaboom"}], ~"isError" => true},
        Result
    ).

dispatch_tools_call_unknown_tool(_Config) ->
    Params = #{~"name" => ~"nope", ~"arguments" => #{}},
    {error, #{~"error" := #{~"code" := Code}}} = dispatch(~"tools/call", Params, 6),
    ?assertEqual(-32602, Code).

dispatch_tools_call_missing_name(_Config) ->
    {error, #{~"error" := #{~"code" := Code}}} = dispatch(~"tools/call", #{}, 7),
    ?assertEqual(-32602, Code).

dispatch_unknown_method(_Config) ->
    {error, #{~"error" := #{~"code" := Code}}} = dispatch(~"does/not/exist", #{}, 8),
    ?assertEqual(-32601, Code).

dispatch_unknown_notification(_Config) ->
    ?assertEqual(notification, dispatch(~"some/notification", #{}, undefined)).

dispatch_notifications_initialized(_Config) ->
    ?assertEqual(notification, dispatch(~"notifications/initialized", #{}, undefined)).

%% --------------------------------------------------------------------
%% handle/1 (transport: method gate + origin + crash mapping)
%% --------------------------------------------------------------------

handle_get_405(_Config) ->
    Resp = handle(~"GET", [], <<>>, #{handler => ?SERVER}),
    ?assertEqual(405, status(Resp)),
    ?assertEqual({~"allow", ~"POST"}, lists:keyfind(~"allow", 1, headers(Resp))).

handle_origin_absent_ok(_Config) ->
    Resp = handle(~"POST", [], ping_body(), #{handler => ?SERVER, origins => []}),
    ?assertEqual(200, status(Resp)),
    ?assertMatch(#{~"result" := #{}}, decode_body(Resp)).

handle_origin_allowed_ok(_Config) ->
    Headers = [{~"origin", ~"http://localhost:3000"}],
    Opts = #{handler => ?SERVER, origins => [~"http://localhost:3000"]},
    Resp = handle(~"POST", Headers, ping_body(), Opts),
    ?assertEqual(200, status(Resp)).

handle_origin_disallowed_403(_Config) ->
    Headers = [{~"origin", ~"http://evil.example"}],
    Opts = #{handler => ?SERVER, origins => [~"http://localhost:3000"]},
    Resp = handle(~"POST", Headers, ping_body(), Opts),
    ?assertEqual(403, status(Resp)).

handle_origin_empty_allowlist_403(_Config) ->
    Headers = [{~"origin", ~"http://localhost:3000"}],
    Resp = handle(~"POST", Headers, ping_body(), #{handler => ?SERVER, origins => []}),
    ?assertEqual(403, status(Resp)).

handle_tool_crash_internal_error(_Config) ->
    Body =
        ~"""
    {"jsonrpc":"2.0","id":9,"method":"tools/call","params":{"name":"crash","arguments":{}}}
    """,
    Resp = handle(~"POST", [], Body, #{handler => ?SERVER}),
    ?assertEqual(200, status(Resp)),
    ?assertMatch(#{~"error" := #{~"code" := -32603}}, decode_body(Resp)).

%% --------------------------------------------------------------------
%% Helpers
%% --------------------------------------------------------------------

dispatch(Method, Params, Id) ->
    Request = #{method => Method, params => Params, id => Id},
    arizona_mcp_handler:dispatch(?SERVER, Request, #{}).

handle(Method, Headers, Body, Opts) ->
    %% A complete `roadrunner_req:request()` (method/target/version/headers
    %% are required) so the handler's contract is satisfied at the type level.
    Req = #{
        method => Method,
        target => ~"/mcp",
        version => {1, 1},
        headers => Headers,
        body => Body,
        state => #{arizona => Opts}
    },
    {Resp, _Req} = arizona_mcp_handler:handle(Req),
    Resp.

ping_body() ->
    ~"""
    {"jsonrpc":"2.0","id":1,"method":"ping"}
    """.

status({Status, _Headers, _Body}) -> Status.

headers({_Status, Headers, _Body}) -> Headers.

decode_body({_Status, _Headers, Body}) -> json:decode(iolist_to_binary(Body)).
