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
    dispatch_tools_call_stateless_no_accumulate/1,
    streaming_tool_relays_progress/1,
    streaming_tool_unknown/1,
    streaming_tool_crash/1,
    progress_no_opts/1,
    progress_with_opts/1,
    progress_inert_noop/1,
    dispatch_tools_call_unknown_tool/1,
    dispatch_tools_call_missing_name/1,
    dispatch_unknown_method/1,
    dispatch_unknown_notification/1,
    dispatch_notifications_initialized/1,
    dispatch_resources_list/1,
    dispatch_resources_read/1,
    dispatch_resources_read_structured/1,
    dispatch_resources_read_error/1,
    dispatch_resources_read_unknown/1,
    dispatch_resources_read_missing_uri/1,
    dispatch_resources_unsupported/1,
    dispatch_resources_templates_list/1,
    dispatch_resources_templates_unsupported/1,
    dispatch_resources_subscribe_missing_uri/1,
    dispatch_prompts_list/1,
    dispatch_prompts_get/1,
    dispatch_prompts_get_error/1,
    dispatch_prompts_get_unknown/1,
    dispatch_prompts_get_missing_name/1,
    dispatch_prompts_unsupported/1,
    dispatch_set_level/1,
    dispatch_set_level_invalid/1,
    dispatch_set_level_missing/1,
    dispatch_logging_unsupported/1,
    dispatch_complete/1,
    dispatch_complete_invalid_ref/1,
    dispatch_complete_unsupported/1,
    log_level_severities/1,
    parse_log_levels/1,
    pagination_tools_walk/1,
    pagination_resources_walk/1,
    pagination_no_cursor_when_list_fits/1,
    pagination_past_end_cursor/1,
    pagination_invalid_cursor/1,
    pagination_non_binary_cursor/1,
    handle_get_405/1,
    handle_origin_absent_ok/1,
    handle_origin_allowed_ok/1,
    handle_origin_disallowed_403/1,
    handle_origin_empty_allowlist_403/1,
    handle_auth_rejects/1,
    handle_auth_reads_header/1,
    handle_auth_mfa/1,
    handle_tool_crash_internal_error/1
]).

%% An `{Module, Function}` auth hook target (see handle_auth_mfa/1).
-export([reject_auth/1]).

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
        dispatch_tools_call_stateless_no_accumulate,
        streaming_tool_relays_progress,
        streaming_tool_unknown,
        streaming_tool_crash,
        progress_no_opts,
        progress_with_opts,
        progress_inert_noop,
        dispatch_tools_call_unknown_tool,
        dispatch_tools_call_missing_name,
        dispatch_unknown_method,
        dispatch_unknown_notification,
        dispatch_notifications_initialized,
        dispatch_resources_list,
        dispatch_resources_read,
        dispatch_resources_read_structured,
        dispatch_resources_read_error,
        dispatch_resources_read_unknown,
        dispatch_resources_read_missing_uri,
        dispatch_resources_unsupported,
        dispatch_resources_templates_list,
        dispatch_resources_templates_unsupported,
        dispatch_resources_subscribe_missing_uri,
        dispatch_prompts_list,
        dispatch_prompts_get,
        dispatch_prompts_get_error,
        dispatch_prompts_get_unknown,
        dispatch_prompts_get_missing_name,
        dispatch_prompts_unsupported,
        dispatch_set_level,
        dispatch_set_level_invalid,
        dispatch_set_level_missing,
        dispatch_logging_unsupported,
        dispatch_complete,
        dispatch_complete_invalid_ref,
        dispatch_complete_unsupported,
        log_level_severities,
        parse_log_levels,
        pagination_tools_walk,
        pagination_resources_walk,
        pagination_no_cursor_when_list_fits,
        pagination_past_end_cursor,
        pagination_invalid_cursor,
        pagination_non_binary_cursor,
        handle_get_405,
        handle_origin_absent_ok,
        handle_origin_allowed_ok,
        handle_origin_disallowed_403,
        handle_origin_empty_allowlist_403,
        handle_auth_rejects,
        handle_auth_reads_header,
        handle_auth_mfa,
        handle_tool_crash_internal_error
    ].

%% --------------------------------------------------------------------
%% dispatch/3 (pure)
%% --------------------------------------------------------------------

dispatch_initialize(_Config) ->
    Params = #{~"protocolVersion" => ~"2025-06-18", ~"capabilities" => #{}},
    {reply, #{~"result" := Result}} = dispatch(~"initialize", Params, 1),
    ?assertEqual(~"2025-06-18", maps:get(~"protocolVersion", Result)),
    ?assertEqual(
        #{
            tools => #{},
            resources => #{subscribe => true},
            prompts => #{},
            logging => #{},
            completions => #{}
        },
        maps:get(~"capabilities", Result)
    ),
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
    ?assertEqual(
        [
            ~"add",
            ~"boom",
            ~"crash",
            ~"echo",
            ~"count",
            ~"progress",
            ~"block",
            ~"sleep",
            ~"selfkill"
        ],
        [maps:get(~"name", T) || T <- Tools]
    ),
    [Add | _] = Tools,
    %% input_schema is renamed to the wire's inputSchema.
    ?assertMatch(#{~"inputSchema" := #{type := ~"object"}}, Add),
    ?assertNot(maps:is_key(~"input_schema", Add)),
    %% A tool's optional title passes through to the wire.
    [Echo] = [T || T <- Tools, maps:get(~"name", T) =:= ~"echo"],
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

dispatch_tools_call_stateless_no_accumulate(_Config) ->
    %% Stateless mode rebuilds state from init/1 each request, so a stateful
    %% tool never accumulates -- both calls see a fresh counter at 1.
    Params = #{~"name" => ~"count", ~"arguments" => #{}},
    ?assertEqual(~"1", stateless_count(Params, 40)),
    ?assertEqual(~"1", stateless_count(Params, 41)).

%% run_streaming_tool/6 relays each progress notification, then the final
%% result, to the conn pid (here, the test process).
streaming_tool_relays_progress(_Config) ->
    Ctx = #{token => ~"t", to => self()},
    _Session1 = arizona_mcp_handler:run_streaming_tool(session(), ~"progress", #{}, 1, Ctx, self()),
    ?assertEqual(1, recv_progress()),
    ?assertEqual(2, recv_progress()),
    ?assertMatch(
        #{~"result" := #{~"content" := [#{~"text" := ~"done"}], ~"isError" := false}},
        recv_result()
    ).

%% An unknown tool name in a streaming call relays a -32602 result, no crash.
streaming_tool_unknown(_Config) ->
    Ctx = #{token => ~"t", to => self()},
    _Session1 = arizona_mcp_handler:run_streaming_tool(session(), ~"nope", #{}, 1, Ctx, self()),
    ?assertMatch(#{~"error" := #{~"code" := -32602}}, recv_result()).

%% A crashing tool in a streaming call relays a -32603 result, no crash.
streaming_tool_crash(_Config) ->
    Ctx = #{token => ~"t", to => self()},
    _Session1 = arizona_mcp_handler:run_streaming_tool(session(), ~"crash", #{}, 1, Ctx, self()),
    ?assertMatch(#{~"error" := #{~"code" := -32603}}, recv_result()).

%% progress/2 (no opts): just the token + amount, no total/message.
progress_no_opts(_Config) ->
    ok = arizona_mcp:progress(#{token => ~"t", to => self()}, 7),
    #{~"method" := ~"notifications/progress", ~"params" := Params} = recv_progress_notification(),
    ?assertEqual(#{~"progressToken" => ~"t", ~"progress" => 7}, Params).

%% progress/3 with opts: total + message ride along; an integer token is fine.
progress_with_opts(_Config) ->
    ok = arizona_mcp:progress(#{token => 9, to => self()}, 3, #{total => 5, message => ~"go"}),
    #{~"params" := Params} = recv_progress_notification(),
    ?assertEqual(
        #{~"progressToken" => 9, ~"progress" => 3, ~"total" => 5, ~"message" => ~"go"}, Params
    ).

%% An inert context (no client token) makes both arities a no-op: nothing sent.
progress_inert_noop(_Config) ->
    Inert = #{token => undefined, to => undefined},
    ok = arizona_mcp:progress(Inert, 1),
    ok = arizona_mcp:progress(Inert, 1, #{total => 2}),
    receive
        {mcp_progress, _} -> ct:fail(unexpected_progress)
    after 50 -> ok
    end.

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

dispatch_resources_list(_Config) ->
    {reply, #{~"result" := #{~"resources" := Resources}}} = dispatch(~"resources/list", #{}, 20),
    ?assertEqual(
        [~"mem://greeting", ~"mem://locked", ~"mem://structured", ~"mem://counter"],
        [maps:get(~"uri", R) || R <- Resources]
    ),
    [Greeting | _] = Resources,
    ?assertEqual(~"text/plain", maps:get(~"mimeType", Greeting)),
    ?assertEqual(~"A greeting", maps:get(~"description", Greeting)).

dispatch_resources_read(_Config) ->
    {reply, #{~"result" := Result}} = dispatch(
        ~"resources/read", #{~"uri" => ~"mem://greeting"}, 21
    ),
    ?assertEqual(
        #{~"contents" => [#{~"uri" => ~"mem://greeting", ~"text" => ~"hello"}]},
        Result
    ).

dispatch_resources_read_structured(_Config) ->
    {reply, #{~"result" := Result}} = dispatch(
        ~"resources/read", #{~"uri" => ~"mem://structured"}, 26
    ),
    %% The app-supplied content entries pass through verbatim (atom keys
    %% until JSON encode).
    ?assertMatch(
        #{~"contents" := [#{uri := ~"mem://structured", text := ~"raw"}]},
        Result
    ).

dispatch_resources_read_error(_Config) ->
    {error, #{~"error" := #{~"code" := Code, ~"message" := Message}}} =
        dispatch(~"resources/read", #{~"uri" => ~"mem://locked"}, 22),
    ?assertEqual(-32002, Code),
    ?assertEqual(~"resource is locked", Message).

dispatch_resources_read_unknown(_Config) ->
    {error, #{~"error" := #{~"code" := Code}}} =
        dispatch(~"resources/read", #{~"uri" => ~"mem://nope"}, 23),
    ?assertEqual(-32002, Code).

dispatch_resources_read_missing_uri(_Config) ->
    {error, #{~"error" := #{~"code" := Code}}} = dispatch(~"resources/read", #{}, 24),
    ?assertEqual(-32602, Code).

dispatch_resources_unsupported(_Config) ->
    {error, #{~"error" := #{~"code" := Code}}} =
        dispatch_on(arizona_mcp_minimal_server, ~"resources/list", #{}, 25),
    ?assertEqual(-32601, Code).

dispatch_resources_templates_list(_Config) ->
    {reply, #{~"result" := #{~"resourceTemplates" := [Template]}}} =
        dispatch(~"resources/templates/list", #{}, 26),
    ?assertEqual(~"mem://user/{id}", maps:get(~"uriTemplate", Template)),
    ?assertEqual(~"user", maps:get(~"name", Template)),
    %% uri_template is renamed to the wire's uriTemplate.
    ?assertNot(maps:is_key(~"uri_template", Template)).

dispatch_resources_templates_unsupported(_Config) ->
    %% A server without the resources capability has no templates endpoint.
    {error, #{~"error" := #{~"code" := Code}}} =
        dispatch_on(arizona_mcp_minimal_server, ~"resources/templates/list", #{}, 27),
    ?assertEqual(-32601, Code).

dispatch_resources_subscribe_missing_uri(_Config) ->
    %% The missing-uri rejection happens before any pubsub join (no app needed).
    {error, #{~"error" := #{~"code" := Code}}} =
        dispatch(~"resources/subscribe", #{}, 28),
    ?assertEqual(-32602, Code).

dispatch_prompts_list(_Config) ->
    {reply, #{~"result" := #{~"prompts" := Prompts}}} = dispatch(~"prompts/list", #{}, 30),
    ?assertEqual([~"greet", ~"deny", ~"count"], [maps:get(~"name", P) || P <- Prompts]),
    [Greet | _] = Prompts,
    %% Prompt argument metadata passes through (atom keys until JSON encode).
    ?assertMatch([#{name := ~"who", required := true}], maps:get(~"arguments", Greet)).

dispatch_prompts_get(_Config) ->
    Params = #{~"name" => ~"greet", ~"arguments" => #{~"who" => ~"Ada"}},
    {reply, #{~"result" := Result}} = dispatch(~"prompts/get", Params, 31),
    ?assertEqual(~"A greeting", maps:get(~"description", Result)),
    ?assertEqual(
        [#{role => ~"user", content => #{type => ~"text", text => ~"Hello, Ada"}}],
        maps:get(~"messages", Result)
    ).

dispatch_prompts_get_error(_Config) ->
    {error, #{~"error" := #{~"code" := Code, ~"message" := Message}}} =
        dispatch(~"prompts/get", #{~"name" => ~"deny"}, 32),
    ?assertEqual(-32602, Code),
    ?assertEqual(~"prompt denied", Message).

dispatch_prompts_get_unknown(_Config) ->
    {error, #{~"error" := #{~"code" := Code}}} =
        dispatch(~"prompts/get", #{~"name" => ~"nope"}, 33),
    ?assertEqual(-32602, Code).

dispatch_prompts_get_missing_name(_Config) ->
    {error, #{~"error" := #{~"code" := Code}}} = dispatch(~"prompts/get", #{}, 34),
    ?assertEqual(-32602, Code).

dispatch_prompts_unsupported(_Config) ->
    {error, #{~"error" := #{~"code" := Code}}} =
        dispatch_on(arizona_mcp_minimal_server, ~"prompts/list", #{}, 35),
    ?assertEqual(-32601, Code).

%% --------------------------------------------------------------------
%% Logging (logging/setLevel) and completion (completion/complete)
%% --------------------------------------------------------------------

dispatch_set_level(_Config) ->
    %% setLevel answers the MCP empty result.
    {reply, #{~"result" := Result}} = dispatch(~"logging/setLevel", #{~"level" => ~"warning"}, 40),
    ?assertEqual(#{}, Result).

dispatch_set_level_invalid(_Config) ->
    {error, #{~"error" := #{~"code" := Code}}} =
        dispatch(~"logging/setLevel", #{~"level" => ~"loud"}, 41),
    ?assertEqual(-32602, Code).

dispatch_set_level_missing(_Config) ->
    {error, #{~"error" := #{~"code" := Code}}} = dispatch(~"logging/setLevel", #{}, 42),
    ?assertEqual(-32602, Code).

dispatch_logging_unsupported(_Config) ->
    {error, #{~"error" := #{~"code" := Code}}} =
        dispatch_on(arizona_mcp_minimal_server, ~"logging/setLevel", #{~"level" => ~"info"}, 43),
    ?assertEqual(-32601, Code).

dispatch_complete(_Config) ->
    Params = #{
        ~"ref" => #{~"type" => ~"ref/prompt", ~"name" => ~"greet"},
        ~"argument" => #{~"name" => ~"who", ~"value" => ~"Ad"}
    },
    {reply, #{~"result" := #{~"completion" := Completion}}} =
        dispatch(~"completion/complete", Params, 44),
    ?assertEqual([~"Ada", ~"Adam"], maps:get(~"values", Completion)),
    ?assertEqual(false, maps:get(~"hasMore", Completion)).

dispatch_complete_invalid_ref(_Config) ->
    Params = #{
        ~"ref" => #{~"type" => ~"ref/bogus"},
        ~"argument" => #{~"name" => ~"who", ~"value" => ~"Ad"}
    },
    {error, #{~"error" := #{~"code" := Code}}} = dispatch(~"completion/complete", Params, 45),
    ?assertEqual(-32602, Code).

dispatch_complete_unsupported(_Config) ->
    Params = #{
        ~"ref" => #{~"type" => ~"ref/prompt", ~"name" => ~"greet"},
        ~"argument" => #{~"name" => ~"who", ~"value" => ~"Ad"}
    },
    {error, #{~"error" := #{~"code" := Code}}} =
        dispatch_on(arizona_mcp_minimal_server, ~"completion/complete", Params, 46),
    ?assertEqual(-32601, Code).

log_level_severities(_Config) ->
    %% Every level maps to a strictly ascending severity.
    Levels = [debug, info, notice, warning, error, critical, alert, emergency],
    ?assertEqual([0, 1, 2, 3, 4, 5, 6, 7], [arizona_mcp:level_severity(L) || L <- Levels]).

parse_log_levels(_Config) ->
    %% Every level round-trips from its wire string; an unknown one is `error`.
    Levels = [debug, info, notice, warning, error, critical, alert, emergency],
    lists:foreach(
        fun(L) -> ?assertEqual({ok, L}, arizona_mcp:parse_level(atom_to_binary(L))) end,
        Levels
    ),
    ?assertEqual(error, arizona_mcp:parse_level(~"nope")).

%% --------------------------------------------------------------------
%% Pagination (framework-side opaque cursors over the */list methods)
%% --------------------------------------------------------------------

pagination_tools_walk(_Config) ->
    %% 9 tools at page size 2 -> 5 pages; following the cursor chain yields all
    %% 9 names in order. The page count proves it actually paginated.
    {Names, Pages} = walk(~"tools/list", ~"tools", 2),
    ?assertEqual(
        [
            ~"add",
            ~"boom",
            ~"crash",
            ~"echo",
            ~"count",
            ~"progress",
            ~"block",
            ~"sleep",
            ~"selfkill"
        ],
        Names
    ),
    ?assertEqual(5, Pages).

pagination_resources_walk(_Config) ->
    %% The capability-gated list path paginates identically.
    {Names, Pages} = walk(~"resources/list", ~"resources", 2),
    ?assertEqual([~"greeting", ~"locked", ~"structured", ~"counter"], Names),
    ?assertEqual(2, Pages).

pagination_no_cursor_when_list_fits(_Config) ->
    %% A page larger than the list returns everything and omits nextCursor.
    {reply, #{~"result" := Result}} = dispatch_paged(~"tools/list", #{}, 1, 100),
    ?assertEqual(9, length(maps:get(~"tools", Result))),
    ?assertNot(maps:is_key(~"nextCursor", Result)).

pagination_past_end_cursor(_Config) ->
    %% A valid cursor whose offset is past the end (stale/tampered) returns an
    %% empty final page with no cursor -- not a -32603 crash.
    PastEnd = base64:encode(integer_to_binary(1000)),
    {reply, #{~"result" := Result}} = dispatch_paged(~"tools/list", #{~"cursor" => PastEnd}, 1, 2),
    ?assertEqual([], maps:get(~"tools", Result)),
    ?assertNot(maps:is_key(~"nextCursor", Result)).

pagination_invalid_cursor(_Config) ->
    %% A cursor that does not decode to a non-negative integer is a -32602.
    Garbage = base64:encode(~"not-a-number"),
    {error, #{~"error" := #{~"code" := Code}}} =
        dispatch_paged(~"tools/list", #{~"cursor" => Garbage}, 1, 2),
    ?assertEqual(-32602, Code).

pagination_non_binary_cursor(_Config) ->
    %% A non-binary cursor (a misbehaving client) is rejected, not crashed.
    {error, #{~"error" := #{~"code" := Code}}} =
        dispatch_paged(~"tools/list", #{~"cursor" => 7}, 1, 2),
    ?assertEqual(-32602, Code).

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

handle_auth_rejects(_Config) ->
    Auth = fun(_Req) -> {reject, 401} end,
    Resp = handle(~"POST", [], ping_body(), #{handler => ?SERVER, auth => Auth}),
    ?assertEqual(401, status(Resp)).

handle_auth_reads_header(_Config) ->
    Auth = fun(Req) ->
        case roadrunner_req:header(~"authorization", Req) of
            ~"Bearer ok" -> ok;
            _ -> {reject, 401}
        end
    end,
    Opts = #{handler => ?SERVER, auth => Auth},
    Allowed = handle(~"POST", [{~"authorization", ~"Bearer ok"}], ping_body(), Opts),
    ?assertEqual(200, status(Allowed)),
    Denied = handle(~"POST", [], ping_body(), Opts),
    ?assertEqual(401, status(Denied)).

handle_auth_mfa(_Config) ->
    %% The `{Module, Function}` hook form.
    Resp = handle(~"POST", [], ping_body(), #{handler => ?SERVER, auth => {?MODULE, reject_auth}}),
    ?assertEqual(403, status(Resp)).

reject_auth(_Req) ->
    {reject, 403}.

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
    dispatch_on(?SERVER, Method, Params, Id).

%% Dispatch statelessly with a configured list page size (via the dispatch Ctx).
dispatch_paged(Method, Params, Id, PageSize) ->
    Request = #{method => Method, params => Params, id => Id},
    arizona_mcp_handler:dispatch(?SERVER, Request, #{page_size => PageSize}).

%% Follow the cursor chain of a `*/list` method to exhaustion, returning all
%% item names (every item -- tool/resource/prompt -- carries `name`) and the
%% number of pages it took.
walk(Method, Key, PageSize) ->
    walk(Method, Key, PageSize, undefined, [], 0).

walk(Method, Key, PageSize, Cursor, Acc, Pages) ->
    Params =
        case Cursor of
            undefined -> #{};
            _ -> #{~"cursor" => Cursor}
        end,
    {reply, #{~"result" := Result}} = dispatch_paged(Method, Params, 1, PageSize),
    Names = Acc ++ [maps:get(~"name", Item) || Item <- maps:get(Key, Result)],
    case Result of
        #{~"nextCursor" := Next} -> walk(Method, Key, PageSize, Next, Names, Pages + 1);
        _ -> {Names, Pages + 1}
    end.

%% Dispatch a `tools/call` statelessly and return the reply's counter text.
stateless_count(Params, Id) ->
    {reply, #{~"result" := #{~"content" := [#{~"text" := Text}]}}} =
        dispatch(~"tools/call", Params, Id),
    Text.

%% A session map for the test server, as the transport builds at initialize.
session() ->
    #{
        mod => ?SERVER,
        state => #{},
        caps => #{tools => #{}, resources => #{}, prompts => #{}},
        page_size => 50,
        log_min_severity => 1
    }.

%% Receive one relayed progress notification, returning its `progress` amount.
recv_progress() ->
    receive
        {mcp_progress, #{~"params" := #{~"progress" := Progress}}} -> Progress
    after 5000 -> ct:fail(no_progress)
    end.

%% Receive the relayed final result object.
recv_result() ->
    receive
        {mcp_result, Object} -> Object
    after 5000 -> ct:fail(no_result)
    end.

%% Receive one relayed progress notification (the whole JSON-RPC object).
recv_progress_notification() ->
    receive
        {mcp_progress, Notification} -> Notification
    after 5000 -> ct:fail(no_progress)
    end.

dispatch_on(Mod, Method, Params, Id) ->
    Request = #{method => Method, params => Params, id => Id},
    arizona_mcp_handler:dispatch(Mod, Request, #{}).

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
