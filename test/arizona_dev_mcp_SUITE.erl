-module(arizona_dev_mcp_SUITE).
-include_lib("stdlib/include/assert.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([
    tools_list/1,
    list_routes_lists_mcp_route/1,
    list_routes_formats_shapes/1,
    list_routes_env_unset/1,
    describe_component_stateful/1,
    describe_component_stateless/1,
    describe_component_other/1,
    describe_component_not_loaded/1,
    get_module_docs/1,
    get_function_docs/1,
    get_docs_unknown_function/1,
    get_source_location_module/1,
    get_source_location_function/1,
    get_source_location_no_line_info/1,
    get_source_location_unknown_function/1,
    reloader_status_ok/1,
    app_info_reports_version/1,
    eval_returns_value/1,
    eval_appends_dot/1,
    eval_bindings_persist/1,
    eval_error_is_in_band/1,
    render_component_stateful/1,
    render_component_stateless/1,
    render_component_other/1,
    render_component_bad_bindings/1,
    render_component_unknown_key/1
]).

all() ->
    [
        tools_list,
        list_routes_lists_mcp_route,
        list_routes_formats_shapes,
        list_routes_env_unset,
        describe_component_stateful,
        describe_component_stateless,
        describe_component_other,
        describe_component_not_loaded,
        get_module_docs,
        get_function_docs,
        get_docs_unknown_function,
        get_source_location_module,
        get_source_location_function,
        get_source_location_no_line_info,
        get_source_location_unknown_function,
        reloader_status_ok,
        app_info_reports_version,
        eval_returns_value,
        eval_appends_dot,
        eval_bindings_persist,
        eval_error_is_in_band,
        render_component_stateful,
        render_component_stateless,
        render_component_other,
        render_component_bad_bindings,
        render_component_unknown_key
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(arizona),
    Config.

end_per_suite(_Config) ->
    _ = application:stop(arizona),
    ok.

init_per_testcase(_Case, Config) ->
    %% list_routes reads the `server` app env; seed it with the dev server routes.
    ok = application:set_env(arizona, server, #{routes => arizona_test_server:routes()}),
    Config.

end_per_testcase(_Case, _Config) ->
    ok = application:unset_env(arizona, server),
    ok.

%% --------------------------------------------------------------------
%% tools
%% --------------------------------------------------------------------

tools_list(_Config) ->
    Names = [maps:get(name, Tool) || Tool <- arizona_dev_mcp:tools(#{})],
    Expected = [
        ~"list_routes",
        ~"describe_component",
        ~"get_docs",
        ~"get_source_location",
        ~"reloader_status",
        ~"app_info",
        ~"render_component",
        ~"eval"
    ],
    ?assertEqual(lists:sort(Expected), lists:sort(Names)).

%% --------------------------------------------------------------------
%% list_routes
%% --------------------------------------------------------------------

list_routes_lists_mcp_route(_Config) ->
    {reply, Text, _} = call(~"list_routes", #{}),
    ?assertMatch({_, _}, binary:match(Text, ~"arizona_dev_mcp")),
    ?assertMatch({_, _}, binary:match(Text, ~"/mcp")).

list_routes_formats_shapes(_Config) ->
    {reply, Text, _} = call(~"list_routes", #{}),
    %% A 4-tuple (live), and the two 3-tuple shapes (ws, asset).
    ?assertMatch({_, _}, binary:match(Text, ~"live")),
    ?assertMatch({_, _}, binary:match(Text, ~"ws")),
    ?assertMatch({_, _}, binary:match(Text, ~"asset")).

list_routes_env_unset(_Config) ->
    ok = application:unset_env(arizona, server),
    ?assertMatch({reply, ~"(no routes configured)", _}, call(~"list_routes", #{})).

%% --------------------------------------------------------------------
%% describe_component
%% --------------------------------------------------------------------

describe_component_stateful(_Config) ->
    {reply, Text, _} = call(~"describe_component", #{~"module" => ~"arizona_counter"}),
    ?assertMatch({_, _}, binary:match(Text, ~"stateful")),
    ?assertMatch({_, _}, binary:match(Text, ~"mount/1")),
    ?assertMatch({_, _}, binary:match(Text, ~"render/1")).

describe_component_stateless(_Config) ->
    {reply, Text, _} = call(~"describe_component", #{~"module" => ~"arizona_layout"}),
    ?assertMatch({_, _}, binary:match(Text, ~"stateless")),
    ?assertMatch({_, _}, binary:match(Text, ~"render/1")),
    ?assertEqual(nomatch, binary:match(Text, ~"mount")).

describe_component_other(_Config) ->
    %% arizona_pubsub is a plain module (no mount/1 or render/1).
    {reply, Text, _} = call(~"describe_component", #{~"module" => ~"arizona_pubsub"}),
    ?assertMatch({_, _}, binary:match(Text, ~"other")).

describe_component_not_loaded(_Config) ->
    Args = #{~"module" => ~"arizona_no_such_module_xyzzy"},
    ?assertMatch({error, Bin, _} when is_binary(Bin), call(~"describe_component", Args)).

%% --------------------------------------------------------------------
%% get_docs
%% --------------------------------------------------------------------

get_module_docs(_Config) ->
    %% arizona_mcp has a substantial moduledoc mentioning MCP.
    {reply, Text, _} = call(~"get_docs", #{~"module" => ~"arizona_mcp"}),
    ?assertMatch({_, _}, binary:match(Text, ~"MCP")).

get_function_docs(_Config) ->
    Args = #{~"module" => ~"arizona_mcp", ~"function" => ~"progress"},
    ?assertMatch({reply, Bin, _} when is_binary(Bin) andalso Bin =/= <<>>, call(~"get_docs", Args)).

get_docs_unknown_function(_Config) ->
    %% A function name that isn't a known atom reports no docs, in-band.
    Args = #{~"module" => ~"arizona_mcp", ~"function" => ~"zzz_no_such_fn_zzz"},
    ?assertMatch({reply, ~"(no documentation for that function)", _}, call(~"get_docs", Args)).

%% --------------------------------------------------------------------
%% get_source_location
%% --------------------------------------------------------------------

get_source_location_module(_Config) ->
    {reply, Text, _} = call(~"get_source_location", #{~"module" => ~"arizona_counter"}),
    ?assertMatch({_, _}, binary:match(Text, ~"arizona_counter.erl")).

get_source_location_function(_Config) ->
    %% arizona_mcp has a doc chunk, so the function's line is resolved.
    Args = #{~"module" => ~"arizona_mcp", ~"function" => ~"progress"},
    {reply, Text, _} = call(~"get_source_location", Args),
    ?assertMatch({_, _}, binary:match(Text, ~"arizona_mcp.erl")),
    ?assertMatch({_, _}, binary:match(Text, ~"line")).

get_source_location_no_line_info(_Config) ->
    %% A known atom that isn't a function of the module: source file, no line.
    Args = #{~"module" => ~"arizona_counter", ~"function" => ~"start_link"},
    {reply, Text, _} = call(~"get_source_location", Args),
    ?assertMatch({_, _}, binary:match(Text, ~"arizona_counter.erl")),
    ?assertMatch({_, _}, binary:match(Text, ~"no line info")).

get_source_location_unknown_function(_Config) ->
    %% A function name that isn't a known atom still returns the source file.
    Args = #{~"module" => ~"arizona_counter", ~"function" => ~"zzz_no_such_fn_zzz"},
    {reply, Text, _} = call(~"get_source_location", Args),
    ?assertMatch({_, _}, binary:match(Text, ~"arizona_counter.erl")),
    ?assertMatch({_, _}, binary:match(Text, ~"no such function")).

%% --------------------------------------------------------------------
%% reloader_status / app_info
%% --------------------------------------------------------------------

reloader_status_ok(_Config) ->
    ok = arizona_reloader:clear_error(),
    {reply, Text, _} = call(~"reloader_status", #{}),
    ?assertMatch({_, _}, binary:match(Text, ~"ok")).

app_info_reports_version(_Config) ->
    {reply, Text, _} = call(~"app_info", #{}),
    ?assertMatch({_, _}, binary:match(Text, ~"arizona")),
    ?assertMatch({_, _}, binary:match(Text, ~"OTP")).

%% --------------------------------------------------------------------
%% eval
%% --------------------------------------------------------------------

eval_returns_value(_Config) ->
    ?assertMatch({reply, ~"3", _}, call(~"eval", #{~"code" => ~"1 + 2."})).

eval_appends_dot(_Config) ->
    %% Code without a trailing dot still evaluates.
    ?assertMatch({reply, ~"3", _}, call(~"eval", #{~"code" => ~"1 + 2"})).

eval_bindings_persist(_Config) ->
    {reply, _, State1} = call(~"eval", #{~"code" => ~"X = 41."}, state()),
    ?assertMatch({reply, ~"42", _}, call(~"eval", #{~"code" => ~"X + 1."}, State1)).

eval_error_is_in_band(_Config) ->
    ?assertMatch({error, Bin, _} when is_binary(Bin), call(~"eval", #{~"code" => ~"1 +."})).

%% --------------------------------------------------------------------
%% render_component
%% --------------------------------------------------------------------

render_component_stateful(_Config) ->
    Args = #{~"module" => ~"arizona_counter", ~"bindings" => #{~"id" => ~"c1", ~"count" => 7}},
    {reply, Html, _} = call(~"render_component", Args),
    ?assertMatch({_, _}, binary:match(Html, ~"Count")).

render_component_stateless(_Config) ->
    Bindings = #{~"title" => ~"Hi", ~"inner_content" => ~"<p>Body</p>"},
    Args = #{~"module" => ~"arizona_layout", ~"bindings" => Bindings},
    {reply, Html, _} = call(~"render_component", Args),
    ?assertMatch({_, _}, binary:match(Html, ~"Body")).

render_component_other(_Config) ->
    Args = #{~"module" => ~"arizona_pubsub", ~"bindings" => #{}},
    ?assertMatch({error, Bin, _} when is_binary(Bin), call(~"render_component", Args)).

render_component_bad_bindings(_Config) ->
    %% A layout without inner_content fails to render; the try/catch makes it in-band.
    Args = #{~"module" => ~"arizona_layout", ~"bindings" => #{~"title" => ~"Hi"}},
    ?assertMatch({error, Bin, _} when is_binary(Bin), call(~"render_component", Args)).

render_component_unknown_key(_Config) ->
    %% A binding key that isn't an existing atom is rejected in-band (no atom leak).
    Args = #{~"module" => ~"arizona_counter", ~"bindings" => #{~"no_such_binding_xyzzy" => 1}},
    ?assertMatch({error, Bin, _} when is_binary(Bin), call(~"render_component", Args)).

%% --------------------------------------------------------------------
%% Helpers
%% --------------------------------------------------------------------

state() ->
    {ok, _Info, _Caps, State} = arizona_dev_mcp:init(#{}),
    State.

ctx() ->
    #{token => undefined, to => undefined}.

call(Tool, Args) ->
    call(Tool, Args, state()).

call(Tool, Args, State) ->
    arizona_dev_mcp:handle_tool(Tool, Args, ctx(), State).
