-module(arizona_demo_mcp_SUITE).
-include_lib("stdlib/include/assert.hrl").

-export([all/0]).
-export([
    list_routes_lists_mcp_route/1,
    get_module_docs/1,
    get_function_docs/1,
    app_info_reports_version/1,
    reloader_status_ok/1,
    eval_returns_value/1,
    eval_bindings_persist/1,
    eval_error_is_in_band/1
]).

all() ->
    [
        list_routes_lists_mcp_route,
        get_module_docs,
        get_function_docs,
        app_info_reports_version,
        reloader_status_ok,
        eval_returns_value,
        eval_bindings_persist,
        eval_error_is_in_band
    ].

list_routes_lists_mcp_route(_Config) ->
    {reply, Text, _} = call(~"list_routes", #{}),
    ?assertMatch({_, _}, binary:match(Text, ~"arizona_demo_mcp")),
    ?assertMatch({_, _}, binary:match(Text, ~"/mcp")).

get_module_docs(_Config) ->
    %% The demo module's own moduledoc mentions Tidewave.
    {reply, Text, _} = call(~"get_docs", #{~"module" => ~"arizona_demo_mcp"}),
    ?assertMatch({_, _}, binary:match(Text, ~"Tidewave")).

get_function_docs(_Config) ->
    %% The tool returns a function's docs as non-empty text.
    Args = #{~"module" => ~"arizona_mcp", ~"function" => ~"progress"},
    ?assertMatch({reply, Bin, _} when is_binary(Bin) andalso Bin =/= <<>>, call(~"get_docs", Args)).

app_info_reports_version(_Config) ->
    {reply, Text, _} = call(~"app_info", #{}),
    ?assertMatch({_, _}, binary:match(Text, ~"arizona")),
    ?assertMatch({_, _}, binary:match(Text, ~"OTP")).

reloader_status_ok(_Config) ->
    %% Clear any compile error a prior suite may have left in persistent_term.
    ok = arizona_reloader:clear_error(),
    {reply, Text, _} = call(~"reloader_status", #{}),
    ?assertMatch({_, _}, binary:match(Text, ~"ok")).

eval_returns_value(_Config) ->
    ?assertMatch({reply, ~"3", _}, call(~"eval", #{~"code" => ~"1 + 2."})).

eval_bindings_persist(_Config) ->
    %% A binding set in one eval is visible in the next (the session holds them).
    {reply, _, State1} = eval(~"X = 41.", state()),
    ?assertMatch({reply, ~"42", _}, eval(~"X + 1.", State1)).

eval_error_is_in_band(_Config) ->
    %% A parse/runtime failure comes back as an in-band tool error, not a crash.
    ?assertMatch({error, Bin, _} when is_binary(Bin), call(~"eval", #{~"code" => ~"1 +."})).

%% --------------------------------------------------------------------
%% Helpers
%% --------------------------------------------------------------------

state() ->
    {ok, _Info, _Caps, State} = arizona_demo_mcp:init(#{}),
    State.

ctx() ->
    #{token => undefined, to => undefined}.

call(Tool, Args) ->
    arizona_demo_mcp:handle_tool(Tool, Args, ctx(), state()).

%% Run an eval against a given session state, so a test can thread bindings.
eval(Code, State) ->
    arizona_demo_mcp:handle_tool(~"eval", #{~"code" => Code}, ctx(), State).
