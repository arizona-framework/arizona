-module(arizona_server_registry_SUITE).
-moduledoc """
The registry's dev-mode signals, driven only by the listeners adapters report
(`listener_up/3`, `listener_down/1`), with no server running.
""".
-include_lib("stdlib/include/assert.hrl").

-export([all/0]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([reload_url_follows_the_running_reload_routes/1]).
-export([dev_mcp_flag_follows_the_running_dev_mcp_route/1]).
-export([stopping_an_unknown_listener_changes_nothing/1]).

all() ->
    [
        reload_url_follows_the_running_reload_routes,
        dev_mcp_flag_follows_the_running_dev_mcp_route,
        stopping_an_unknown_listener_changes_nothing
    ].

%% The signals are node-wide; each case starts clean and leaves it so.
init_per_testcase(_Case, Config) ->
    ok = clear(),
    Config.

end_per_testcase(_Case, _Config) ->
    clear().

reload_url_follows_the_running_reload_routes(Config) when is_list(Config) ->
    ok = arizona_server_registry:listener_up(arizona_fake_server, prod, [{live, ~"/", page, #{}}]),
    ?assertEqual(undefined, reload_url()),
    ok = arizona_server_registry:listener_up(arizona_fake_server, dev_a, [{reload, ~"/a", #{}}]),
    ?assertEqual(~"/a", reload_url()),
    %% The listener started last takes the URL.
    ok = arizona_server_registry:listener_up(arizona_fake_server, dev_b, [{reload, ~"/b", #{}}]),
    ?assertEqual(~"/b", reload_url()),
    %% Stopping it hands the URL back to one still serving a reload route.
    ok = arizona_server_registry:listener_down(dev_b),
    ?assertEqual(~"/a", reload_url()),
    %% Stopping the last dev listener clears it, so the production one never
    %% serves the dev error page.
    ok = arizona_server_registry:listener_down(dev_a),
    ?assertEqual(undefined, reload_url()).

dev_mcp_flag_follows_the_running_dev_mcp_route(Config) when is_list(Config) ->
    ok = arizona_server_registry:listener_up(arizona_fake_server, mcp_listener, [
        arizona_dev_mcp:route(~"/mcp")
    ]),
    ?assertEqual(true, persistent_term:get(arizona_dev_mcp_route, undefined)),
    ok = arizona_server_registry:listener_down(mcp_listener),
    ?assertEqual(undefined, persistent_term:get(arizona_dev_mcp_route, undefined)).

stopping_an_unknown_listener_changes_nothing(Config) when is_list(Config) ->
    ok = arizona_server_registry:listener_up(arizona_fake_server, dev_a, [{reload, ~"/a", #{}}]),
    ok = arizona_server_registry:listener_down(never_started),
    ?assertEqual(~"/a", reload_url()).

reload_url() ->
    persistent_term:get(arizona_reload_url, undefined).

clear() ->
    [
        ok = arizona_server_registry:listener_down(Name)
     || Name <- [prod, dev_a, dev_b, mcp_listener, never_started]
    ],
    ok.
