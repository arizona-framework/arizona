-module(arizona_ws_upgrade_SUITE).
-moduledoc """
`arizona_ws:upgrade/3`, the server-neutral answer to a WebSocket upgrade
request, over the in-memory `arizona_req_test_adapter` (no listener).
""".
-include_lib("stdlib/include/assert.hrl").

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).

-export([unknown_path_is_a_404_reply/1]).
-export([halt_is_its_halt_reply/1]).
-export([upgrade_carries_the_flushed_lines/1]).

all() ->
    [
        unknown_path_is_a_404_reply,
        halt_is_its_halt_reply,
        upgrade_carries_the_flushed_lines
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(arizona),
    Config.

end_per_suite(Config) when is_list(Config) ->
    ok = application:stop(arizona).

%% The reply goes back with the adapter state it belongs to, untouched.
unknown_path_is_a_404_reply(Config) when is_list(Config) ->
    Raw = raw(#{}),
    ?assertEqual(
        {reply, 404, [], Raw},
        arizona_ws:upgrade([{~"_az_path", ~"/missing"}], arizona_req_test_adapter, Raw)
    ).

halt_is_its_halt_reply(Config) when is_list(Config) ->
    Gate = fun(R, _B) ->
        R1 = arizona_req:put_resp_cookie(R, ~"gate", ~"seen", #{}),
        {halt, arizona_req:redirect(R1, 303, ~"/login")}
    end,
    ?assertMatch(
        {reply, 303, [{~"set-cookie", ~"gate=seen"}, {~"location", ~"/login"}], #{}},
        arizona_ws:upgrade(
            [{~"_az_path", ~"/"}], arizona_req_test_adapter, raw(#{middlewares => [Gate]})
        )
    ).

upgrade_carries_the_flushed_lines(Config) when is_list(Config) ->
    Stash = fun(R, B) -> {cont, arizona_req:put_resp_cookie(R, ~"probe", ~"ws", #{}), B} end,
    ?assertMatch(
        {upgrade, [{~"set-cookie", ~"probe=ws"}], #{handler := arizona_root_counter}},
        arizona_ws:upgrade(
            [{~"_az_path", ~"/"}], arizona_req_test_adapter, raw(#{middlewares => [Stash]})
        )
    ).

raw(RouteOpts) ->
    #{headers => #{}, routes => #{~"/" => {arizona_root_counter, RouteOpts}}}.
