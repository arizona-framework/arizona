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
-export([user_query_is_rebuilt_as_escaped_ascii/1]).

all() ->
    [
        unknown_path_is_a_404_reply,
        halt_is_its_halt_reply,
        upgrade_carries_the_flushed_lines,
        user_query_is_rebuilt_as_escaped_ascii
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

%% The parsed query is rebuilt for the route without the `_az_*` keys: every
%% octet but the unreserved ones escaped, a reserved character and a byte that
%% is not UTF-8 alike, so it parses back to the same pairs on any server.
user_query_is_rebuilt_as_escaped_ascii(Config) when is_list(Config) ->
    QS = [
        {~"_az_path", ~"/"},
        {~"x", <<255>>},
        {~"k&=", ~"%&=#"},
        {~"y", ~"a b+~-._"},
        {~"_az_reconnect", ~"1"},
        {~"flag", true}
    ],
    {cont, #{req := Req}} = arizona_ws:prepare(QS, arizona_req_test_adapter, raw(#{})),
    ?assertMatch(
        #{resolved_qs := ~"x=%FF&k%26%3D=%25%26%3D%23&y=a%20b%2B~-._&flag"},
        arizona_req:raw(Req)
    ).

raw(RouteOpts) ->
    #{headers => #{}, routes => #{~"/" => {arizona_root_counter, RouteOpts}}}.
