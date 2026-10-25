-module(arizona_route_SUITE).
-include_lib("stdlib/include/assert.hrl").

-export([all/0]).

-export([kind_names_every_route_shape/1]).
-export([kind_refuses_a_term_that_is_no_route/1]).
-export([unsupported_names_kind_and_adapter/1]).

all() ->
    [
        kind_names_every_route_shape,
        kind_refuses_a_term_that_is_no_route,
        unsupported_names_kind_and_adapter
    ].

kind_names_every_route_shape(Config) when is_list(Config) ->
    ?assertEqual(
        [live, ws, asset, asset, controller, controller, controller, mcp, reload],
        [
            arizona_route:kind(R)
         || R <- [
                {live, ~"/", page, #{}},
                {ws, ~"/ws", #{}},
                {asset, ~"/a", {dir, "priv"}},
                {asset, ~"/a", {dir, "priv"}, #{}},
                {post, ~"/c", ctrl, #{}},
                {options, ~"/c", ctrl, #{}},
                {match, '*', ~"/c", ctrl, #{}},
                {mcp, ~"/mcp", server, #{}},
                {reload, ~"/reload", #{}}
            ]
        ]
    ).

%% A route list is plain data (often a sys.config), so a typo -- an unknown tag,
%% a string path, a missing element -- is refused with the valid shapes named.
kind_refuses_a_term_that_is_no_route(Config) when is_list(Config) ->
    [
        ?assertError({unknown_route, Route}, arizona_route:kind(Route))
     || Route <- [
            {lvie, ~"/", page, #{}},
            {live, "/", page, #{}},
            {ws, ~"/ws"},
            {fetch, ~"/c", ctrl, #{}}
        ]
    ],
    Message = format(fun() -> arizona_route:kind({lvie, ~"/", page, #{}}) end),
    ?assertNotEqual(nomatch, binary:match(Message, ~"is not a route")).

unsupported_names_kind_and_adapter(Config) when is_list(Config) ->
    ?assertError(
        {unsupported_route, mcp, some_adapter}, arizona_route:unsupported(mcp, some_adapter)
    ),
    Message = format(fun() -> arizona_route:unsupported(mcp, some_adapter) end),
    ?assertNotEqual(
        nomatch, binary:match(Message, ~"some_adapter server adapter cannot serve a mcp")
    ).

format(Fun) ->
    try Fun() of
        Value -> error({not_raised, Value})
    catch
        error:Reason:ST -> iolist_to_binary(erl_error:format_exception(error, Reason, ST))
    end.
