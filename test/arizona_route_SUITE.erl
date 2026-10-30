-module(arizona_route_SUITE).
-include_lib("stdlib/include/assert.hrl").

-export([all/0]).

-export([kind_names_every_route_shape/1]).
-export([kind_refuses_a_term_that_is_no_route/1]).
-export([unsupported_names_kind_and_adapter/1]).
-export([path_reads_every_shape/1]).
-export([methods_imply_head_and_any/1]).
-export([methods_refuse_a_wildcard_inside_a_list/1]).
-export([steps_put_check_origin_first_unless_opted_out/1]).
-export([transforms_put_compression_outermost/1]).
-export([transforms_refuse_typos_and_asset_steps/1]).
-export([live_and_controller_name_what_they_run/1]).

all() ->
    [
        kind_names_every_route_shape,
        kind_refuses_a_term_that_is_no_route,
        unsupported_names_kind_and_adapter,
        path_reads_every_shape,
        methods_imply_head_and_any,
        methods_refuse_a_wildcard_inside_a_list,
        steps_put_check_origin_first_unless_opted_out,
        transforms_put_compression_outermost,
        transforms_refuse_typos_and_asset_steps,
        live_and_controller_name_what_they_run
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
    Message = format(arizona_route, kind, [{lvie, ~"/", page, #{}}]),
    ?assertNotEqual(nomatch, binary:match(Message, ~"is not a route")).

unsupported_names_kind_and_adapter(Config) when is_list(Config) ->
    ?assertError(
        {unsupported_route, mcp, some_adapter}, arizona_route:unsupported(mcp, some_adapter)
    ),
    Message = format(arizona_route, unsupported, [mcp, some_adapter]),
    ?assertNotEqual(
        nomatch, binary:match(Message, ~"some_adapter server adapter cannot serve a mcp")
    ).

path_reads_every_shape(Config) when is_list(Config) ->
    ?assertEqual(~"/c", arizona_route:path({match, '*', ~"/c", ctrl, #{}})),
    ?assertEqual(~"/c", arizona_route:path({post, ~"/c", ctrl, #{}})),
    ?assertEqual(~"/ws", arizona_route:path({ws, ~"/ws", #{}})).

methods_imply_head_and_any(Config) when is_list(Config) ->
    ?assertEqual([~"GET", ~"HEAD"], arizona_route:methods({live, ~"/", page, #{}})),
    ?assertEqual([~"GET", ~"HEAD"], arizona_route:methods({get, ~"/c", ctrl, #{}})),
    ?assertEqual([~"POST"], arizona_route:methods({post, ~"/c", ctrl, #{}})),
    ?assertEqual(
        [~"PROPFIND", ~"MOVE"],
        arizona_route:methods({match, [~"propfind", move], ~"/c", ctrl, #{}})
    ),
    ?assertEqual(any, arizona_route:methods({match, '*', ~"/c", ctrl, #{}})),
    ?assertEqual(any, arizona_route:methods({ws, ~"/ws", #{}})).

methods_refuse_a_wildcard_inside_a_list(Config) when is_list(Config) ->
    ?assertError(
        wildcard_in_method_list, arizona_route:methods({match, [get, '*'], ~"/c", ctrl, #{}})
    ),
    ?assertError(wildcard_in_method_list, arizona_route:methods({match, [~"*"], ~"/c", ctrl, #{}})).

%% The CSRF check is a default an adapter must not be able to forget: it is
%% part of the steps the route compiles to.
steps_put_check_origin_first_unless_opted_out(Config) when is_list(Config) ->
    Step = {my_mod, my_step},
    Mws = [arizona_middleware:etag(), Step],
    ?assertEqual(
        [{arizona_middleware, check_origin}, Step],
        arizona_route:steps({post, ~"/c", ctrl, #{middlewares => Mws}})
    ),
    ?assertEqual(
        [Step],
        arizona_route:steps({live, ~"/", page, #{middlewares => Mws, check_origin => false}})
    ).

transforms_put_compression_outermost(Config) when is_list(Config) ->
    Etag = arizona_middleware:etag(),
    Compress = arizona_middleware:compress(),
    Page = {live, ~"/", page, #{middlewares => [Etag]}},
    ?assertEqual([Compress, Etag], arizona_route:transforms(Page, #{})),
    ?assertEqual([Etag], arizona_route:transforms(Page, #{compress => false})),
    Explicit = {post, ~"/c", ctrl, #{middlewares => [Etag, Compress]}},
    ?assertEqual([Compress, Etag], arizona_route:transforms(Explicit, #{compress => false})),
    ?assertEqual(
        [Etag], arizona_route:transforms({post, ~"/c", ctrl, #{middlewares => [Etag]}}, #{})
    ),
    ?assertEqual([], arizona_route:transforms({ws, ~"/ws", #{}}, #{})).

transforms_refuse_typos_and_asset_steps(Config) when is_list(Config) ->
    ?assertError(
        {unknown_http_transform, etga},
        arizona_route:transforms(
            {live, ~"/", page, #{middlewares => [{http_transform, etga}]}}, #{}
        )
    ),
    ?assertError(
        {asset_middleware_not_transform, [{my_mod, my_step}]},
        arizona_route:transforms(
            {asset, ~"/a", {dir, "priv"}, #{middlewares => [{my_mod, my_step}]}}, #{}
        )
    ).

live_and_controller_name_what_they_run(Config) when is_list(Config) ->
    ?assertEqual(
        #{
            handler => page,
            layouts => [{lay, render}],
            bindings => #{a => 1},
            on_mount => [],
            middlewares => [{arizona_middleware, check_origin}],
            error_page => {my_err, render}
        },
        arizona_route:live(
            {live, ~"/", page, #{layouts => [{lay, render}], bindings => #{a => 1}}},
            #{error_page => {my_err, render}}
        )
    ),
    %% No listener `error_page`, none injected: a dev crash is then plain text.
    ?assertEqual(
        #{
            handler => page,
            layouts => [],
            bindings => #{},
            on_mount => [],
            middlewares => [{arizona_middleware, check_origin}]
        },
        arizona_route:live({live, ~"/", page, #{}}, #{compress => true})
    ),
    ?assertEqual(
        #{
            handler => ctrl,
            action => create,
            state => #{k => v},
            middlewares => [{arizona_middleware, check_origin}]
        },
        arizona_route:controller({post, ~"/c", ctrl, #{action => create, state => #{k => v}}})
    ),
    ?assertEqual("priv", arizona_route:asset_dir({asset, ~"/a", {dir, "priv"}})).

%% The rendered error of calling `M:F(A...)`, which must raise.
format(M, F, A) ->
    try apply(M, F, A) of
        Value -> error({not_raised, Value})
    catch
        error:Reason:ST -> iolist_to_binary(erl_error:format_exception(error, Reason, ST))
    end.
