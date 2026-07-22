-module(arizona_router_SUITE).
-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

-export([all/0, groups/0, init_per_group/2, end_per_group/2]).
-export([
    compile_routes_asset_dir/1,
    compile_routes_asset_priv_dir/1,
    compile_routes_asset_cache_control/1,
    compile_routes_controller/1,
    compile_routes_controller_default_origin_check/1,
    compile_routes_controller_opts_out_origin_check/1,
    compile_routes_method_gating/1,
    compile_routes_match_any_and_list/1,
    compile_routes_match_custom_verb/1,
    compile_routes_same_path_dispatch/1,
    compile_routes_per_listener_dispatch/1,
    compile_routes_per_listener_error_page/1,
    compile_routes_mcp/1,
    compile_routes_live/1,
    compile_routes_live_no_layout/1,
    compile_routes_live_multiple_pages/1,
    compile_routes_live_default_origin_check/1,
    compile_routes_live_opts_out_origin_check/1,
    compile_routes_mixed/1,
    compile_routes_ws/1
]).

all() ->
    [{group, roadrunner}].

groups() ->
    Cases = [
        compile_routes_live,
        compile_routes_live_no_layout,
        compile_routes_live_multiple_pages,
        compile_routes_live_default_origin_check,
        compile_routes_live_opts_out_origin_check,
        compile_routes_ws,
        compile_routes_asset_dir,
        compile_routes_asset_priv_dir,
        compile_routes_asset_cache_control,
        compile_routes_controller,
        compile_routes_controller_default_origin_check,
        compile_routes_controller_opts_out_origin_check,
        compile_routes_method_gating,
        compile_routes_match_any_and_list,
        compile_routes_match_custom_verb,
        compile_routes_same_path_dispatch,
        compile_routes_per_listener_dispatch,
        compile_routes_per_listener_error_page,
        compile_routes_mcp,
        compile_routes_mixed
    ],
    [
        {roadrunner, [sequence], Cases}
    ].

init_per_group(roadrunner, Config) ->
    [
        {router, arizona_roadrunner_router},
        {http_handler, arizona_roadrunner_http},
        {ws_handler, arizona_roadrunner_ws},
        {static_handler, roadrunner_static}
        | Config
    ].

end_per_group(_Adapter, _Config) ->
    ok.

%% --------------------------------------------------------------------
%% Helpers
%% --------------------------------------------------------------------

compile(Config, Routes) ->
    Router = ?config(router, Config),
    ok = Router:compile_routes(Routes).

%% Resolve a request path to `{Handler, Opts}` via roadrunner's runtime
%% lookup: `roadrunner_router:match/3` against the persistent term key
%% arizona writes — the per-route state (5th element) is what arizona
%% stashes its metadata in. Defaults the method to GET; pass a method
%% with `route_match/2` for verb-restricted routes.
route_match(Path) ->
    route_match(~"GET", Path).

route_match(Method, Path) ->
    {ok, Handler, _Bindings, _Pipeline, State} = match_result(Method, Path),
    %% arizona's per-route opts live under the `arizona` namespace
    %% inside `state` to keep them opaque from roadrunner's pipeline.
    %% Unwrap so the assertions can read handler/layouts/etc. directly.
    %% asset routes have no arizona wrap and expose their opts at the top level.
    case State of
        #{arizona := ArzOpts} -> {Handler, ArzOpts};
        _ -> {Handler, State}
    end.

%% The raw `match/3` result (incl. `{method_not_allowed, Allow}` and `not_found`).
match_result(Method, Path) ->
    Compiled = persistent_term:get(arizona_roadrunner_dispatch),
    roadrunner_router:match(Method, Path, Compiled).

%% A raw roadrunner request carrying a `listener_name`, shaped as
%% `arizona_roadrunner_req:resolve_route/3` reads it -- the method and path
%% drive the match, the listener name selects the per-listener dispatch table.
%% The `target`/`version`/`headers` keys are unused here but are required by the
%% `roadrunner_req:request()` type, so include them to keep the call well-typed.
listener_req(Name, Path) ->
    #{
        method => ~"GET",
        target => Path,
        path => Path,
        version => {1, 1},
        headers => [],
        listener_name => Name
    }.

%% --------------------------------------------------------------------
%% Tests
%% --------------------------------------------------------------------

compile_routes_live(Config) ->
    compile(Config, [
        {live, <<"/foo">>, my_handler, #{
            bindings => #{page => home},
            layouts => [{my_layout, render}]
        }}
    ]),
    {Handler, Opts} = route_match(<<"/foo">>),
    ?assertEqual(?config(http_handler, Config), Handler),
    ?assertEqual(my_handler, maps:get(handler, Opts)),
    ?assertEqual([{my_layout, render}], maps:get(layouts, Opts)),
    ?assertEqual(#{page => home}, maps:get(bindings, Opts)).

compile_routes_live_no_layout(Config) ->
    compile(Config, [
        {live, <<"/bare">>, my_handler, #{bindings => #{x => 1}}}
    ]),
    {Handler, Opts} = route_match(<<"/bare">>),
    ?assertEqual(?config(http_handler, Config), Handler),
    ?assertEqual(my_handler, maps:get(handler, Opts)),
    ?assertEqual([], maps:get(layouts, Opts)),
    ?assertEqual(#{x => 1}, maps:get(bindings, Opts)).

compile_routes_live_multiple_pages(Config) ->
    RouteOpts = fun(Id) ->
        #{bindings => #{id => Id}, layouts => [{layout, render}]}
    end,
    compile(Config, [
        {live, <<"/page1">>, handler1, RouteOpts(<<"p1">>)},
        {live, <<"/page2">>, handler2, RouteOpts(<<"p2">>)}
    ]),
    {H1, Opts1} = route_match(<<"/page1">>),
    {H2, Opts2} = route_match(<<"/page2">>),
    Expected = ?config(http_handler, Config),
    ?assertEqual(Expected, H1),
    ?assertEqual(Expected, H2),
    ?assertEqual(handler1, maps:get(handler, Opts1)),
    ?assertEqual(handler2, maps:get(handler, Opts2)),
    ?assertEqual(#{id => <<"p1">>}, maps:get(bindings, Opts1)),
    ?assertEqual(#{id => <<"p2">>}, maps:get(bindings, Opts2)).

compile_routes_live_default_origin_check(Config) ->
    %% CSRF defense is on by default: the compiled live route carries check_origin.
    compile(Config, [{live, <<"/foo">>, my_handler, #{}}]),
    {_Handler, Opts} = route_match(<<"/foo">>),
    ?assert(lists:member({arizona_middleware, check_origin}, maps:get(middlewares, Opts))).

compile_routes_live_opts_out_origin_check(Config) ->
    %% `check_origin => false` opts a route out of the default Origin check.
    compile(Config, [{live, <<"/foo">>, my_handler, #{check_origin => false}}]),
    {_Handler, Opts} = route_match(<<"/foo">>),
    ?assertNot(lists:member({arizona_middleware, check_origin}, maps:get(middlewares, Opts))).

compile_routes_ws(Config) ->
    compile(Config, [
        {ws, <<"/ws">>, #{idle_timeout => 60000}}
    ]),
    {Handler, Opts} = route_match(<<"/ws">>),
    ?assertEqual(?config(ws_handler, Config), Handler),
    ?assertEqual(#{idle_timeout => 60000}, Opts).

compile_routes_asset_dir(Config) ->
    compile(Config, [
        {asset, <<"/static">>, {dir, "/tmp/assets"}}
    ]),
    {Handler, Opts} = route_match(<<"/static/foo.js">>),
    ?assertEqual(?config(static_handler, Config), Handler),
    ?assertEqual(#{dir => "/tmp/assets"}, Opts).

compile_routes_asset_priv_dir(Config) ->
    compile(Config, [
        {asset, <<"/priv">>, {priv_dir, arizona, "static/assets/js"}}
    ]),
    {Handler, Opts} = route_match(<<"/priv/arizona.min.js">>),
    ExpectedDir = filename:join(code:priv_dir(arizona), "static/assets/js"),
    ?assertEqual(?config(static_handler, Config), Handler),
    ?assertEqual(#{dir => ExpectedDir}, Opts).

compile_routes_asset_cache_control(Config) ->
    %% The 4-tuple asset form threads `cache_control` into roadrunner_static's
    %% state verbatim; the 3-tuple form (covered above) carries only `dir`.
    compile(Config, [
        {asset, <<"/static">>, {dir, "/tmp/assets"}, #{
            cache_control => ~"public, max-age=31536000, immutable"
        }}
    ]),
    {Handler, Opts} = route_match(<<"/static/foo.js">>),
    ?assertEqual(?config(static_handler, Config), Handler),
    ?assertEqual(
        #{dir => "/tmp/assets", cache_control => ~"public, max-age=31536000, immutable"},
        Opts
    ).

compile_routes_controller(Config) ->
    %% Verb-tag controllers dispatch through arizona_roadrunner_controller; the app
    %% handler, action (default `handle`), and state live in the arizona meta.
    compile(Config, [
        {post, <<"/api/health">>, my_controller, #{state => #{key => val}}}
    ]),
    {Handler, Opts} = route_match(~"POST", <<"/api/health">>),
    ?assertEqual(arizona_roadrunner_controller, Handler),
    ?assertEqual(my_controller, maps:get(handler, Opts)),
    ?assertEqual(handle, maps:get(action, Opts)),
    ?assertEqual(#{key => val}, maps:get(state, Opts)).

compile_routes_controller_default_origin_check(Config) ->
    compile(Config, [{post, <<"/c">>, my_controller, #{}}]),
    {_Handler, Opts} = route_match(~"POST", <<"/c">>),
    ?assert(lists:member({arizona_middleware, check_origin}, maps:get(middlewares, Opts))).

compile_routes_controller_opts_out_origin_check(Config) ->
    compile(Config, [{post, <<"/c">>, my_controller, #{check_origin => false}}]),
    {_Handler, Opts} = route_match(~"POST", <<"/c">>),
    ?assertNot(lists:member({arizona_middleware, check_origin}, maps:get(middlewares, Opts))).

compile_routes_method_gating(Config) ->
    %% A verb route only answers its verb; another method on the same path is 405,
    %% and the Allow set is exactly the route's methods.
    compile(Config, [{post, <<"/only-post">>, my_controller, #{}}]),
    ?assertMatch({ok, _, _, _, _}, match_result(~"POST", <<"/only-post">>)),
    ?assertEqual({method_not_allowed, [~"POST"]}, match_result(~"GET", <<"/only-post">>)).

compile_routes_match_any_and_list(Config) ->
    %% `match` with `'*'` answers every method; with a list it gates to that set
    %% (and a GET allowlist implicitly answers HEAD).
    compile(Config, [
        {match, '*', <<"/any">>, my_controller, #{}},
        {match, [get, post], <<"/multi">>, my_controller, #{}}
    ]),
    ?assertMatch({ok, _, _, _, _}, match_result(~"DELETE", <<"/any">>)),
    ?assertMatch({ok, _, _, _, _}, match_result(~"GET", <<"/multi">>)),
    ?assertMatch({ok, _, _, _, _}, match_result(~"HEAD", <<"/multi">>)),
    ?assertEqual(
        {method_not_allowed, [~"GET", ~"HEAD", ~"POST"]}, match_result(~"PUT", <<"/multi">>)
    ).

compile_routes_match_custom_verb(Config) ->
    %% `match` accepts an arbitrary atom verb (upper-cased) or a custom method
    %% binary -- both compile to the same single-method allowlist.
    compile(Config, [
        {match, move, <<"/dav-a">>, my_controller, #{}},
        {match, ~"PROPFIND", <<"/dav-b">>, my_controller, #{}}
    ]),
    ?assertMatch({ok, _, _, _, _}, match_result(~"MOVE", <<"/dav-a">>)),
    ?assertEqual({method_not_allowed, [~"MOVE"]}, match_result(~"GET", <<"/dav-a">>)),
    ?assertMatch({ok, _, _, _, _}, match_result(~"PROPFIND", <<"/dav-b">>)),
    ?assertEqual({method_not_allowed, [~"PROPFIND"]}, match_result(~"GET", <<"/dav-b">>)).

compile_routes_same_path_dispatch(Config) ->
    %% Two verb routes share a path: the method selects the handler.
    compile(Config, [
        {get, <<"/u">>, index_controller, #{}},
        {post, <<"/u">>, create_controller, #{}}
    ]),
    {_, GetOpts} = route_match(~"GET", <<"/u">>),
    {_, PostOpts} = route_match(~"POST", <<"/u">>),
    ?assertEqual(index_controller, maps:get(handler, GetOpts)),
    ?assertEqual(create_controller, maps:get(handler, PostOpts)).

compile_routes_per_listener_dispatch(_Config) ->
    %% Two listeners compile the SAME path to DIFFERENT handlers under their own
    %% name-scoped dispatch key. A WS upgrade/navigate must resolve against the
    %% routes of the listener that accepted it -- not whichever compiled last.
    %% Before the per-listener keying this shared a single global dispatch, so
    %% listener_a resolved to handler_b (the last compile wins).
    ok = arizona_roadrunner_router:compile_routes(
        [{live, ~"/shared", handler_a, #{}}], #{}, listener_a
    ),
    ok = arizona_roadrunner_router:compile_routes(
        [{live, ~"/shared", handler_b, #{}}], #{}, listener_b
    ),
    try
        ?assertMatch(
            {ok, handler_a, _, _},
            arizona_roadrunner_req:resolve_route(
                ~"/shared", <<>>, listener_req(listener_a, ~"/shared")
            )
        ),
        ?assertMatch(
            {ok, handler_b, _, _},
            arizona_roadrunner_req:resolve_route(
                ~"/shared", <<>>, listener_req(listener_b, ~"/shared")
            )
        )
    after
        ok = arizona_roadrunner_router:forget_routes(listener_a),
        ok = arizona_roadrunner_router:forget_routes(listener_b)
    end.

compile_routes_per_listener_error_page(_Config) ->
    %% Each listener's `error_page` (threaded via BuildOpts) is baked into its own
    %% live routes, so the render-crash path serves the owning listener's error
    %% page. Before the fix this was one global term the last listener overwrote
    %% (and either listener's stop/1 erased).
    ok = arizona_roadrunner_router:compile_routes(
        [{live, ~"/e", handler_a, #{}}], #{error_page => {page_a, render}}, listener_a
    ),
    ok = arizona_roadrunner_router:compile_routes(
        [{live, ~"/e", handler_b, #{}}], #{error_page => {page_b, render}}, listener_b
    ),
    try
        {ok, handler_a, OptsA, _} =
            arizona_roadrunner_req:resolve_route(~"/e", <<>>, listener_req(listener_a, ~"/e")),
        {ok, handler_b, OptsB, _} =
            arizona_roadrunner_req:resolve_route(~"/e", <<>>, listener_req(listener_b, ~"/e")),
        ?assertEqual({page_a, render}, maps:get(error_page, OptsA)),
        ?assertEqual({page_b, render}, maps:get(error_page, OptsB))
    after
        ok = arizona_roadrunner_router:forget_routes(listener_a),
        ok = arizona_roadrunner_router:forget_routes(listener_b)
    end.

compile_routes_mcp(Config) ->
    compile(Config, [
        {mcp, <<"/mcp">>, my_mcp, #{origins => [<<"http://localhost:3000">>]}}
    ]),
    {Handler, Opts} = route_match(<<"/mcp">>),
    ?assertEqual(arizona_mcp_handler, Handler),
    %% The app handler module is folded into the per-route opts so the
    %% transport reads it at request time.
    ?assertEqual(my_mcp, maps:get(handler, Opts)),
    ?assertEqual([<<"http://localhost:3000">>], maps:get(origins, Opts)).

compile_routes_mixed(Config) ->
    compile(Config, [
        {live, <<"/">>, page_h, #{layouts => [{lay, render}]}},
        {ws, <<"/ws">>, #{timeout => 30}},
        {asset, <<"/assets">>, {dir, "/var/www"}},
        {post, <<"/health">>, health_h, #{state => #{status => ok}}}
    ]),
    {H1, _} = route_match(<<"/">>),
    ?assertEqual(?config(http_handler, Config), H1),
    {H2, Opts2} = route_match(<<"/ws">>),
    ?assertEqual(?config(ws_handler, Config), H2),
    ?assertEqual(#{timeout => 30}, Opts2),
    {H3, Opts3} = route_match(<<"/assets/style.css">>),
    ?assertEqual(?config(static_handler, Config), H3),
    ?assertEqual(#{dir => "/var/www"}, Opts3),
    {H4, Opts4} = route_match(~"POST", <<"/health">>),
    ?assertEqual(arizona_roadrunner_controller, H4),
    ?assertEqual(health_h, maps:get(handler, Opts4)),
    ?assertEqual(#{status => ok}, maps:get(state, Opts4)).
