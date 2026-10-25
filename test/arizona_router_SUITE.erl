-module(arizona_router_SUITE).
-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

%% The listener name the suite compiles its routes under -- compile_routes/3 is
%% listener-scoped (there is no nameless global form).
-define(LISTENER, arizona_router_suite).

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
    compile_routes_match_wildcard_in_list_errors/1,
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
    compile_routes_live_method_gating/1,
    compile_routes_mixed/1,
    compile_routes_http_transforms/1,
    compile_routes_http_transform_errors/1,
    compile_routes_cors_opens_options/1,
    compile_routes_cors_keeps_declared_options/1,
    compile_routes_cors_defers_to_declared_options/1,
    compile_routes_cors_shadowing_declared_options_errors/1,
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
        compile_routes_live_method_gating,
        compile_routes_ws,
        compile_routes_asset_dir,
        compile_routes_asset_priv_dir,
        compile_routes_asset_cache_control,
        compile_routes_controller,
        compile_routes_controller_default_origin_check,
        compile_routes_controller_opts_out_origin_check,
        compile_routes_method_gating,
        compile_routes_match_any_and_list,
        compile_routes_match_wildcard_in_list_errors,
        compile_routes_match_custom_verb,
        compile_routes_same_path_dispatch,
        compile_routes_per_listener_dispatch,
        compile_routes_per_listener_error_page,
        compile_routes_mcp,
        compile_routes_mixed,
        compile_routes_http_transforms,
        compile_routes_http_transform_errors,
        compile_routes_cors_opens_options,
        compile_routes_cors_keeps_declared_options,
        compile_routes_cors_defers_to_declared_options,
        compile_routes_cors_shadowing_declared_options_errors
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
    ok = Router:compile_routes(Routes, #{}, ?LISTENER).

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
    Compiled = persistent_term:get({arizona_roadrunner_dispatch, ?LISTENER}),
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

compile_routes_live_method_gating(Config) ->
    %% A live route answers GET (the page render) plus HEAD only; the WebSocket
    %% upgrade rides its own `{ws, ...}` route. A non-GET to a live path is 405 with
    %% an Allow of exactly the live methods -- decided by the router before any
    %% middleware, so a mutating verb (a cross-site POST to a page) never reaches a
    %% handler that only renders. Controller 405s are covered above; this guards the
    %% same gate on the `{live, ...}` kind.
    compile(Config, [{live, <<"/dash">>, my_handler, #{}}]),
    ?assertMatch({ok, _, _, _, _}, match_result(~"GET", <<"/dash">>)),
    ?assertMatch({ok, _, _, _, _}, match_result(~"HEAD", <<"/dash">>)),
    ?assertEqual({method_not_allowed, [~"GET", ~"HEAD"]}, match_result(~"POST", <<"/dash">>)).

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
    ?assertEqual(#{dir => "/tmp/assets"}, Opts),
    %% roadrunner_static serves whatever the method, so the allowlist is
    %% what turns a non-read verb into a 405.
    ?assertMatch({ok, _, _, _, _}, match_result(~"HEAD", <<"/static/foo.js">>)),
    ?assertEqual(
        {method_not_allowed, [~"GET", ~"HEAD"]}, match_result(~"POST", <<"/static/foo.js">>)
    ),
    compile(Config, [
        {asset, <<"/static">>, {dir, "/tmp/assets"}, #{
            middlewares => [arizona_middleware:cors(#{origins => [~"https://app.example"]})]
        }}
    ]),
    ?assertMatch({ok, _, _, _, _}, match_result(~"OPTIONS", <<"/static/foo.js">>)).

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

compile_routes_match_wildcard_in_list_errors(Config) ->
    %% '*' means "any method" only as the WHOLE spec. Inside a method list it
    %% used to silently normalize to the literal binary ~"*" -- a method no real
    %% request carries (while the 405 Allow header advertised "*") -- so route
    %% compilation now fails loudly instead. The binary spelling is the same
    %% trap, listed or bare.
    Router = ?config(router, Config),
    ?assertError(
        wildcard_in_method_list,
        Router:routes([{match, [get, '*'], <<"/w">>, my_controller, #{}}])
    ),
    ?assertError(
        wildcard_in_method_list,
        Router:routes([{match, [~"*"], <<"/w">>, my_controller, #{}}])
    ),
    ?assertError(
        wildcard_in_method_list,
        Router:routes([{match, ~"*", <<"/w">>, my_controller, #{}}])
    ),
    %% The bare-atom wildcard stays the supported any-method form.
    ?assertMatch(
        [#{methods := undefined}],
        Router:routes([{match, '*', <<"/w">>, my_controller, #{}}])
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
    {Handler, Controller} = route_match(<<"/mcp">>),
    %% A controller route running `arizona_mcp_handler`, with the app handler
    %% module folded into its state so the transport reads it at request time.
    ?assertEqual(arizona_roadrunner_controller, Handler),
    ?assertMatch(#{handler := arizona_mcp_handler, middlewares := []}, Controller),
    #{state := State} = Controller,
    ?assertEqual(my_mcp, maps:get(handler, State)),
    ?assertEqual([<<"http://localhost:3000">>], maps:get(origins, State)).

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

compile_routes_http_transforms(Config) when is_list(Config) ->
    %% A route's single `middlewares` list holds two kinds of entry; the
    %% router splits them at compile time. HTTP response transforms
    %% (arizona_middleware constructors) map to roadrunner middlewares at the
    %% route's TOP-LEVEL `middlewares` key -- roadrunner's own pipeline --
    %% while request-to-bindings steps stay nested under `state.arizona`.
    %% The framework's compress is PREPENDED, staying outermost, so a
    %% transform sees the response before compress re-encodes it.
    Router = ?config(router, Config),
    %% Live: compress outermost, the transform inside.
    ?assertMatch(
        [#{middlewares := [roadrunner_compress, roadrunner_etag]}],
        Router:routes([
            {live, ~"/", my_page, #{middlewares => [arizona_middleware:etag()]}}
        ])
    ),
    %% Live with compress off: the transform stands alone.
    ?assertMatch(
        [#{middlewares := [roadrunner_etag]}],
        Router:routes(
            [{live, ~"/", my_page, #{middlewares => [arizona_middleware:etag()]}}],
            #{compress => false}
        )
    ),
    %% compress() is position-independent: the router keeps compression
    %% OUTERMOST wherever it is written, so an etag always hashes the
    %% pre-compression body -- and it is never doubled.
    ?assertMatch(
        [#{middlewares := [roadrunner_compress, roadrunner_etag]}],
        Router:routes([
            {live, ~"/", my_page, #{
                middlewares => [arizona_middleware:etag(), arizona_middleware:compress()]
            }}
        ])
    ),
    %% An explicit compress() survives the server's compress => false (a
    %% route-level request beats the listener-level default-off) -- still
    %% outermost.
    ?assertMatch(
        [#{middlewares := [roadrunner_compress, roadrunner_etag]}],
        Router:routes(
            [
                {live, ~"/", my_page, #{
                    middlewares => [arizona_middleware:etag(), arizona_middleware:compress()]
                }}
            ],
            #{compress => false}
        )
    ),
    %% The literal term is the constructor's documented product -- a
    %% sys.config route spells exactly this.
    ?assertMatch(
        [#{middlewares := [roadrunner_compress, roadrunner_etag]}],
        Router:routes([{live, ~"/", my_page, #{middlewares => [{http_transform, etag}]}}])
    ),
    %% Without transforms a live route carries compress alone -- unchanged.
    ?assertMatch(
        [#{middlewares := [roadrunner_compress]}],
        Router:routes([{live, ~"/", my_page, #{}}])
    ),
    %% Config-carrying constructors map through with their config.
    ?assertMatch(
        [
            #{
                middlewares := [
                    roadrunner_compress,
                    {roadrunner_cors, #{origins := [~"https://app.example"]}},
                    roadrunner_security_headers,
                    {roadrunner_security_headers, #{frame_options := ~"DENY"}},
                    OptionsReply
                ]
            }
        ] when is_function(OptionsReply, 3),
        Router:routes([
            {live, ~"/", my_page, #{
                middlewares => [
                    arizona_middleware:cors(#{origins => [~"https://app.example"]}),
                    arizona_middleware:security_headers(),
                    arizona_middleware:security_headers(#{frame_options => ~"DENY"})
                ]
            }}
        ])
    ),
    %% Controller: the split leaves Arizona steps (plus the default origin
    %% check) nested in state while the transform reaches roadrunner's
    %% pipeline.
    [ControllerRoute] = Router:routes([
        {post, ~"/api", my_controller, #{
            middlewares => [{my_mod, my_step}, arizona_middleware:etag()]
        }}
    ]),
    ?assertMatch(#{middlewares := [roadrunner_etag]}, ControllerRoute),
    #{state := #{arizona := #{middlewares := ArizonaMws}}} = ControllerRoute,
    ?assertMatch([{arizona_middleware, check_origin}, {my_mod, my_step}], ArizonaMws),
    %% Without transforms a controller route has no roadrunner middlewares key.
    [BareController] = Router:routes([{post, ~"/api", my_controller, #{}}]),
    ?assertNot(is_map_key(middlewares, BareController)),
    %% Controllers get no automatic compression, but an explicit compress()
    %% still normalizes outermost.
    ?assertMatch(
        [#{middlewares := [roadrunner_compress, roadrunner_etag]}],
        Router:routes([
            {post, ~"/api", my_controller, #{
                middlewares => [arizona_middleware:etag(), arizona_middleware:compress()]
            }}
        ])
    ),
    %% A {match, ...} multi-verb route splits identically to the verb sugar.
    ?assertMatch(
        [#{methods := [~"GET", ~"POST", ~"HEAD"], middlewares := [roadrunner_etag]}],
        Router:routes([
            {match, [get, post], ~"/multi", my_controller, #{
                middlewares => [arizona_middleware:etag()]
            }}
        ])
    ),
    %% The documented CORS pairing: serve cross-origin deliberately -- the
    %% cors transform on the roadrunner side, check_origin opted out on
    %% Arizona's.
    [CorsRoute] = Router:routes([
        {post, ~"/public-api", my_controller, #{
            check_origin => false,
            middlewares => [arizona_middleware:cors(#{origins => [~"https://app.example"]})]
        }}
    ]),
    ?assertMatch(
        #{
            methods := [~"POST", ~"OPTIONS"],
            middlewares := [
                {roadrunner_cors, #{origins := [~"https://app.example"]}},
                OptionsReply
            ]
        } when is_function(OptionsReply, 3),
        CorsRoute
    ),
    #{state := #{arizona := #{middlewares := CorsArizonaMws}}} = CorsRoute,
    ?assertEqual([], CorsArizonaMws),
    %% A live route's compiled meta -- what the WS upgrade and navigate read
    %% their middlewares from -- carries the steps only; transforms never
    %% reach Arizona's pipeline on those paths.
    [LiveRoute] = Router:routes([
        {live, ~"/mixed", my_page, #{
            middlewares => [{my_mod, my_step}, arizona_middleware:etag()]
        }}
    ]),
    #{state := #{arizona := #{middlewares := LiveMws}}} = LiveRoute,
    ?assertMatch([{arizona_middleware, check_origin}, {my_mod, my_step}], LiveMws),
    %% Asset: transforms fine, an Arizona step fails loudly -- no pipeline
    %% would ever run it there.
    ?assertMatch(
        [#{middlewares := [roadrunner_compress, roadrunner_security_headers]}],
        Router:routes([
            {asset, ~"/a", {dir, "/tmp"}, #{
                middlewares => [arizona_middleware:security_headers()]
            }}
        ])
    ),
    ?assertError(
        {asset_middleware_not_transform, [{my_mod, my_step}]},
        Router:routes([
            {asset, ~"/a", {dir, "/tmp"}, #{middlewares => [{my_mod, my_step}]}}
        ])
    ),
    %% A transform reaching the Arizona pipeline anyway (a custom
    %% resolve_route result) is inert, not a crash: the step after it still
    %% runs and the pipeline conts.
    Req = arizona_req_test_adapter:new(),
    ?assertMatch(
        {cont, _, #{method := _}},
        arizona_middleware:apply_middlewares(
            [arizona_middleware:etag(), arizona_middleware:extract([method])],
            Req,
            #{}
        )
    ).

compile_routes_http_transform_errors(Config) when is_list(Config) ->
    %% A typo'd hand-spelled transform (the sys.config hazard) fails route
    %% compilation with a named error, and format_error/2 renders a message
    %% naming the valid set -- same convention as the asset guard below.
    Router = ?config(router, Config),
    ?assertError(
        {unknown_http_transform, etga},
        Router:routes([{live, ~"/", my_page, #{middlewares => [{http_transform, etga}]}}])
    ),
    Stack = [{arizona_route, transforms, 2, []}],
    #{general := UnknownMsg} =
        arizona_route:format_error({unknown_http_transform, etga}, Stack),
    ?assertNotEqual(nomatch, binary:match(iolist_to_binary(UnknownMsg), ~"etga")),
    ?assertNotEqual(nomatch, binary:match(iolist_to_binary(UnknownMsg), ~"etag")),
    #{general := AssetMsg} =
        arizona_route:format_error({asset_middleware_not_transform, [{my_mod, my_step}]}, Stack),
    ?assertNotEqual(nomatch, binary:match(iolist_to_binary(AssetMsg), ~"my_mod")),
    #{general := WildcardMsg} = arizona_route:format_error(wildcard_in_method_list, Stack),
    ?assertNotEqual(nomatch, binary:match(iolist_to_binary(WildcardMsg), ~"'*'")).

compile_routes_cors_opens_options(Config) ->
    %% roadrunner 405s a method outside a route's allowlist before any per-route
    %% middleware runs, so a cors route must allow OPTIONS for its transform to
    %% see a preflight. A sibling sharing the path keeps its own verbs, and the
    %% 405 Allow set now advertises OPTIONS.
    Cors = arizona_middleware:cors(#{origins => [~"https://app.example"]}),
    compile(Config, [
        {post, <<"/cors-api">>, my_controller, #{middlewares => [Cors]}},
        {delete, <<"/cors-api">>, my_controller, #{}},
        {live, <<"/cors-page">>, my_page, #{middlewares => [Cors]}},
        {post, <<"/plain-api">>, my_controller, #{}}
    ]),
    ?assertMatch({ok, _, _, _, _}, match_result(~"OPTIONS", <<"/cors-api">>)),
    ?assertMatch({ok, _, _, _, _}, match_result(~"DELETE", <<"/cors-api">>)),
    ?assertEqual(
        {method_not_allowed, [~"DELETE", ~"OPTIONS", ~"POST"]},
        match_result(~"PUT", <<"/cors-api">>)
    ),
    ?assertMatch({ok, _, _, _, _}, match_result(~"OPTIONS", <<"/cors-page">>)),
    ?assertEqual(
        {method_not_allowed, [~"GET", ~"HEAD", ~"OPTIONS"]},
        match_result(~"POST", <<"/cors-page">>)
    ),
    %% No cors, no change.
    ?assertEqual({method_not_allowed, [~"POST"]}, match_result(~"OPTIONS", <<"/plain-api">>)),
    %% The plain-OPTIONS reply sits innermost, under the transform (compress
    %% stays outermost).
    Router = ?config(router, Config),
    ?assertMatch(
        [
            #{
                methods := [~"GET", ~"HEAD", ~"OPTIONS"],
                middlewares := [roadrunner_compress, {roadrunner_cors, _}, OptionsReply]
            }
        ] when is_function(OptionsReply, 3),
        Router:routes([{live, <<"/cors-page">>, my_page, #{middlewares => [Cors]}}])
    ).

compile_routes_cors_keeps_declared_options(Config) ->
    %% A route that declares OPTIONS itself, or answers any method, already
    %% reaches the transform; its handler owns a plain OPTIONS, so nothing is
    %% added.
    Router = ?config(router, Config),
    Cors = arizona_middleware:cors(#{origins => [~"https://app.example"]}),
    ?assertMatch(
        [#{methods := [~"POST", ~"OPTIONS"], middlewares := [{roadrunner_cors, _}]}],
        Router:routes([
            {match, [post, options], <<"/declared">>, my_controller, #{middlewares => [Cors]}}
        ])
    ),
    ?assertMatch(
        [#{methods := undefined, middlewares := [{roadrunner_cors, _}]}],
        Router:routes([{match, '*', <<"/any">>, my_controller, #{middlewares => [Cors]}}])
    ).

compile_routes_cors_defers_to_declared_options(Config) ->
    %% A route declaring OPTIONS listed first keeps the URLs it matches; the cors
    %% route still opens OPTIONS for everything else.
    Cors = arizona_middleware:cors(#{origins => [~"https://app.example"]}),
    compile(Config, [
        {options, <<"/o2">>, options_controller, #{}},
        {post, <<"/o2/:id">>, cors_controller, #{middlewares => [Cors]}},
        {post, <<"/o2">>, cors_controller, #{middlewares => [Cors]}}
    ]),
    ?assertMatch(
        {arizona_roadrunner_controller, #{handler := options_controller}},
        route_match(~"OPTIONS", <<"/o2">>)
    ),
    ?assertMatch({ok, _, _, _, _}, match_result(~"POST", <<"/o2">>)),
    ?assertMatch(
        {arizona_roadrunner_controller, #{handler := cors_controller}},
        route_match(~"OPTIONS", <<"/o2/42">>)
    ).

compile_routes_cors_shadowing_declared_options_errors(Config) ->
    %% Listed after the cors route, a declared OPTIONS route would be
    %% unreachable once OPTIONS opens. Keeping OPTIONS closed instead would
    %% silently 405 every preflight to the cors route, so compilation fails,
    %% naming both routes -- for an identical path and a partial overlap alike.
    Router = ?config(router, Config),
    Cors = arizona_middleware:cors(#{origins => [~"https://app.example"]}),
    Shadowing = fun(CorsPath, OptionsPath) ->
        [
            {get, <<"/other">>, my_controller, #{}},
            {post, CorsPath, cors_controller, #{middlewares => [Cors]}},
            {options, OptionsPath, options_controller, #{}}
        ]
    end,
    ?assertError(
        {cors_options_shadows_route, {2, <<"/o1">>}, {3, <<"/o1">>}},
        Router:routes(Shadowing(<<"/o1">>, <<"/o1">>))
    ),
    ?assertError(
        {cors_options_shadows_route, {2, <<"/u/:id">>}, {3, <<"/u/me">>}},
        Router:routes(Shadowing(<<"/u/:id">>, <<"/u/me">>))
    ),
    #{general := Msg} = Router:format_error(
        {cors_options_shadows_route, {2, <<"/u/:id">>}, {3, <<"/u/me">>}},
        [{Router, routes, 1, []}]
    ),
    Bin = iolist_to_binary(Msg),
    [
        ?assertNotEqual(nomatch, binary:match(Bin, Part))
     || Part <- [~"/u/:id", ~"/u/me", ~"#2", ~"#3", ~"first"]
    ],
    ok.
