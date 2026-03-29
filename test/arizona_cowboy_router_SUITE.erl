-module(arizona_cowboy_router_SUITE).
-include_lib("stdlib/include/assert.hrl").
-dialyzer(
    {nowarn_function, [
        route_match/1,
        compile_routes_live/1,
        compile_routes_live_no_layout/1,
        compile_routes_live_multiple_pages/1,
        compile_routes_ws/1,
        compile_routes_asset_dir/1,
        compile_routes_asset_priv_dir/1,
        compile_routes_controller/1,
        compile_routes_mixed/1
    ]}
).

%% Helper: resolve route via arizona_cowboy_adapter to get both handler and opts.

-export([all/0, groups/0]).
-export([
    compile_routes_asset_dir/1,
    compile_routes_asset_priv_dir/1,
    compile_routes_controller/1,
    compile_routes_live/1,
    compile_routes_live_no_layout/1,
    compile_routes_live_multiple_pages/1,
    compile_routes_mixed/1,
    compile_routes_ws/1
]).

all() ->
    [{group, routes}].

groups() ->
    [
        {routes, [sequence], [
            compile_routes_live,
            compile_routes_live_no_layout,
            compile_routes_live_multiple_pages,
            compile_routes_ws,
            compile_routes_asset_dir,
            compile_routes_asset_priv_dir,
            compile_routes_controller,
            compile_routes_mixed
        ]}
    ].

route_match(Path) ->
    {Handler, _ResolvedReq, Opts} = arizona_cowboy_adapter:resolve_cowboy_route(#{
        host => <<"localhost">>, path => Path
    }),
    {Handler, Opts}.

compile_routes_live(Config) when is_list(Config) ->
    ok = arizona_cowboy_router:compile_routes([
        {live, <<"/foo">>, my_handler, #{
            bindings => #{page => home},
            layout => {my_layout, render}
        }}
    ]),
    {Handler, Opts} = route_match(<<"/foo">>),
    ?assertEqual(arizona_cowboy_http, Handler),
    ?assertEqual(my_handler, maps:get(handler, Opts)),
    ?assertEqual({my_layout, render}, maps:get(layout, Opts)),
    ?assertEqual(#{page => home}, maps:get(bindings, Opts)).

compile_routes_live_no_layout(Config) when is_list(Config) ->
    ok = arizona_cowboy_router:compile_routes([
        {live, <<"/bare">>, my_handler, #{bindings => #{x => 1}}}
    ]),
    {Handler, Opts} = route_match(<<"/bare">>),
    ?assertEqual(arizona_cowboy_http, Handler),
    ?assertEqual(my_handler, maps:get(handler, Opts)),
    ?assertEqual(undefined, maps:get(layout, Opts)),
    ?assertEqual(#{x => 1}, maps:get(bindings, Opts)).

compile_routes_live_multiple_pages(Config) when is_list(Config) ->
    RouteOpts = fun(Id) ->
        #{bindings => #{id => Id}, layout => {layout, render}}
    end,
    ok = arizona_cowboy_router:compile_routes([
        {live, <<"/page1">>, handler1, RouteOpts(<<"p1">>)},
        {live, <<"/page2">>, handler2, RouteOpts(<<"p2">>)}
    ]),
    {H1, Opts1} = route_match(<<"/page1">>),
    {H2, Opts2} = route_match(<<"/page2">>),
    ?assertEqual(arizona_cowboy_http, H1),
    ?assertEqual(arizona_cowboy_http, H2),
    ?assertEqual(handler1, maps:get(handler, Opts1)),
    ?assertEqual(handler2, maps:get(handler, Opts2)),
    ?assertEqual(#{id => <<"p1">>}, maps:get(bindings, Opts1)),
    ?assertEqual(#{id => <<"p2">>}, maps:get(bindings, Opts2)).

compile_routes_ws(Config) when is_list(Config) ->
    ok = arizona_cowboy_router:compile_routes([
        {ws, <<"/ws">>, #{idle_timeout => 60000}}
    ]),
    {Handler, Opts} = route_match(<<"/ws">>),
    ?assertEqual(arizona_cowboy_ws, Handler),
    ?assertEqual(#{idle_timeout => 60000}, Opts).

compile_routes_asset_dir(Config) when is_list(Config) ->
    ok = arizona_cowboy_router:compile_routes([
        {asset, <<"/static">>, {dir, "/tmp/assets"}}
    ]),
    {Handler, Opts} = route_match(<<"/static/foo.js">>),
    ?assertEqual(arizona_cowboy_static, Handler),
    ?assertEqual(#{dir => "/tmp/assets"}, Opts).

compile_routes_asset_priv_dir(Config) when is_list(Config) ->
    ok = arizona_cowboy_router:compile_routes([
        {asset, <<"/priv">>, {priv_dir, arizona, "static/assets/js"}}
    ]),
    {Handler, Opts} = route_match(<<"/priv/arizona.min.js">>),
    ExpectedDir = filename:join(code:priv_dir(arizona), "static/assets/js"),
    ?assertEqual(arizona_cowboy_static, Handler),
    ?assertEqual(#{dir => ExpectedDir}, Opts).

compile_routes_controller(Config) when is_list(Config) ->
    ok = arizona_cowboy_router:compile_routes([
        {controller, <<"/api/health">>, my_controller, #{key => val}}
    ]),
    {Handler, Opts} = route_match(<<"/api/health">>),
    ?assertEqual(my_controller, Handler),
    ?assertEqual(#{key => val}, Opts).

compile_routes_mixed(Config) when is_list(Config) ->
    ok = arizona_cowboy_router:compile_routes([
        {live, <<"/">>, page_h, #{layout => {lay, render}}},
        {ws, <<"/ws">>, #{timeout => 30}},
        {asset, <<"/assets">>, {dir, "/var/www"}},
        {controller, <<"/health">>, health_h, #{status => ok}}
    ]),
    {H1, _} = route_match(<<"/">>),
    ?assertEqual(arizona_cowboy_http, H1),
    {H2, Opts2} = route_match(<<"/ws">>),
    ?assertEqual(arizona_cowboy_ws, H2),
    ?assertEqual(#{timeout => 30}, Opts2),
    {H3, Opts3} = route_match(<<"/assets/style.css">>),
    ?assertEqual(arizona_cowboy_static, H3),
    ?assertEqual(#{dir => "/var/www"}, Opts3),
    {H4, Opts4} = route_match(<<"/health">>),
    ?assertEqual(health_h, H4),
    ?assertEqual(#{status => ok}, Opts4).
