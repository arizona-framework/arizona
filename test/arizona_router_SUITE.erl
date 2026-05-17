-module(arizona_router_SUITE).
-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").
%% The cowboy branch of route_match/2 hands cowboy_router:execute a
%% synthesized request map (not a real cowboy_req); silence the
%% spec-violation warning that produces.
-dialyzer({nowarn_function, [route_match/2]}).

-export([all/0, groups/0, init_per_group/2, end_per_group/2]).
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
    [{group, roadrunner}, {group, cowboy}].

groups() ->
    Cases = [
        compile_routes_live,
        compile_routes_live_no_layout,
        compile_routes_live_multiple_pages,
        compile_routes_ws,
        compile_routes_asset_dir,
        compile_routes_asset_priv_dir,
        compile_routes_controller,
        compile_routes_mixed
    ],
    [
        {roadrunner, [sequence], Cases},
        {cowboy, [sequence], Cases}
    ].

init_per_group(roadrunner, Config) ->
    [
        {adapter, roadrunner},
        {router, arizona_roadrunner_router},
        {http_handler, arizona_roadrunner_http},
        {ws_handler, arizona_roadrunner_ws},
        {static_handler, arizona_roadrunner_static}
        | Config
    ];
init_per_group(cowboy, Config) ->
    [
        {adapter, cowboy},
        {router, arizona_cowboy_router},
        {http_handler, arizona_cowboy_http},
        {ws_handler, arizona_cowboy_ws},
        {static_handler, arizona_cowboy_static}
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

%% Resolve a request path to `{Handler, Opts}` via the configured
%% adapter's runtime lookup. For cowboy that's `cowboy_router:execute/2`;
%% for roadrunner it's `roadrunner_router:match/2` against the persistent
%% term key arizona writes — the per-route state (5th element)
%% is what arizona stashes its metadata in.
route_match(Config, Path) ->
    case ?config(adapter, Config) of
        cowboy ->
            {ok, _ResolvedReq, Env} = cowboy_router:execute(
                #{host => <<"localhost">>, path => Path},
                #{dispatch => {persistent_term, arizona_dispatch}}
            ),
            {maps:get(handler, Env), maps:get(handler_opts, Env)};
        roadrunner ->
            Compiled = persistent_term:get(arizona_roadrunner_dispatch),
            {ok, Handler, _Bindings, _Pipeline, State} =
                roadrunner_router:match(Path, Compiled),
            %% arizona's per-route opts live under the `arizona`
            %% namespace inside `state` to keep them opaque from
            %% roadrunner's pipeline. Unwrap so the parameterized
            %% assertions can read handler/layouts/etc. directly.
            %% asset/controller routes have no arizona wrap and expose
            %% their opts at the top level.
            case State of
                #{arizona := ArzOpts} -> {Handler, ArzOpts};
                _ -> {Handler, State}
            end
    end.

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
    {Handler, Opts} = route_match(Config, <<"/foo">>),
    ?assertEqual(?config(http_handler, Config), Handler),
    ?assertEqual(my_handler, maps:get(handler, Opts)),
    ?assertEqual([{my_layout, render}], maps:get(layouts, Opts)),
    ?assertEqual(#{page => home}, maps:get(bindings, Opts)).

compile_routes_live_no_layout(Config) ->
    compile(Config, [
        {live, <<"/bare">>, my_handler, #{bindings => #{x => 1}}}
    ]),
    {Handler, Opts} = route_match(Config, <<"/bare">>),
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
    {H1, Opts1} = route_match(Config, <<"/page1">>),
    {H2, Opts2} = route_match(Config, <<"/page2">>),
    Expected = ?config(http_handler, Config),
    ?assertEqual(Expected, H1),
    ?assertEqual(Expected, H2),
    ?assertEqual(handler1, maps:get(handler, Opts1)),
    ?assertEqual(handler2, maps:get(handler, Opts2)),
    ?assertEqual(#{id => <<"p1">>}, maps:get(bindings, Opts1)),
    ?assertEqual(#{id => <<"p2">>}, maps:get(bindings, Opts2)).

compile_routes_ws(Config) ->
    compile(Config, [
        {ws, <<"/ws">>, #{idle_timeout => 60000}}
    ]),
    {Handler, Opts} = route_match(Config, <<"/ws">>),
    ?assertEqual(?config(ws_handler, Config), Handler),
    ?assertEqual(#{idle_timeout => 60000}, Opts).

compile_routes_asset_dir(Config) ->
    compile(Config, [
        {asset, <<"/static">>, {dir, "/tmp/assets"}}
    ]),
    {Handler, Opts} = route_match(Config, <<"/static/foo.js">>),
    ?assertEqual(?config(static_handler, Config), Handler),
    ?assertEqual(#{dir => "/tmp/assets"}, Opts).

compile_routes_asset_priv_dir(Config) ->
    compile(Config, [
        {asset, <<"/priv">>, {priv_dir, arizona, "static/assets/js"}}
    ]),
    {Handler, Opts} = route_match(Config, <<"/priv/arizona.min.js">>),
    ExpectedDir = filename:join(code:priv_dir(arizona), "static/assets/js"),
    ?assertEqual(?config(static_handler, Config), Handler),
    ?assertEqual(#{dir => ExpectedDir}, Opts).

compile_routes_controller(Config) ->
    compile(Config, [
        {controller, <<"/api/health">>, my_controller, #{key => val}}
    ]),
    {Handler, Opts} = route_match(Config, <<"/api/health">>),
    ?assertEqual(my_controller, Handler),
    ?assertEqual(#{key => val}, Opts).

compile_routes_mixed(Config) ->
    compile(Config, [
        {live, <<"/">>, page_h, #{layouts => [{lay, render}]}},
        {ws, <<"/ws">>, #{timeout => 30}},
        {asset, <<"/assets">>, {dir, "/var/www"}},
        {controller, <<"/health">>, health_h, #{status => ok}}
    ]),
    {H1, _} = route_match(Config, <<"/">>),
    ?assertEqual(?config(http_handler, Config), H1),
    {H2, Opts2} = route_match(Config, <<"/ws">>),
    ?assertEqual(?config(ws_handler, Config), H2),
    ?assertEqual(#{timeout => 30}, Opts2),
    {H3, Opts3} = route_match(Config, <<"/assets/style.css">>),
    ?assertEqual(?config(static_handler, Config), H3),
    ?assertEqual(#{dir => "/var/www"}, Opts3),
    {H4, Opts4} = route_match(Config, <<"/health">>),
    ?assertEqual(health_h, H4),
    ?assertEqual(#{status => ok}, Opts4).
