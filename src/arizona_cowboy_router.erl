-module(arizona_cowboy_router).
-export([compile_routes/1]).

-type path() :: binary().
-type live_opts() :: #{
    layout => arizona_render:layout(),
    bindings => map()
}.
-type route() ::
    {live, path(), module(), live_opts()}
    | {ws, path(), map()}
    | {asset, path(), {dir, file:filename_all()}}
    | {asset, path(), {priv_dir, atom(), file:filename_all()}}
    | {controller, path(), module(), term()}
    | {reload, path(), map()}.

-spec compile_routes([route()]) -> ok.
compile_routes(Routes) ->
    CowboyRoutes = lists:flatmap(fun route_to_cowboy/1, Routes),
    Dispatch = cowboy_router:compile([{'_', CowboyRoutes}]),
    persistent_term:put(arizona_dispatch, Dispatch),
    ok.

route_to_cowboy({live, Path, Handler, Opts}) ->
    [
        {binary_to_list(Path), arizona_cowboy_http, #{
            handler => Handler,
            layout => maps:get(layout, Opts, undefined),
            bindings => maps:get(bindings, Opts, #{}),
            on_mount => maps:get(on_mount, Opts, []),
            middlewares => maps:get(middlewares, Opts, [])
        }}
    ];
route_to_cowboy({ws, Path, Opts}) ->
    [{binary_to_list(Path), arizona_cowboy_ws, Opts}];
route_to_cowboy({asset, Path, {dir, Dir}}) ->
    [{binary_to_list(Path) ++ "/[...]", arizona_cowboy_static, #{dir => Dir}}];
route_to_cowboy({asset, Path, {priv_dir, App, SubDir}}) ->
    Dir = filename:join(code:priv_dir(App), SubDir),
    [{binary_to_list(Path) ++ "/[...]", arizona_cowboy_static, #{dir => Dir}}];
route_to_cowboy({controller, Path, Handler, State}) ->
    [{binary_to_list(Path), Handler, State}];
route_to_cowboy({reload, Path, Opts}) ->
    persistent_term:put(arizona_reload_url, Path),
    [{binary_to_list(Path), arizona_cowboy_reload, Opts}].
