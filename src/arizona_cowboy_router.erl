-module(arizona_cowboy_router).
-moduledoc """
Compiles a list of Arizona route specs into a Cowboy dispatch table.

The resulting dispatch is stored under the persistent term key
`arizona_dispatch`, where Cowboy and `arizona_cowboy_req` look it
up. Calling `compile_routes/1` again replaces the previous dispatch,
which is how `arizona_cowboy_server:recompile_routes/0` picks up new
routes after a hot reload without restarting the listener.

## Route shapes

| Tag | Description | Cowboy handler |
|-----|-------------|----------------|
| `{live, Path, Handler, Opts}` | Arizona stateful page | `arizona_cowboy_http` |
| `{ws, Path, Opts}` | WebSocket endpoint | `arizona_cowboy_ws` |
| `{asset, Path, {dir, Dir}}` | Static files from directory | `arizona_cowboy_static` |
| `{asset, Path, {priv_dir, App, SubDir}}` | Static files from app priv | `arizona_cowboy_static` |
| `{controller, Path, Handler, State}` | Plain Cowboy handler | `Handler` |
| `{reload, Path, Opts}` | Dev SSE reload endpoint | `arizona_cowboy_reload` |

`{reload, ...}` also stashes the path in the `arizona_reload_url`
persistent term so the dev error page can build the SSE connect URL.
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([compile_routes/1]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([path/0]).
-export_type([live_opts/0]).
-export_type([route/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-nominal path() :: binary().

-nominal live_opts() :: #{
    layouts => [arizona_render:layout()],
    bindings => map()
}.

-nominal route() ::
    {live, path(), module(), live_opts()}
    | {ws, path(), map()}
    | {asset, path(), {dir, file:filename_all()}}
    | {asset, path(), {priv_dir, atom(), file:filename_all()}}
    | {controller, path(), module(), term()}
    | {reload, path(), map()}.

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc """
Compiles `Routes` into a Cowboy dispatch table and stores it under
the persistent term key `arizona_dispatch`. Replaces any previous
dispatch atomically.
""".
-spec compile_routes(Routes) -> ok when
    Routes :: [route()].
compile_routes(Routes) ->
    CowboyRoutes = lists:flatmap(fun route_to_cowboy/1, Routes),
    Dispatch = cowboy_router:compile([{'_', CowboyRoutes}]),
    persistent_term:put(arizona_dispatch, Dispatch),
    ok.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

route_to_cowboy({live, Path, Handler, Opts}) ->
    [
        {binary_to_list(Path), arizona_cowboy_http, #{
            handler => Handler,
            layouts => maps:get(layouts, Opts, []),
            bindings => maps:get(bindings, Opts, #{}),
            on_mount => maps:get(on_mount, Opts, []),
            middlewares => maps:get(middlewares, Opts, [])
        }}
    ];
route_to_cowboy({ws, Path, Opts}) ->
    [{binary_to_list(Path), arizona_cowboy_ws, Opts}];
route_to_cowboy({asset, Path, {dir, Dir}}) ->
    asset_route(Path, Dir);
route_to_cowboy({asset, Path, {priv_dir, App, SubDir}}) ->
    asset_route(Path, filename:join(code:priv_dir(App), SubDir));
route_to_cowboy({controller, Path, Handler, State}) ->
    [{binary_to_list(Path), Handler, State}];
route_to_cowboy({reload, Path, Opts}) ->
    persistent_term:put(arizona_reload_url, Path),
    [{binary_to_list(Path), arizona_cowboy_reload, Opts}].

asset_route(Path, Dir) ->
    [{binary_to_list(Path) ++ "/[...]", arizona_cowboy_static, #{dir => Dir}}].
