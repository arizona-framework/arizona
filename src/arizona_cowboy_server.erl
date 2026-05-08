-module(arizona_cowboy_server).
-moduledoc """
Boots a Cowboy listener wired up with Arizona routes.

`start/2` compiles the route list, stashes the originals so they can
be re-compiled later, configures the dispatch persistent term, and
launches a clear or TLS listener depending on the `scheme` option.

`recompile_routes/0` rebuilds every Arizona-managed dispatch by
walking persistent terms tagged with the routes key. The dev hot
reloader calls this after a successful recompile so new or changed
routes take effect without restarting the listener.

## Options

- `routes` -- list of `t:arizona_cowboy_router:route/0` (required)
- `error_page` -- `{Module, Function}` to render the dev error page
  (default `{arizona_error_page, render}`)
- `scheme` -- `http` (default) or `https`
- `transport_opts` -- Cowboy ranch transport opts (default `[{port, 4040}]`)
- `proto_opts` -- Cowboy protocol opts; the dispatch is always
  injected on top of the user-provided env
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([start/2]).
-export([stop/1]).
-export([recompile_routes/0]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

-ignore_xref([start/2, stop/1]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------

-define(ROUTES_KEY, arizona_routes).

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc """
Starts a Cowboy listener named `Name` configured with Arizona routes
from `Opts`.
""".
-spec start(Name, Opts) -> {ok, pid()} | {error, term()} when
    Name :: atom(),
    Opts :: map().
start(Name, #{routes := Routes} = Opts) ->
    ok = arizona_cowboy_router:compile_routes(Routes),
    persistent_term:put({?ROUTES_KEY, Name}, Routes),
    ErrorPage = maps:get(error_page, Opts, {arizona_error_page, render}),
    persistent_term:put(arizona_error_page, ErrorPage),
    Scheme = maps:get(scheme, Opts, http),
    TransportOpts = maps:get(transport_opts, Opts, [{port, 4040}]),
    UserProtoOpts = maps:get(proto_opts, Opts, #{}),
    Env = maps:get(env, UserProtoOpts, #{}),
    ProtoOpts = UserProtoOpts#{env => Env#{dispatch => {persistent_term, arizona_dispatch}}},
    case Scheme of
        http -> cowboy:start_clear(Name, TransportOpts, ProtoOpts);
        https -> cowboy:start_tls(Name, TransportOpts, ProtoOpts)
    end.

-doc """
Stops the listener named `Name` and clears its persistent terms.
Returns `{error, not_found}` if `Name` is not a running listener.
""".
-spec stop(Name) -> ok | {error, not_found} when
    Name :: atom().
stop(Name) ->
    persistent_term:erase({?ROUTES_KEY, Name}),
    persistent_term:erase(arizona_error_page),
    cowboy:stop_listener(Name).

-doc """
Recompiles every Arizona-managed dispatch by walking persistent
terms. Called by the dev hot reloader after a successful recompile.
""".
-spec recompile_routes() -> ok.
recompile_routes() ->
    Terms = persistent_term:get(),
    [
        arizona_cowboy_router:compile_routes(Routes)
     || {{?ROUTES_KEY, _}, Routes} <- Terms
    ],
    ok.
