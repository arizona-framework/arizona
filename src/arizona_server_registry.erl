-module(arizona_server_registry).
-moduledoc """
The `arizona_server` adapters that have run a listener in this node, and the
listeners running now with their routes.

An adapter reports each listener it starts (`listener_up/3`) and stops
(`listener_down/1`). From that the registry:

- asks every adapter that ran a listener to recompile its routes after a dev
  reload (`recompile_routes/0`), which is how a listener started outside the
  `server` env -- by hand, by a test, or by another framework that owns it --
  still gets its routes refreshed;
- keeps the node's dev-mode signals in step with the routes running: the
  `{reload, ...}` route's path (`arizona_reload_url`, which turns on the rich
  error page and the checked render) and whether a dev MCP route is mounted
  (`arizona_dev_mcp_route`, read by `arizona_reloader:enabled/0`). A dev
  listener that stops takes them along, so a production listener started
  later in the same node never serves the dev error page.

Everything lives in `persistent_term`s, so it needs no process: a node that
never starts a server pays nothing, and readers never call. An adapter is never
unregistered; one with no listener left recompiles nothing.
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([listener_up/3]).
-export([listener_down/1]).
-export([adapters/0]).
-export([recompile_routes/0]).
-export([capabilities/1]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

%% Introspection for apps and tests; in-tree code only calls it internally.
-ignore_xref([adapters/0]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------

-define(KEY, arizona_server_adapters).
-define(LISTENERS, arizona_server_listeners).

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc """
Reports that `Adapter` started the listener `Name` serving `Routes`: registers
the adapter and turns on the dev-mode signals the routes carry. Called once the
listener is up, so a failed start leaves nothing behind.
""".
-spec listener_up(Adapter, Name, Routes) -> ok when
    Adapter :: module(),
    Name :: atom(),
    Routes :: [term()].
listener_up(Adapter, Name, Routes) ->
    ok = register(Adapter),
    persistent_term:put(?LISTENERS, (listeners())#{Name => Routes}),
    %% The listener started last takes the reload URL, as when each listener
    %% wrote it as its routes compiled.
    ok =
        case reload_paths(Routes) of
            [] -> ok;
            Paths -> set(arizona_reload_url, lists:last(Paths))
        end,
    refresh_dev_signals().

-doc """
Reports that the listener `Name` stopped: its routes stop counting toward the
dev-mode signals, handed back to a listener still running that carries them,
or cleared. Harmless for a name that was not up.
""".
-spec listener_down(Name) -> ok when
    Name :: atom().
listener_down(Name) ->
    case listeners() of
        #{Name := _} = Listeners -> persistent_term:put(?LISTENERS, maps:remove(Name, Listeners));
        #{} -> ok
    end,
    refresh_dev_signals().

-doc "The registered adapters, most recently registered first.".
-spec adapters() -> [module()].
adapters() ->
    persistent_term:get(?KEY, []).

-doc """
`Adapter`'s capabilities (`c:arizona_server:capabilities/0`); every kind when it
does not declare them.
""".
-spec capabilities(Adapter) ->
    #{routes := [mcp | reload], replies := [stream | sendfile | loop]}
when
    Adapter :: module().
capabilities(Adapter) ->
    {module, Adapter} = code:ensure_loaded(Adapter),
    case erlang:function_exported(Adapter, capabilities, 0) of
        true -> Adapter:capabilities();
        false -> #{routes => [mcp, reload], replies => [stream, sendfile, loop]}
    end.

-doc "Asks every registered adapter to recompile its listeners' routes.".
-spec recompile_routes() -> ok.
recompile_routes() ->
    lists:foreach(fun(Adapter) -> ok = Adapter:recompile_routes() end, adapters()).

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% Registers `Adapter`, writing the term only when the set changes so repeated
%% starts do not pay a `persistent_term` update each.
-spec register(Adapter) -> ok when
    Adapter :: module().
register(Adapter) when is_atom(Adapter) ->
    Adapters = adapters(),
    case lists:member(Adapter, Adapters) of
        true -> ok;
        false -> persistent_term:put(?KEY, [Adapter | Adapters])
    end.

listeners() ->
    persistent_term:get(?LISTENERS, #{}).

%% Each term is written only when it changes: a `persistent_term` update makes
%% every process that read it copy the old value, so it must not run per start.
refresh_dev_signals() ->
    Routes = lists:append(maps:values(listeners())),
    ok = refresh_reload_url(reload_paths(Routes)),
    case [Path || {mcp, Path, arizona_dev_mcp, _Opts} <- Routes] of
        [] -> set(arizona_dev_mcp_route, undefined);
        [_ | _] -> set(arizona_dev_mcp_route, true)
    end.

reload_paths(Routes) ->
    [Path || {reload, Path, _Opts} <- Routes].

%% The URL in use stays while a running listener still serves it; otherwise the
%% last reload route running takes over.
refresh_reload_url([]) ->
    set(arizona_reload_url, undefined);
refresh_reload_url(Paths) ->
    case lists:member(persistent_term:get(arizona_reload_url, undefined), Paths) of
        true -> ok;
        false -> set(arizona_reload_url, lists:last(Paths))
    end.

set(Key, Value) ->
    case {persistent_term:get(Key, undefined), Value} of
        {Value, _} ->
            ok;
        {_, undefined} ->
            persistent_term:erase(Key),
            ok;
        {_, _} ->
            persistent_term:put(Key, Value)
    end.
