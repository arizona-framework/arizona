-module(arizona_az_attrs).
-moduledoc """
The `az-*` attribute names an application's templates declare.

The parse transform resolves every attribute name to a literal at compile time
and records it as an `arizona_az_attrs` module attribute. This module unions
those across the running applications so the client can delegate exactly the DOM
event types in play, without re-deriving them from rendered bytes.

That re-derivation is what this replaces, and why it is worth the boot scan: a
regex over serialized HTML cannot tell an attribute NAME from `az-` text inside
an attribute VALUE, so user-authored content echoed into a `title` could declare
event types; and it truncates on a `>` inside a value, silently dropping real
ones. The names are compile-time literals, so reading them from the beam is both
exact and free of any user-content path.

The scan reads the `attributes` chunk with `beam_lib`, so a module that has not
been loaded yet -- the common case for a route nobody has visited -- is still
counted. Missing one would silently break every event that module declares.
""".

-export([all/1]).
-export([flush/0]).

-ignore_xref([flush/0]).

-define(CACHE_KEY, {?MODULE, names}).

-doc """
Every `az-*` attribute name declared by any loaded application that depends on
Arizona, unioned and sorted.

Cached in `persistent_term`, keyed on the route handlers already accounted for.

A handler is the entry point to everything a page can render, so a handler this
node has not served before is the one moment the answer can grow -- a module
outside any `.app` modules list (compiled at runtime, or a test fixture) becomes
visible only once something loads it. Polling `length(code:all_loaded())` instead
also worked, but cost 72 us of a 97 us connect: three quarters of the connect path
spent proving an answer that had not changed. This costs one map lookup.
""".
-spec all(module()) -> [binary()].
all(Handler) ->
    case persistent_term:get(?CACHE_KEY, undefined) of
        {Seen, Names} when is_map_key(Handler, Seen) ->
            Names;
        {Seen, _Stale} ->
            rescan(Seen, Handler);
        undefined ->
            rescan(#{}, Handler)
    end.

-doc """
Drop the cached set, so the next `all/0` rescans. For tests and for a dev-mode
code swap that keeps the node alive.
""".
-spec flush() -> ok.
flush() ->
    _ = persistent_term:erase(?CACHE_KEY),
    ok.

%% --------------------------------------------------------------------
%% Internal
%% --------------------------------------------------------------------

%% Force the handler in before walking: `beam_lib` answers for anything listed in
%% an `.app` whether loaded or not, but a module outside one is invisible until it
%% loads, and the handler is exactly such a module in a fixture tree.
rescan(Seen, Handler) ->
    _ = code:ensure_loaded(Handler),
    {Examined, Names} = scan(Handler),
    %% Every module the walk and the scan looked at is now accounted for, not just
    %% this handler: one cold connect therefore covers every route in the app.
    persistent_term:put(?CACHE_KEY, {maps:merge(Seen, Examined#{Handler => true}), Names}),
    Names.

%% Walk the component graph from the handler, then fall back to an application
%% scan for anything the graph cannot reach.
%%
%% The graph is exact: `?stateful(Mod, _)` / `?stateless(Mod, Fun, _)` name their
%% module as a literal, so the transform records it (`arizona_az_deps`) and this
%% walk visits precisely the modules a page can render. The scan is a heuristic --
%% it guesses which applications might hold templates -- and exists only for a
%% module reached through a runtime-bound module name, which the transform cannot
%% resolve and therefore does not record.
scan(Handler) ->
    {GraphMods, GraphNames} = walk(#{}, [Handler], []),
    {AppMods, AppNames} = scan_apps(),
    {maps:merge(AppMods, GraphMods), lists:usort(GraphNames ++ AppNames)}.

walk(Seen, [], Acc) ->
    {Seen, Acc};
walk(Seen, [Mod | Rest], Acc) when is_map_key(Mod, Seen) ->
    walk(Seen, Rest, Acc);
walk(Seen, [Mod | Rest], Acc) ->
    _ = code:ensure_loaded(Mod),
    Attrs = module_attrs(Mod),
    Names = proplists:get_value(arizona_az_attrs, Attrs, []),
    Deps = proplists:get_value(arizona_az_deps, Attrs, []),
    walk(Seen#{Mod => true}, Deps ++ Rest, Names ++ Acc).

scan_apps() ->
    Apps = [App || {App, _, _} <- application:loaded_applications(), depends_on_arizona(App)],
    AppMods = [Mod || App <- Apps, Mod <- app_modules(App)],
    FromApps = [Name || Mod <- AppMods, Name <- beam_az_attrs(Mod)],
    LoadedMods = [Mod || {Mod, _} <- code:all_loaded()],
    FromLoaded = [Name || Mod <- LoadedMods, Name <- loaded_az_attrs(Mod)],
    {maps:from_keys(AppMods ++ LoadedMods, true), lists:usort(FromApps ++ FromLoaded)}.

depends_on_arizona(arizona) ->
    true;
depends_on_arizona(App) ->
    case application:get_key(App, applications) of
        {ok, Deps} -> lists:member(arizona, Deps);
        undefined -> false
    end.

app_modules(App) ->
    case application:get_key(App, modules) of
        {ok, Mods} -> Mods;
        undefined -> []
    end.

%% Prefer the loaded module (the walk just ensured it), falling back to the beam so
%% a dependency that is on disk but not yet loaded still answers.
module_attrs(Mod) ->
    try
        Mod:module_info(attributes)
    catch
        error:undef -> beam_attrs(Mod)
    end.

beam_attrs(Mod) ->
    case code:which(Mod) of
        Beam when is_list(Beam) ->
            case beam_lib:chunks(Beam, [attributes]) of
                {ok, {_Mod, [{attributes, Attrs}]}} -> Attrs;
                _NotReadable -> []
            end;
        _NonFile ->
            []
    end.

%% `beam_lib` rather than `Mod:module_info/1`: reading the chunk does not load the
%% module, and an unvisited route's view is typically not loaded at boot.
beam_az_attrs(Mod) ->
    proplists:get_value(arizona_az_attrs, beam_attrs(Mod), []).

loaded_az_attrs(Mod) ->
    try
        proplists:get_value(arizona_az_attrs, Mod:module_info(attributes), [])
    catch
        error:undef -> []
    end.
