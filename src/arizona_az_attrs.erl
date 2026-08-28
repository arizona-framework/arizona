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

%% Only applications that list `arizona` as a dependency can hold templates, so
%% scanning the rest (OTP's own, the deps of deps) would be thousands of modules
%% for nothing. Arizona itself is included: its test fixtures carry templates.
%% Force the handler in before scanning: `beam_lib` answers for anything listed in
%% an `.app` whether loaded or not, but a module outside one is invisible until it
%% loads, and the handler is exactly such a module in a test fixture tree.
rescan(Seen, Handler) ->
    _ = code:ensure_loaded(Handler),
    {Examined, Names} = scan(),
    %% Every module the scan looked at is now accounted for, not just this handler.
    %% One cold connect therefore covers every route in the app: a later handler is
    %% already in the set and reads the cache, instead of each route paying its own
    %% ~5 ms rescan on its first visit.
    persistent_term:put(?CACHE_KEY, {maps:merge(Seen, Examined#{Handler => true}), Names}),
    Names.

scan() ->
    Apps = [App || {App, _, _} <- application:loaded_applications(), depends_on_arizona(App)],
    AppMods = [Mod || App <- Apps, Mod <- app_modules(App)],
    FromApps = [Name || Mod <- AppMods, Name <- beam_az_attrs(Mod)],
    %% Anything already loaded but absent from an `.app` modules list -- a module
    %% compiled at runtime, a test fixture -- would be missed by the pass above,
    %% and missing one silently kills every event it declares. `module_info/1`
    %% reads memory rather than a file, so this second pass is cheap (measured
    %% ~0.9 ms over 110 modules against ~4.7 ms for the beam pass).
    LoadedMods = [Mod || {Mod, _} <- code:all_loaded()],
    FromLoaded = [Name || Mod <- LoadedMods, Name <- loaded_az_attrs(Mod)],
    Examined = maps:from_keys(AppMods ++ LoadedMods, true),
    {Examined, lists:usort(FromApps ++ FromLoaded)}.

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

%% `beam_lib` rather than `Mod:module_info/1`: reading the chunk does not load the
%% module, and an unvisited route's view is typically not loaded at boot.
beam_az_attrs(Mod) ->
    case code:which(Mod) of
        Beam when is_list(Beam) ->
            case beam_lib:chunks(Beam, [attributes]) of
                {ok, {_Mod, [{attributes, Attrs}]}} ->
                    proplists:get_value(arizona_az_attrs, Attrs, []);
                _NotReadable ->
                    []
            end;
        _NonFile ->
            %% Cover-compiled or loaded from memory: the loaded pass answers for it.
            []
    end.

loaded_az_attrs(Mod) ->
    try
        proplists:get_value(arizona_az_attrs, Mod:module_info(attributes), [])
    catch
        error:undef -> []
    end.
