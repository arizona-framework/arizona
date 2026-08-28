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

-export([all/0]).
-export([flush/0]).

-ignore_xref([flush/0]).

-define(CACHE_KEY, {?MODULE, names}).

-doc """
Every `az-*` attribute name declared by any loaded application that depends on
Arizona, unioned and sorted.

Cached in `persistent_term` and recomputed when the set of loaded modules
changes, since a module outside any `.app` list is only visible once loaded.
""".
-spec all() -> [binary()].
all() ->
    %% Keyed on the number of loaded modules, not cached outright. The app-listed
    %% pass is stable, but the loaded pass is not: a module outside any `.app`
    %% modules list (compiled at runtime, or a test fixture) only becomes visible
    %% once it loads, so a set cached before that is permanently missing its names
    %% and every event it declares is silently dead. Detecting the change costs
    %% ~70 us against ~4 ms to rescan, and after the first few connections the
    %% count stops moving.
    Loaded = length(code:all_loaded()),
    case persistent_term:get(?CACHE_KEY, undefined) of
        {Loaded, Names} ->
            Names;
        _StaleOrMissing ->
            Names = scan(),
            persistent_term:put(?CACHE_KEY, {Loaded, Names}),
            Names
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
scan() ->
    Apps = [App || {App, _, _} <- application:loaded_applications(), depends_on_arizona(App)],
    FromApps = [
        Name
     || App <- Apps,
        Mod <- app_modules(App),
        Name <- beam_az_attrs(Mod)
    ],
    %% Anything already loaded but absent from an `.app` modules list -- a module
    %% compiled at runtime, a test fixture -- would be missed by the pass above,
    %% and missing one silently kills every event it declares. `module_info/1`
    %% reads memory rather than a file, so this second pass is cheap (measured
    %% ~0.9 ms over 110 modules against ~4.7 ms for the beam pass).
    FromLoaded = [
        Name
     || {Mod, _} <- code:all_loaded(),
        Name <- loaded_az_attrs(Mod)
    ],
    lists:usort(FromApps ++ FromLoaded).

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
