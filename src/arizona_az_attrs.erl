-module(arizona_az_attrs).
-moduledoc """
The `az-*` attributes a page can render, resolved from the component graph.

The parse transform records two things per module at compile time: the `az-*`
attribute names its templates declare (`arizona_az_attrs`), and the component
modules they render (`arizona_az_deps`). Both are literals -- `?stateful(Mod, _)`
and `?stateless(Mod, Fun, _)` name their module directly -- so walking from a route
handler reaches exactly the modules that page can render, and no others.

This exists because a rendered page cannot answer the question. A component inside
an `?each` item flattens to HTML during render, so its template never becomes a
snapshot and its attributes are gone by the time anything could look: measured, a
walk of the finished snapshot misses `az-dblclick` on a `?stateless` inside an item
that the rendered HTML plainly contains. Compile time is the only moment the whole
graph is visible.

Deliberately uncached. The walk costs ~7 us against ~4.6 ms for the application
scan it replaces, which is cheap enough that a cache would only add a staleness
bug -- the previous one went stale on hot reload and served a page's events short.
""".

-export([all/1]).

-doc """
Every `az-*` attribute reachable from `Handler`, itself included.
""".
-spec all(Handler) -> [binary()] when
    Handler :: module().
all(Handler) ->
    lists:usort(walk(#{}, [Handler], [])).

%% --------------------------------------------------------------------
%% Internal
%% --------------------------------------------------------------------

walk(_Seen, [], Acc) ->
    Acc;
walk(Seen, [Mod | Rest], Acc) when is_map_key(Mod, Seen) ->
    walk(Seen, Rest, Acc);
walk(Seen, [Mod | Rest], Acc) ->
    %% A handler for a route nobody has visited is typically not loaded yet, and an
    %% unloaded module answers `undef` rather than an empty list.
    _ = code:ensure_loaded(Mod),
    Attrs = module_attrs(Mod),
    walk(
        Seen#{Mod => true},
        proplists:get_value(arizona_az_deps, Attrs, []) ++ Rest,
        proplists:get_value(arizona_az_attrs, Attrs, []) ++ Acc
    ).

module_attrs(Mod) ->
    try
        Mod:module_info(attributes)
    catch
        error:undef -> []
    end.
