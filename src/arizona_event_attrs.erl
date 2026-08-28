-module(arizona_event_attrs).
-moduledoc """
The `az-*` attributes a page can prove it renders, and how the proof reaches
the client.

The client delegates a DOM event type only for a name it has been told about,
so every name must be *proved* to carry commands -- guessing would either leave
a declared event dead or hand app data to the command interpreter. There are
two proofs, one per moment the question is answerable:

1. **Compile time.** The parse transform records per module the `az-*` names
   whose values it can prove are commands (the `arizona_az_attrs` module
   attribute) and the component modules its templates name literally
   (`arizona_az_deps`). `all/1` walks that graph from a set of roots (a
   route's handler plus its layout modules), so the connect frame delivers a
   page's whole vocabulary up front -- including names sitting in branches
   nothing has rendered yet. A handler may extend the graph by declaring
   `-arizona_az_deps([mod, ...]).` itself; every instance of the attribute is
   unioned.
2. **Render time.** What compile time cannot prove, evaluation can: a dynamic
   attribute value that classifies as an `{arizona_effect, _}` command names
   its attribute (`observe_attr/1`), and a `?stateful`/`?stateless`
   instantiation names its module (`observe_mod/1`) -- covering a module bound
   at runtime (`?stateful(?get(page), ...)`), which the walk cannot follow.
   A live process arms the collector (`arm/0`) and drains it into every reply
   it emits (`drain/0`); the socket walks newly observed modules, dedups
   against what it already sent, and ships the delta on that same frame, so a
   name arrives with the first frame that can render the markup declaring it.

The collector is process-dictionary state, armed only inside a live process:
the request-free SSR paths (HTTP render, static generation) never arm it, so
observation there is a no-op.

The walk is deliberately uncached: it costs microseconds against the
milliseconds of the application scan it replaced, cheap enough that a cache
would only add a staleness bug -- the previous one went stale on hot reload
and served a page's events short.
""".

-export([all/1]).
-export([arm/0]).
-export([drain/0]).
-export([observe_attr/1]).
-export([observe_mod/1]).

-export_type([observed/0]).

%% Render-time observations drained from a live process: proven command
%% attribute names, and component modules instantiated since the last drain.
-nominal observed() :: {[binary()], [module()]}.

-define(ATTRS_KEY, '$arizona_observed_attrs').
-define(MODS_KEY, '$arizona_observed_mods').

-doc """
Every compile-time-proven `az-*` attribute reachable from `Roots`, themselves
included.
""".
-spec all(Roots) -> [binary()] when
    Roots :: [module()].
all(Roots) ->
    lists:usort(walk(#{}, Roots, [])).

-doc """
Arms the render-time collector in the calling process. Until armed,
`observe_attr/1` and `observe_mod/1` are no-ops.
""".
-spec arm() -> ok.
arm() ->
    put(?ATTRS_KEY, []),
    put(?MODS_KEY, []),
    ok.

-doc """
The observations collected since the last drain, clearing the collector.
`{[], []}` in an unarmed process.
""".
-spec drain() -> observed().
drain() ->
    case get(?ATTRS_KEY) of
        undefined ->
            {[], []};
        Names ->
            Mods = get(?MODS_KEY),
            put(?ATTRS_KEY, []),
            put(?MODS_KEY, []),
            {lists:usort(Names), lists:usort(Mods)}
    end.

-doc """
Records that a dynamic `az-*` attribute rendered a command value -- the typed
proof compile time cannot see for an opaque expression. Any other name is
ignored, so callers can hand over every dynamic attribute name unfiltered.
""".
-spec observe_attr(Name) -> ok when
    Name :: binary().
observe_attr(<<"az-", _/binary>> = Name) ->
    case get(?ATTRS_KEY) of
        undefined ->
            ok;
        Names ->
            put(?ATTRS_KEY, [Name | Names]),
            ok
    end;
observe_attr(_Name) ->
    ok.

-doc """
Records that a component module was instantiated, so the socket can walk it --
the only way a module bound at runtime ever reaches the graph.
""".
-spec observe_mod(Mod) -> ok when
    Mod :: module().
observe_mod(Mod) ->
    case get(?MODS_KEY) of
        undefined ->
            ok;
        Mods ->
            put(?MODS_KEY, [Mod | Mods]),
            ok
    end.

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
        proplists:append_values(arizona_az_deps, Attrs) ++ Rest,
        proplists:append_values(arizona_az_attrs, Attrs) ++ Acc
    ).

module_attrs(Mod) ->
    try
        Mod:module_info(attributes)
    catch
        error:undef -> []
    end.
