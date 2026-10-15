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
   attribute) and the modules it can take markup from (`arizona_az_deps`):
   the component modules its templates name literally, and every module it
   calls, imports, or takes a `fun Mod:F/A` of by a literal name -- a remote
   helper's template (`other_mod:button()`) carries that module's folded
   names in its statics, which no render re-proves. `all/1` walks that graph
   from a set of roots (a
   route's handler plus its layout modules), so the connect frame delivers a
   page's whole vocabulary up front -- including names sitting in branches
   nothing has rendered yet. A handler may extend the graph by declaring
   `-arizona_az_deps([mod, ...]).` itself; every instance of the attribute is
   unioned.
2. **Render time.** What compile time cannot prove, evaluation can: a dynamic
   attribute value (or `?local` attribute init) that classifies as an
   `{arizona_effect, _}` command names its attribute (`observe_attr/1`) where
   `arizona_eval` evaluates it -- every live mount, render and diff, so a mount
   observes without building the document and a diff observes every value its
   ops ship -- and a `?stateful`/`?stateless`
   instantiation names its module (`observe_mod/1`) -- covering a module bound
   at runtime (`?stateful(?get(page), ...)`), which the walk cannot follow.
   A live process arms the collector (`arm/0`) and drains it into every reply
   it emits (`drain/0`); the socket walks newly observed modules, dedups
   against what it already sent, and ships the delta on that same frame, so a
   name arrives with the first frame that can render the markup declaring it.

The collector is process-dictionary state, armed only inside a live process:
the request-free SSR paths (HTTP render, static generation) never arm it, so
observation there is a no-op.

The walk follows every literal callee a module records, so it loads any of
them not loaded yet -- running that module's `-on_load`, as its first call
would. Each module's names and edges are cached in `persistent_term`, keyed by
module and stamped with the loaded code's MD5: a hit costs a `module_loaded`
check, the MD5 read, and a lookup, where re-reading the attributes of a large
framework callee costs microseconds per module on every connect. The stamp is
what keeps the cache honest across a hot reload (a cache keyed by module alone
once went stale there and served a page's events short): new code has a new
MD5, so its first walk re-reads it and replaces the entry -- one
`persistent_term` write per module version. The compiler's MD5 covers code,
not attributes; the transform-recorded ones follow the code (a folded name is
a template static, an edge is a call or a component atom), and the dev
reloader drops the entry of every module it loads itself, so a dev edit to
only a hand-written `-arizona_az_deps`/`-arizona_az_attrs` is seen too. Any
other load path (a release upgrade) keeps the previous version's entry for
such an edit until the code itself changes or the VM restarts. (Under `rebar3
shell` the agent never reloads an attribute-only edit at all: it decides what
changed by the same MD5.)
A module that fails to load is cached as absent, so a dangling edge does not
cost a code-server round trip on every connect; that entry is ignored once
the module is loaded by any other means (a deploy, the dev reloader).

The per-socket dedup the socket keeps on
top of it (`az_walked`/`az_sent`) is a narrower memo with a narrower window:
a hot reload that adds a FOLDED command attribute (a literal builder call
bakes into the statics, so no dynamic slot ever re-proves it) leaves already
open sockets without the new name until they reconnect -- which the dev
watcher's reload forces anyway.

Known limits, all loud or documented rather than silent:

- A command that reaches a LAYOUT attribute opaquely can never be delivered
  (layouts render once, at SSR; no frame re-renders one) -- the render warns
  (`observe_attr/1`'s unarmed branch). A `?stateless` with a runtime-bound
  module inside a layout is the same hole one level up; `?stateful` in a
  layout is already a render-time error.
- A non-transport caller of `arizona_live:handle_event/4` (or `patch/2`)
  receives that reply's drained observations and they die with it: unlike
  ops, which the stale-snapshot mechanism re-emits on the next root diff, an
  unchanged value is never re-proven. In-repo the only such callers are the
  terminal transports, which have no DOM to delegate on.
- A helper template reached through no literal edge -- a module bound at
  runtime (`Mod:button()`), a fun value, `apply/3`, or an intermediate module
  compiled without the Arizona headers (it carries no `arizona_az_deps`, so
  the walk stops there) -- delivers only the names a render re-proves, never
  its FOLDED ones. Declare the edge by hand with `-arizona_az_deps([mod]).`
  on the page (or the intermediate module). The recorded edges also
  over-approximate: every literal callee counts whether or not its markup
  renders, at the same per-module grain as a `?stateful` in an untaken
  branch.
""".

-export([all/1]).
-export([arm/0]).
-export([drain/0]).
-export([forget/1]).
-export([observe_attr/1]).
-export([observe_callback/1]).
-export([observe_mod/1]).

-export_type([observed/0]).

%% Render-time observations drained from a live process: proven command
%% attribute names, and component modules instantiated since the last drain.
-nominal observed() :: {[binary()], [module()]}.

-define(ATTRS_KEY, '$arizona_observed_attrs').
-define(MODS_KEY, '$arizona_observed_mods').
-define(CACHE_KEY(Mod), {?MODULE, Mod}).

-doc """
Every compile-time-proven `az-*` attribute reachable from `Roots`, themselves
included.
""".
-spec all(Roots) -> [binary()] when
    Roots :: [module()].
all(Roots) ->
    lists:usort(walk(#{}, Roots, [])).

%% Drops `Mod`'s walk cache entry, so the next walk re-reads it. Called by the
%% dev reloader after loading a module: the MD5 stamp covers code, not
%% attributes, so an edit to only a hand-written `-arizona_az_deps` or
%% `-arizona_az_attrs` would otherwise keep the previous entry.
-doc false.
-spec forget(Mod) -> ok when
    Mod :: module().
forget(Mod) ->
    case persistent_term:erase(?CACHE_KEY(Mod)) of
        true -> ok;
        false -> ok
    end.

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
    %% The empty-empty case is almost every drain (one per reply a live process
    %% emits), so it must not write the dictionary or allocate.
    case get(?ATTRS_KEY) of
        undefined ->
            {[], []};
        [] ->
            case get(?MODS_KEY) of
                [] ->
                    {[], []};
                Mods ->
                    put(?MODS_KEY, []),
                    {[], lists:usort(Mods)}
            end;
        Names ->
            put(?ATTRS_KEY, []),
            Mods = get(?MODS_KEY),
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
            unprovable_in_layout(Name);
        Names ->
            put(?ATTRS_KEY, [Name | Names]),
            ok
    end;
observe_attr(_Name) ->
    ok.

%% An unarmed observation is normally fine: the request-path SSR render is
%% re-run by the connect mount, which observes into a live socket. The one
%% place that does not hold is a LAYOUT render -- layouts render once, at SSR,
%% in a peerless process, and no frame ever re-renders one -- so a command
%% proven only there can never reach the client and its event is permanently
%% dead. Loud beats invisible.
unprovable_in_layout(Name) ->
    case arizona_render:current_layout() of
        undefined ->
            ok;
        {Mod, Fun} ->
            logger:warning(
                "az attribute ~ts in layout ~p:~p/1 carries a command the "
                "client will never delegate: a layout renders once, at SSR, "
                "so its render-time proof reaches no frame. Build the command "
                "with a literal arizona_js/arizona_android/arizona_os call so "
                "it is proven at compile time instead",
                [Name, Mod, Fun]
            )
    end.

-doc """
`observe_mod/1` for a stateless render callback: the armed check runs BEFORE
the `fun_info` module lookup, so an unarmed render (SSR, static generation)
pays one dictionary read and nothing else.
""".
-spec observe_callback(Callback) -> ok when
    Callback :: fun((map()) -> term()).
observe_callback(Callback) ->
    case get(?MODS_KEY) of
        undefined ->
            ok;
        Mods ->
            {module, Mod} = erlang:fun_info(Callback, module),
            put(?MODS_KEY, [Mod | Mods]),
            ok
    end.

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
    {Names, Deps} = module_edges(Mod),
    walk(Seen#{Mod => true}, Deps ++ Rest, Names ++ Acc).

%% `{Names, Deps}` of a module, through the MD5-stamped cache (see the
%% moduledoc). A handler for a route nobody has visited is typically not loaded
%% yet, so an unloaded module is loaded once; one that cannot be is cached as
%% `absent` so the code server is not asked again on every connect.
module_edges(Mod) ->
    case erlang:module_loaded(Mod) of
        true ->
            loaded_edges(Mod);
        false ->
            case persistent_term:get(?CACHE_KEY(Mod), undefined) of
                absent -> {[], []};
                _NotLoadedYet -> load_edges(Mod)
            end
    end.

load_edges(Mod) ->
    case code:ensure_loaded(Mod) of
        {module, Mod} ->
            loaded_edges(Mod);
        {error, _NotFound} ->
            ok = persistent_term:put(?CACHE_KEY(Mod), absent),
            {[], []}
    end.

loaded_edges(Mod) ->
    try erlang:get_module_info(Mod, md5) of
        MD5 ->
            case persistent_term:get(?CACHE_KEY(Mod), undefined) of
                {MD5, Names, Deps} ->
                    {Names, Deps};
                _MissOrStale ->
                    Attrs = erlang:get_module_info(Mod, attributes),
                    Names = proplists:append_values(arizona_az_attrs, Attrs),
                    Deps = proplists:append_values(arizona_az_deps, Attrs),
                    ok = persistent_term:put(?CACHE_KEY(Mod), {MD5, Names, Deps}),
                    {Names, Deps}
            end
    catch
        error:badarg ->
            %% Purged between the `module_loaded` check and the read.
            {[], []}
    end.
