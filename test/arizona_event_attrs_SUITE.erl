-module(arizona_event_attrs_SUITE).
-include_lib("stdlib/include/assert.hrl").

-export([all/0]).
-export([walk_unions_deps_and_manual_declarations/1]).
-export([walk_takes_several_roots/1]).
-export([walk_skips_a_missing_module/1]).
-export([observe_is_a_noop_until_armed/1]).
-export([armed_collector_drains_once/1]).
-export([observe_attr_ignores_non_az_names/1]).

all() ->
    [
        walk_unions_deps_and_manual_declarations,
        walk_takes_several_roots,
        walk_skips_a_missing_module,
        observe_is_a_noop_until_armed,
        armed_collector_drains_once,
        observe_attr_ignores_non_az_names
    ].

%% The walk follows both the transform-recorded dependency (arizona_walk_child)
%% and the fixture's MANUAL -arizona_az_deps (arizona_walk_extra): every
%% instance of the attribute is unioned, so a handler can extend the graph by
%% declaring one itself.
walk_unions_deps_and_manual_declarations(Config) when is_list(Config) ->
    ?assertEqual(
        [~"az-contextmenu", ~"az-cut", ~"az-wheel"],
        arizona_event_attrs:all([arizona_walk_root])
    ).

walk_takes_several_roots(Config) when is_list(Config) ->
    ?assertEqual(
        [~"az-contextmenu", ~"az-cut", ~"az-wheel"],
        arizona_event_attrs:all([arizona_walk_child, arizona_walk_root, arizona_walk_extra])
    ).

%% A route nobody visited may name a module that does not exist (yet); the walk
%% answers with what it can prove rather than crashing.
walk_skips_a_missing_module(Config) when is_list(Config) ->
    ?assertEqual([], arizona_event_attrs:all([no_such_module_for_this_walk])).

%% Only a transported live process arms the collector; everywhere else --
%% request-path SSR, static generation -- observation must cost nothing and
%% collect nothing.
observe_is_a_noop_until_armed(Config) when is_list(Config) ->
    ok = arizona_event_attrs:observe_attr(~"az-close"),
    ok = arizona_event_attrs:observe_mod(arizona_walk_child),
    ?assertEqual({[], []}, arizona_event_attrs:drain()).

armed_collector_drains_once(Config) when is_list(Config) ->
    ok = arizona_event_attrs:arm(),
    ok = arizona_event_attrs:observe_attr(~"az-close"),
    ok = arizona_event_attrs:observe_attr(~"az-close"),
    ok = arizona_event_attrs:observe_attr(~"az-toggle"),
    ok = arizona_event_attrs:observe_mod(arizona_walk_child),
    ?assertEqual(
        {[~"az-close", ~"az-toggle"], [arizona_walk_child]},
        arizona_event_attrs:drain()
    ),
    ?assertEqual({[], []}, arizona_event_attrs:drain()).

observe_attr_ignores_non_az_names(Config) when is_list(Config) ->
    ok = arizona_event_attrs:arm(),
    ok = arizona_event_attrs:observe_attr(~"data-count"),
    ok = arizona_event_attrs:observe_attr(~"class"),
    ?assertEqual({[], []}, arizona_event_attrs:drain()).
