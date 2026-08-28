-module(arizona_event_attrs_SUITE).
-include_lib("stdlib/include/assert.hrl").

-export([all/0]).
-export([walk_unions_deps_and_manual_declarations/1]).
-export([walk_takes_several_roots/1]).
-export([walk_skips_a_missing_module/1]).
-export([observe_is_a_noop_until_armed/1]).
-export([armed_collector_drains_once/1]).
-export([observe_attr_ignores_non_az_names/1]).
-export([layout_command_warns_at_ssr/1]).

all() ->
    [
        walk_unions_deps_and_manual_declarations,
        walk_takes_several_roots,
        walk_skips_a_missing_module,
        observe_is_a_noop_until_armed,
        armed_collector_drains_once,
        observe_attr_ignores_non_az_names,
        layout_command_warns_at_ssr
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

%% A command proven only inside a layout render can never reach the client:
%% layouts render once, at SSR, in a peerless process, and no frame ever
%% re-renders one -- so the render-time proof has nothing to ride. The render
%% warns, because the alternative is a permanently dead event with no symptom.
layout_command_warns_at_ssr(Config) when is_list(Config) ->
    HandlerId = ?FUNCTION_NAME,
    ok = logger:add_handler(HandlerId, arizona_test_log_handler, #{
        level => warning, config => #{pid => self()}
    }),
    try
        _HTML = arizona_render:render_view_to_iolist(arizona_opaque_layout_page, #{
            layouts => [{arizona_opaque_layout, render}],
            bindings => #{chrome_cmd => arizona_js:toggle(~"#menu")}
        }),
        receive
            {arizona_test_log_handler, #{level := warning, msg := {Fmt, Args}}} ->
                Msg = iolist_to_binary(io_lib:format(Fmt, Args)),
                ?assertMatch({_, _}, binary:match(Msg, ~"az-contextmenu")),
                ?assertMatch({_, _}, binary:match(Msg, ~"arizona_opaque_layout"))
        after 1000 ->
            ct:fail("no warning for an unprovable layout command")
        end
    after
        ok = logger:remove_handler(HandlerId)
    end.
