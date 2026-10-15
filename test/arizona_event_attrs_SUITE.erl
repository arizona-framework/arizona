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
-export([walk_cache_follows_hot_reload/1]).
-export([walk_asks_for_a_missing_callee_once/1]).
-export([walk_terminates_on_a_cycle/1]).

all() ->
    [
        walk_unions_deps_and_manual_declarations,
        walk_takes_several_roots,
        walk_skips_a_missing_module,
        observe_is_a_noop_until_armed,
        armed_collector_drains_once,
        observe_attr_ignores_non_az_names,
        layout_command_warns_at_ssr,
        walk_cache_follows_hot_reload,
        walk_asks_for_a_missing_callee_once,
        walk_terminates_on_a_cycle
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

%% The walk caches each module's names and edges keyed by the loaded code's MD5,
%% so a hot reload must be seen on the very next walk: new code, new MD5, a
%% re-read. Reloading the first version back proves the stamp is compared, not
%% just set once. The probe's code carries its names the way a template's
%% statics carry a folded name, since the MD5 covers code, not attributes.
walk_cache_follows_hot_reload(Config) when is_list(Config) ->
    Mod = arizona_event_attrs_reload_probe,
    ok = load_edges_module(Mod, [~"az-copy"], []),
    ?assertEqual([~"az-copy"], arizona_event_attrs:all([Mod])),
    ok = load_edges_module(Mod, [~"az-paste"], []),
    ?assertEqual([~"az-paste"], arizona_event_attrs:all([Mod])),
    ok = load_edges_module(Mod, [~"az-copy"], []),
    ?assertEqual([~"az-copy"], arizona_event_attrs:all([Mod])).

%% A dangling edge (a recorded callee that does not exist) is asked of the code
%% server once; every later walk answers from the cached absence instead of a
%% code-server round trip per connect.
walk_asks_for_a_missing_callee_once(Config) when is_list(Config) ->
    Root = arizona_event_attrs_missing_root,
    Missing = arizona_event_attrs_never_compiled,
    ok = load_edges_module(Root, [~"az-input"], [Missing]),
    %% A process cannot be its own call tracer, so a collector receives them.
    Tracer = spawn_link(fun() -> collect_ensure_loaded(Missing, 0) end),
    1 = erlang:trace_pattern({code, ensure_loaded, 1}, true, []),
    1 = erlang:trace(self(), true, [call, {tracer, Tracer}]),
    try
        ?assertEqual([~"az-input"], arizona_event_attrs:all([Root])),
        ?assertEqual([~"az-input"], arizona_event_attrs:all([Root])),
        ?assertEqual([~"az-input"], arizona_event_attrs:all([Root]))
    after
        1 = erlang:trace(self(), false, [call]),
        1 = erlang:trace_pattern({code, ensure_loaded, 1}, false, [])
    end,
    Ref = erlang:trace_delivered(self()),
    receive
        {trace_delivered, _, Ref} -> ok
    after 1000 ->
        ct:fail("trace delivery never confirmed")
    end,
    Tracer ! {count, self()},
    receive
        {ensure_loaded_count, Count} -> ?assertEqual(1, Count)
    after 1000 ->
        ct:fail("the trace collector never answered")
    end.

%% Two modules naming each other terminate the walk and union both.
walk_terminates_on_a_cycle(Config) when is_list(Config) ->
    ok = load_edges_module(arizona_event_attrs_cycle_a, [~"az-focusin"], [
        arizona_event_attrs_cycle_b
    ]),
    ok = load_edges_module(arizona_event_attrs_cycle_b, [~"az-focusout"], [
        arizona_event_attrs_cycle_a
    ]),
    ?assertEqual(
        [~"az-focusin", ~"az-focusout"],
        arizona_event_attrs:all([arizona_event_attrs_cycle_a])
    ).

%% Compiles and (re)loads a module carrying the given attributes, and an
%% exported `markup/0` holding the names as a literal, the way a template's
%% statics hold a folded name. Purges an old version first so it can be loaded
%% any number of times.
load_edges_module(Mod, Names, Deps) ->
    Forms = [
        parse_form(io_lib:format("-module(~w).", [Mod])),
        parse_form("-export([markup/0])."),
        parse_form(io_lib:format("-arizona_az_attrs(~w).", [Names])),
        parse_form(io_lib:format("-arizona_az_deps(~w).", [Deps])),
        parse_form(io_lib:format("markup() -> ~w.", [Names]))
    ],
    {ok, Mod, Bin} = compile:forms(Forms, []),
    true = code:soft_purge(Mod),
    {module, Mod} = code:load_binary(Mod, "", Bin),
    ok.

parse_form(Src) ->
    {ok, Tokens, _} = erl_scan:string(lists:flatten(Src)),
    {ok, Form} = erl_parse:parse_form(Tokens),
    Form.

collect_ensure_loaded(Mod, N) ->
    receive
        {trace, _, call, {code, ensure_loaded, [Mod]}} -> collect_ensure_loaded(Mod, N + 1);
        {trace, _, call, {code, ensure_loaded, [_Other]}} -> collect_ensure_loaded(Mod, N);
        {count, From} -> From ! {ensure_loaded_count, N}
    after 5000 ->
        exit(no_count_request)
    end.
