-module(arizona_static_SUITE).
-include_lib("stdlib/include/assert.hrl").
%% Feeds generate/2 a spec whose Opts violates opts() on purpose.
-dialyzer({nowarn_function, collects_non_map_spec_opts/1}).

-export([all/0]).
-export([groups/0]).

-export([writes_file_and_returns_path/1]).
-export([applies_bindings/1]).
-export([applies_layout/1]).
-export([creates_nested_dirs/1]).
-export([applies_on_mount/1]).
-export([multiple_specs_return_all_paths/1]).
-export([default_opts_apply_to_every_spec/1]).
-export([spec_opts_override_defaults/1]).
-export([collects_failures/1]).
-export([failure_carries_render_location/1]).
-export([failed_entries_cover_every_spec_shape/1]).
-export([collects_write_errors/1]).
-export([collects_non_map_spec_opts/1]).
-export([embeds_compile_time_proven_attrs/1]).
-export([embeds_render_time_proven_attrs/1]).
-export([page_without_events_has_no_attrs/1]).
-export([page_without_body_is_left_as_rendered/1]).
-export([leaves_the_collector_disarmed/1]).

all() ->
    [{group, generate}].

groups() ->
    [
        {generate, [parallel], [
            writes_file_and_returns_path,
            applies_bindings,
            applies_layout,
            creates_nested_dirs,
            applies_on_mount,
            multiple_specs_return_all_paths,
            default_opts_apply_to_every_spec,
            spec_opts_override_defaults,
            collects_failures,
            failure_carries_render_location,
            failed_entries_cover_every_spec_shape,
            collects_write_errors,
            collects_non_map_spec_opts,
            embeds_compile_time_proven_attrs,
            embeds_render_time_proven_attrs,
            page_without_events_has_no_attrs,
            page_without_body_is_left_as_rendered,
            leaves_the_collector_disarmed
        ]}
    ].

writes_file_and_returns_path(Config) when is_list(Config) ->
    Dir = subdir(Config, "writes"),
    {[Out], []} = arizona_static:generate(Dir, [{arizona_static_page, ~"index.html"}]),
    ?assert(filelib:is_regular(Out)),
    Content = read(Out),
    ?assertMatch({_, _}, binary:match(Content, ~"static_page")),
    ?assertMatch({_, _}, binary:match(Content, ~"Static")).

applies_bindings(Config) when is_list(Config) ->
    Dir = subdir(Config, "bindings"),
    {[Out], []} = arizona_static:generate(
        Dir, [{arizona_static_page, ~"index.html", #{bindings => #{title => ~"Hello"}}}]
    ),
    ?assertMatch({_, _}, binary:match(read(Out), ~"Hello")).

applies_layout(Config) when is_list(Config) ->
    Dir = subdir(Config, "layout"),
    {[Out], []} = arizona_static:generate(
        Dir, [{arizona_static_page, ~"index.html", #{layouts => [{arizona_layout, render}]}}]
    ),
    Content = read(Out),
    ?assertMatch({_, _}, binary:match(Content, ~"<!DOCTYPE html>")),
    ?assertMatch({_, _}, binary:match(Content, ~"static_page")).

creates_nested_dirs(Config) when is_list(Config) ->
    Dir = subdir(Config, "nested"),
    {[Out], []} = arizona_static:generate(Dir, [{arizona_static_page, ~"deep/page.html"}]),
    ?assert(filelib:is_regular(Out)).

applies_on_mount(Config) when is_list(Config) ->
    Dir = subdir(Config, "on_mount"),
    Hook = fun(B) -> B#{title => ~"FromHook"} end,
    {[Out], []} = arizona_static:generate(
        Dir, [{arizona_static_page, ~"index.html", #{on_mount => [Hook]}}]
    ),
    ?assertMatch({_, _}, binary:match(read(Out), ~"FromHook")).

multiple_specs_return_all_paths(Config) when is_list(Config) ->
    Dir = subdir(Config, "batch"),
    {[A, B], []} = arizona_static:generate(Dir, [
        {arizona_static_page, ~"a.html"},
        {arizona_static_page, ~"sub/b.html", #{bindings => #{title => ~"Bee"}}}
    ]),
    ?assert(filelib:is_regular(A)),
    ?assertMatch({_, _}, binary:match(read(B), ~"Bee")).

default_opts_apply_to_every_spec(Config) when is_list(Config) ->
    Dir = subdir(Config, "defaults"),
    {[P1, P2], []} = arizona_static:generate(
        Dir,
        [{arizona_static_page, ~"a.html"}, {arizona_static_page, ~"b.html"}],
        #{layouts => [{arizona_layout, render}]}
    ),
    %% the shared layout wraps both pages
    ?assertMatch({_, _}, binary:match(read(P1), ~"<!DOCTYPE html>")),
    ?assertMatch({_, _}, binary:match(read(P2), ~"<!DOCTYPE html>")).

spec_opts_override_defaults(Config) when is_list(Config) ->
    Dir = subdir(Config, "override"),
    {[Out], []} = arizona_static:generate(
        Dir,
        [{arizona_static_page, ~"index.html", #{bindings => #{title => ~"Override"}}}],
        #{bindings => #{title => ~"Default"}}
    ),
    Content = read(Out),
    ?assertMatch({_, _}, binary:match(Content, ~"Override")),
    ?assertEqual(nomatch, binary:match(Content, ~"Default")).

collects_failures(Config) when is_list(Config) ->
    Dir = subdir(Config, "failures"),
    %% A good spec is written; a spec whose handler does not exist is collected
    %% in Failed (its `mount/1` raises `undef`) without stopping the batch.
    {[Good], [{BadSpec, Failure}]} = arizona_static:generate(Dir, [
        {arizona_static_page, ~"ok.html"},
        {nonexistent_handler, ~"bad.html"}
    ]),
    ?assert(filelib:is_regular(Good)),
    ?assertEqual({nonexistent_handler, ~"bad.html"}, BadSpec),
    %% Class AND stacktrace, not a bare `undef`: one failing page in a 200-page
    %% build has to say what and where, so the entry keeps the whole exception.
    ?assertMatch(
        {error, undef, [{nonexistent_handler, mount, [_Bindings], _Info} | _]}, Failure
    ),
    ?assertNot(filelib:is_regular(filename:join(Dir, "bad.html"))).

failure_carries_render_location(Config) when is_list(Config) ->
    Dir = subdir(Config, "failure_location"),
    %% A render-time crash: the `arizona_loc` wrapper `arizona_render` attaches
    %% (the template module + line that produced the bad value) must survive
    %% into the `Failed` entry, alongside the frame that raised.
    {[], [{_BadSpec, Failure}]} = arizona_static:generate(Dir, [
        {arizona_static_page, ~"bad.html", #{bindings => #{title => [<<255>>]}}}
    ]),
    ?assertMatch(
        {error, {arizona_loc, {arizona_static_page, _Line}, {non_utf8_render, 255, 0}}, [
            {arizona_template, to_bin, _Args, _Info} | _
        ]},
        Failure
    ).

failed_entries_cover_every_spec_shape(Config) when is_list(Config) ->
    Dir = subdir(Config, "every_shape"),
    Collide = filename:join(Dir, "collide"),
    ok = filelib:ensure_dir(Collide),
    ok = file:write_file(Collide, <<>>),
    %% One batch with every failing shape at once: a crashing 2-tuple spec, a
    %% crashing 3-tuple spec, and a write error. `Failed` must hold all three --
    %% a reporter that matches the SPEC shape or a single failure kind drops the
    %% ones it does not match and silently under-reports a broken build.
    {[Good], Failed} = arizona_static:generate(Dir, [
        {arizona_static_page, ~"ok.html"},
        {nonexistent_handler, ~"crash2.html"},
        {arizona_static_page, ~"crash3.html", #{bindings => #{title => [<<255>>]}}},
        {arizona_static_page, ~"collide/page.html"}
    ]),
    ?assert(filelib:is_regular(Good)),
    ?assertEqual(3, length(Failed)),
    %% The pair shape the documented reporter destructures, for every entry...
    Outfiles = [element(2, Spec) || {Spec, _Failure} <:- Failed],
    ?assertEqual([~"crash2.html", ~"crash3.html", ~"collide/page.html"], Outfiles),
    %% ...and each failure is one of the two documented kinds.
    ?assertMatch(
        [{error, undef, [_ | _]}, {error, {arizona_loc, _, _}, [_ | _]}, {error, _Posix}],
        [Failure || {_Spec, Failure} <:- Failed]
    ).

collects_write_errors(Config) when is_list(Config) ->
    Dir = subdir(Config, "write_errors"),
    %% Put a regular file where a spec needs a parent directory: filelib:ensure_dir
    %% *returns* {error, _} (it does not raise), so this exercises generate's
    %% error-return path -- distinct from a handler crash, which is caught. The
    %% batch still continues and the other spec is written.
    Collide = filename:join(Dir, "collide"),
    ok = filelib:ensure_dir(Collide),
    ok = file:write_file(Collide, <<>>),
    {[Good], [{BadSpec, _Reason}]} = arizona_static:generate(Dir, [
        {arizona_static_page, ~"ok.html"},
        {arizona_static_page, ~"collide/page.html"}
    ]),
    ?assert(filelib:is_regular(Good)),
    ?assertEqual({arizona_static_page, ~"collide/page.html"}, BadSpec),
    ?assertNot(filelib:is_regular(filename:join(Dir, "collide/page.html"))).

collects_non_map_spec_opts(Config) when is_list(Config) ->
    Dir = subdir(Config, "non_map_opts"),
    %% A 3-tuple spec is well-formed, so a non-map Opts in it is a per-page
    %% failure like any other: collected, with the specs around it still written
    %% and returned rather than lost to a batch-wide crash.
    {[First, Last], [{BadSpec, Failure}]} = arizona_static:generate(Dir, [
        {arizona_static_page, ~"first.html"},
        {arizona_static_page, ~"bad.html", not_a_map},
        {arizona_static_page, ~"last.html"}
    ]),
    ?assert(filelib:is_regular(First)),
    ?assert(filelib:is_regular(Last)),
    ?assertEqual({arizona_static_page, ~"bad.html", not_a_map}, BadSpec),
    ?assertMatch({error, {badmap, not_a_map}, [_ | _]}, Failure),
    ?assertNot(filelib:is_regular(filename:join(Dir, "bad.html"))).

%% --------------------------------------------------------------------
%% Helpers
%% --------------------------------------------------------------------

%% The static client delegates exactly the names the page carries: a counter's
%% `az-click` is proven by the compiler and lands before `</body>`.
embeds_compile_time_proven_attrs(Config) when is_list(Config) ->
    Content = generate_with_layout(Config, "proven_compile", arizona_counter),
    ?assertEqual([~"az-click"], embedded_attrs(Content)),
    {Script, _} = binary:match(Content, ~"<script type=\"application/json\" id=\"arizona-attrs\">"),
    {Body, _} = binary:match(Content, ~"</body>"),
    ?assert(Script < Body).

%% A command built at render time is invisible to the compiler: only the armed
%% collector proves `az-dblclick`.
embeds_render_time_proven_attrs(Config) when is_list(Config) ->
    Content = generate_with_layout(Config, "proven_render", arizona_static_runtime_cmd),
    ?assertEqual([~"az-dblclick"], embedded_attrs(Content)).

page_without_events_has_no_attrs(Config) when is_list(Config) ->
    Content = generate_with_layout(Config, "no_events", arizona_static_page),
    ?assertEqual(nomatch, binary:match(Content, ~"arizona-attrs")).

%% With no `</body>` to anchor it (a fragment, a `?native` document), nothing is
%% appended: the page stays exactly what rendering produced.
page_without_body_is_left_as_rendered(Config) when is_list(Config) ->
    Dir = subdir(Config, "no_body"),
    {[Out], []} = arizona_static:generate(Dir, [{arizona_counter, ~"index.html"}]),
    Content = read(Out),
    ?assertEqual(nomatch, binary:match(Content, ~"arizona-attrs")),
    ?assertEqual(
        iolist_to_binary(arizona_render:render_view_to_iolist(arizona_counter, #{})), Content
    ).

%% The collector is armed for each page's render only, so the caller's process is
%% not left recording what it renders later.
leaves_the_collector_disarmed(Config) when is_list(Config) ->
    _Content = generate_with_layout(Config, "disarmed", arizona_counter),
    ok = arizona_event_attrs:observe_attr(~"az-later"),
    ?assertEqual({[], []}, arizona_event_attrs:drain()).

generate_with_layout(Config, Name, Handler) ->
    Dir = subdir(Config, Name),
    {[Out], []} = arizona_static:generate(
        Dir, [{Handler, ~"index.html", #{layouts => [{arizona_layout, render}]}}]
    ),
    read(Out).

embedded_attrs(Content) ->
    {match, [Json]} = re:run(
        Content,
        ~"<script type=\"application/json\" id=\"arizona-attrs\">(.*?)</script>",
        [{capture, all_but_first, binary}]
    ),
    json:decode(Json).

subdir(Config, Name) ->
    filename:join(proplists:get_value(priv_dir, Config), Name).

read(Path) ->
    {ok, Bin} = file:read_file(Path),
    Bin.
