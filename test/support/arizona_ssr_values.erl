-module(arizona_ssr_values).
-include("arizona_stateless.hrl").
-export([render/1, render_nodiff/1, render_stream/1, render_map/1, values/0]).
-export([render_item_crash/1, render_stream_item_crash/1, render_top_crash/1, crash_line/0]).

%% Fixture: one content slot per value shape the SSR escape path distinguishes, at
%% the top level and as `?each` item content (list, stream and map sources), in a
%% diffable and an `az-nodiff` template. SSR shortcuts binaries and integers; every
%% other shape must still render exactly as the live path renders it.

-spec values() -> [term()].
values() ->
    [
        ~"<a href=\"x\">Tom & Jerry's</a>",
        ~"plain",
        <<>>,
        42,
        -7,
        1.5,
        some_atom,
        "caf\x{e9} <b>",
        [~"io", $&, ~"data"],
        helper_raw(~"<i>raw</i>"),
        ?html({em, [], [~"nested"]}),
        ?stateless(fun badge/1, #{label => ~"<s>"}),
        arizona_js:push_event(~"x"),
        helper_local(~"<l>")
    ].

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {'div', [], [
            {p, [], [?get(v)]},
            {ul, [], [?each(fun(V) -> {li, [], [V]} end, ?get(items))]}
        ]}
    ).

-spec render_nodiff(az:bindings()) -> az:template().
render_nodiff(Bindings) ->
    ?html(
        {'div', [az_nodiff], [
            {p, [], [?get(v)]},
            {ul, [], [?each(fun(V) -> {li, [az_nodiff], [V]} end, ?get(items))]}
        ]}
    ).

%% `items` is the value list; it is keyed by position for the stream and map sources.
-spec render_stream(az:bindings()) -> az:template().
render_stream(Bindings) ->
    ?html(
        {ul, [], [
            ?each(
                fun({_I, V}, Key) -> {li, [{az_key, Key}], [V]} end,
                arizona_stream:new(fun({I, _V}) -> I end, lists:enumerate(?get(items)))
            )
        ]}
    ).

-spec render_map(az:bindings()) -> az:template().
render_map(Bindings) ->
    ?html(
        {ul, [], [
            ?each(fun(_I, V) -> {li, [], [V]} end, maps:from_list(lists:enumerate(?get(items))))
        ]}
    ).

-spec badge(az:bindings()) -> az:template().
badge(Bindings) ->
    ?html({span, [], [?get(label)]}).

%% A crash inside a nested template, reached from a top-level slot and from list and
%% stream items: SSR locates all three at the slot that crashed, the line below.
-spec crash_line() -> pos_integer().
crash_line() -> ?LINE + 1.
crashy(Bindings) -> ?html({b, [], [boom(?get(x))]}).

-spec render_item_crash(az:bindings()) -> az:template().
render_item_crash(Bindings) ->
    ?html(
        {ul, [], [
            ?each(fun(V) -> {li, [], [?stateless(fun crashy/1, #{x => V})]} end, ?get(items))
        ]}
    ).

-spec render_stream_item_crash(az:bindings()) -> az:template().
render_stream_item_crash(Bindings) ->
    ?html(
        {ul, [], [
            ?each(
                fun(V, Key) -> {li, [{az_key, Key}], [?stateless(fun crashy/1, #{x => V})]} end,
                arizona_stream:new(fun(V) -> V end, ?get(items))
            )
        ]}
    ).

-spec render_top_crash(az:bindings()) -> az:template().
render_top_crash(Bindings) ->
    ?html({p, [], [?stateless(fun crashy/1, #{x => ?get(v)})]}).

boom(V) ->
    error({boom, V}).

%% A helper returning `?raw` into an escaping slot: the documented rule escapes it.
helper_raw(V) ->
    arizona_template:raw(V).

%% A `?local` reaching a slot through a helper, which a literal one in an escaping
%% position cannot (the transform rejects it).
helper_local(V) ->
    arizona_template:local(~"k", V).
