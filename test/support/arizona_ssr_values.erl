-module(arizona_ssr_values).
-include("arizona_stateless.hrl").
-export([render/1, render_nodiff/1, render_item_crash/1, render_top_crash/1, values/0]).

%% Fixture: one content slot per value shape the SSR escape path distinguishes, at
%% the top level and as `?each` item content, in a diffable and an `az-nodiff`
%% template. SSR shortcuts binaries and integers; every other shape must still
%% render exactly as the live path renders it.

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
        ?stateless(fun badge/1, #{label => ~"<s>"})
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

-spec badge(az:bindings()) -> az:template().
badge(Bindings) ->
    ?html({span, [], [?get(label)]}).

%% A crash inside an item value, and one in a top-level slot: SSR wraps both with a
%% template location, the item one through its enclosing `?each` slot.
-spec render_item_crash(az:bindings()) -> az:template().
render_item_crash(Bindings) ->
    ?html({ul, [az_nodiff], [?each(fun(V) -> {li, [az_nodiff], [boom(V)]} end, ?get(items))]}).

-spec render_top_crash(az:bindings()) -> az:template().
render_top_crash(Bindings) ->
    ?html({p, [az_nodiff], [boom(?get(v))]}).

boom(V) ->
    error({boom, V}).

%% A helper returning `?raw` into an escaping slot: the documented rule escapes it.
helper_raw(V) ->
    arizona_template:raw(V).
