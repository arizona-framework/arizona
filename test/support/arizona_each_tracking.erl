-module(arizona_each_tracking).
-moduledoc """
Stateless templates whose `?each` callbacks read an outer binding (`suffix`), so a
change to it must reach the items. Each function is a distinct scenario driven by
`arizona_diff_SUITE`.
""".
-include("arizona_stateless.hrl").
-export([list/1]).
-export([stream/1]).
-export([stream_mixed/1]).
-export([map/1]).
-export([attr/1]).
-export([branch/1]).
-export([hoisted/1]).
-export([named/1]).
-export([hoisted_read/1]).
-export([item_stateless/1]).
-export([item_branch/1]).
-export([item_element_branch/1]).
-export([computed/1]).
-export([nested_list/1]).
-export([nested_stream_in_list/1]).
-export([nested_list_in_stream/1]).
-export([nested_stream_in_stream/1]).
-export([unrelated/1]).
-export([nodiff_item/1]).
-export([nodiff_slot/1]).

list(Bindings) ->
    ?html({ul, [], [?each(fun(I) -> {li, [], [I, ?get(suffix)]} end, ?get(items))]}).

stream(Bindings) ->
    ?html(
        {ul, [], [?each(fun(I, _K) -> {li, [], [maps:get(n, I), ?get(suffix)]} end, ?get(s))]}
    ).

%% One slot reads both an item field (plain `maps:get/2`, untracked) and a binding.
stream_mixed(Bindings) ->
    ?html(
        {ul, [], [?each(fun(I, _K) -> {li, [], [[maps:get(n, I), ?get(suffix)]]} end, ?get(s))]}
    ).

map(Bindings) ->
    ?html({ul, [], [?each(fun(V, K) -> {li, [], [K, V, ?get(suffix)]} end, ?get(m))]}).

attr(Bindings) ->
    ?html({ul, [], [?each(fun(I) -> {li, [{class, ?get(suffix)}], [I]} end, ?get(items))]}).

%% The each is a conditional branch's value.
branch(Bindings) ->
    ?html(
        {ul, [], [
            case ?get(f) of
                true -> ?each(fun(I) -> {li, [], [I, ?get(suffix)]} end, ?get(items));
                false -> ~"none"
            end
        ]}
    ).

hoisted(Bindings) ->
    Rows = ?each(fun(I) -> {li, [], [I, ?get(suffix)]} end, ?get(items)),
    ?html({ul, [], [Rows]}).

named(Bindings) ->
    ?html({ul, [], [?each(fun(I) -> row(I, Bindings) end, ?get(items))]}).

row(I, Bindings) ->
    {li, [], [I, ?get(suffix)]}.

%% The read is hoisted out of the callback and reaches it as a variable.
hoisted_read(Bindings) ->
    Suffix = ?get(suffix),
    ?html({ul, [], [?each(fun(I) -> {li, [], [I, Suffix]} end, ?get(items))]}).

%% The read is a `?stateless` prop inside the item.
item_stateless(Bindings) ->
    ?html(
        {ul, [], [
            ?each(
                fun(I) -> {li, [], [?stateless(fun tag/1, #{i => I, suffix => ?get(suffix)})]} end,
                ?get(items)
            )
        ]}
    ).

tag(Bindings) ->
    ?html({b, [], [?get(i), ?get(suffix)]}).

%% The binding is read only when the item takes one branch.
item_branch(Bindings) ->
    ?html(
        {ul, [], [
            ?each(
                fun(I) ->
                    {li, [], [
                        case I of
                            ~"a" -> ?get(suffix);
                            _ -> ~"-"
                        end
                    ]}
                end,
                ?get(items)
            )
        ]}
    ).

%% The same, with the read inside an element the branch returns.
item_element_branch(Bindings) ->
    ?html(
        {ul, [], [
            ?each(
                fun(I) ->
                    {li, [], [
                        case I of
                            ~"a" -> {b, [], [?get(suffix)]};
                            _ -> ~"-"
                        end
                    ]}
                end,
                ?get(items)
            )
        ]}
    ).

%% The key is computed, so no compile-time analysis can name it.
computed(Bindings) ->
    Key = binary_to_atom(~"suffix"),
    ?html({ul, [], [?each(fun(I) -> {li, [], [I, ?get(Key)]} end, ?get(items))]}).

nested_list(Bindings) ->
    ?html(
        {ul, [], [
            ?each(
                fun(I) -> {li, [], [?each(fun(J) -> {b, [], [J, ?get(suffix)]} end, [I])]} end,
                ?get(items)
            )
        ]}
    ).

nested_stream_in_list(Bindings) ->
    ?html(
        {ul, [], [
            ?each(
                fun(S) ->
                    {li, [], [
                        ?each(fun(I, _K) -> {b, [], [maps:get(n, I), ?get(suffix)]} end, S)
                    ]}
                end,
                ?get(groups)
            )
        ]}
    ).

nested_list_in_stream(Bindings) ->
    ?html(
        {ul, [], [
            ?each(
                fun(I, _K) ->
                    {li, [], [?each(fun(J) -> {b, [], [J, ?get(suffix)]} end, [maps:get(n, I)])]}
                end,
                ?get(s)
            )
        ]}
    ).

nested_stream_in_stream(Bindings) ->
    ?html(
        {ul, [], [
            ?each(
                fun(G, _K) ->
                    {li, [], [
                        ?each(
                            fun(I, _K2) -> {b, [], [maps:get(n, I), ?get(suffix)]} end,
                            maps:get(inner, G)
                        )
                    ]}
                end,
                ?get(gs)
            )
        ]}
    ).

unrelated(Bindings) ->
    ?html(
        {'div', [], [
            {p, [], [?get(other)]},
            {ul, [], [?each(fun(I) -> {li, [], [I, ?get(suffix)]} end, ?get(items))]}
        ]}
    ).

%% An az-nodiff item renders once.
nodiff_item(Bindings) ->
    ?html({ul, [], [?each(fun(I) -> {li, ['az-nodiff'], [I, ?get(suffix)]} end, ?get(items))]}).

%% So does a whole az-nodiff element holding the each.
nodiff_slot(Bindings) ->
    ?html(
        {'div', [], [
            {p, [], [?get(other)]},
            ?html(
                {ul, ['az-nodiff'], [?each(fun(I) -> {li, [], [I, ?get(suffix)]} end, ?get(items))]}
            )
        ]}
    ).
