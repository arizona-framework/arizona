-module(arizona_nodiff_regions).
-moduledoc """
Slots holding an `az-nodiff` region: the server decides whether the region exists
and which branch produced it, the client owns its DOM once it is there.
""".
-include("arizona_stateless.hrl").
-export([toggle/1]).
-export([switch/1]).
-export([helper_switch/1]).
-export([one_branch/1]).
-export([wrapper_switch/1]).
-export([keyed/1]).
-export([child_paths/1]).
-export([root_child/1]).
-export([region_body/1]).
-export([widget/1]).
-export([list_widgets/1]).
-export([stream_widgets/1]).

-spec toggle(az:bindings()) -> az:template().
toggle(Bindings) ->
    ?html(
        {'div', [], [
            case ?get(show) of
                true -> {section, ['az-nodiff'], [?get(v)]};
                false -> <<>>
            end
        ]}
    ).

%% `a` and `b` share their statics; `c` does not.
-spec switch(az:bindings()) -> az:template().
switch(Bindings) ->
    ?html(
        {'div', [], [
            case ?get(kind) of
                a -> {p, ['az-nodiff'], [~"W ", ?get(v)]};
                b -> {p, ['az-nodiff'], [~"W ", ?get(v)]};
                c -> {ul, ['az-nodiff'], [{li, [], [?get(v)]}]}
            end
        ]}
    ).

%% Two clauses calling the same helper with the same arguments: one inlined at
%% compile time, one called at runtime, one a `?stateless` descriptor.
-spec helper_switch(az:bindings()) -> az:template().
helper_switch(Bindings) ->
    ?html(
        {'div', [], [
            case ?get(kind) of
                line -> chart(?get(v));
                bar -> chart(?get(v))
            end,
            case ?get(kind) of
                line -> chart_rt(?get(v));
                bar -> chart_rt(?get(v))
            end,
            case ?get(kind) of
                line -> ?stateless(fun widget/1, #{v => ?get(v)});
                bar -> ?stateless(fun widget/1, #{v => ?get(v)})
            end
        ]}
    ).

chart(Type) ->
    {'div', ['az-nodiff', {class, Type}], [Type]}.

chart_rt(Type) ->
    Class = <<"chart ", Type/binary>>,
    ?html({'div', ['az-nodiff', {class, Class}], [Type]}).

-spec one_branch(az:bindings()) -> az:template().
one_branch(Bindings) ->
    ?html(
        {'div', [], [
            case ?get(n) > 0 of
                true -> {section, ['az-nodiff'], [?get(n), ?get(v)]};
                false -> ~"off"
            end
        ]}
    ).

%% A switch above the region's own slot, between wrappers with the same statics,
%% with and without an `az_key` naming the switch.
-spec wrapper_switch(az:bindings()) -> az:template().
wrapper_switch(Bindings) ->
    ?html(
        {'div', [], [
            case ?get(tab) of
                a -> {section, [{class, ~"tab"}], [?stateless(fun widget/1, #{v => ?get(v)})]};
                b -> {section, [{class, ~"tab"}], [?stateless(fun widget/1, #{v => ?get(v)})]}
            end,
            case ?get(tab) of
                a -> {section, [], [?stateless(fun widget/1, #{v => ?get(v), rev => ?get(tab)})]};
                b -> {section, [], [?stateless(fun widget/1, #{v => ?get(v), rev => ?get(tab)})]}
            end
        ]}
    ).

%% The same widget with and without `az_key`, in a branch and from a runtime call.
-spec keyed(az:bindings()) -> az:template().
keyed(Bindings) ->
    ?html(
        {'div', [], [
            case ?get(on) of
                true -> keyed_chart(?get(type));
                false -> <<>>
            end,
            case ?get(on) of
                true -> chart(?get(type));
                false -> <<>>
            end,
            keyed_chart_rt(?get(type))
        ]}
    ).

keyed_chart(Type) ->
    {'div', ['az-nodiff', {az_key, Type}], [Type]}.

keyed_chart_rt(Type) ->
    Label = <<"chart ", Type/binary>>,
    ?html({'div', ['az-nodiff', {az_key, Type}], [Label]}).

%% A child view inside a nodiff region, reached through a branch, a `?stateless`
%% body and a list item.
-spec child_paths(az:bindings()) -> az:template().
child_paths(Bindings) ->
    ?html(
        {'div', [], [
            case ?get(on) of
                true ->
                    {section, ['az-nodiff'], [
                        ?stateful(arizona_counter, #{id => ~"in_branch", count => ?get(n)})
                    ]};
                false ->
                    <<>>
            end,
            ?stateless(fun region_body/1, #{n => ?get(n)}),
            {ul, [], [
                ?each(
                    fun(Item) ->
                        {li, [], [?stateless(fun region_item/1, Item)]}
                    end,
                    ?get(items)
                )
            ]}
        ]}
    ).

%% A child view whose own root is az-nodiff, named by a binding.
-spec root_child(az:bindings()) -> az:template().
root_child(Bindings) ->
    ?html(
        {'div', [], [
            ?stateful(arizona_no_diff_counter, #{id => ?get(cid), count => ?get(n)})
        ]}
    ).

-spec region_body(az:bindings()) -> az:template().
region_body(Bindings) ->
    ?html(
        {section, ['az-nodiff'], [
            ?stateful(arizona_counter, #{id => ~"in_stateless", count => ?get(n)})
        ]}
    ).

region_item(Bindings) ->
    ?html(
        {section, ['az-nodiff'], [
            ?stateful(arizona_counter, #{id => ?get(id), count => ?get(n)})
        ]}
    ).

-spec widget(az:bindings()) -> az:template().
widget(Bindings) ->
    ?html({'div', ['az-nodiff', {class, ~"widget"}, {az_key, ?get(rev, 0)}], [?get(v)]}).

-spec list_widgets(az:bindings()) -> az:template().
list_widgets(Bindings) ->
    ?html(
        {ul, [], [
            ?each(
                fun(Item) -> {li, [], [?stateless(fun widget/1, Item)]} end,
                ?get(items)
            )
        ]}
    ).

-spec stream_widgets(az:bindings()) -> az:template().
stream_widgets(Bindings) ->
    ?html(
        {ul, [], [
            ?each(
                fun(Item, Key) -> {li, [{az_key, Key}], [?stateless(fun widget/1, Item)]} end,
                ?get(items)
            )
        ]}
    ).
