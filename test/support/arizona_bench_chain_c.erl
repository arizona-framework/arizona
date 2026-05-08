-module(arizona_bench_chain_c).
-moduledoc """
Bench fixture: leaf stateful in the 3-level chain
(view -> chain_b -> chain_c -> leaves). Renders a 10-item `?each`
list, so the profile exercises both stateful-chain propagation AND
list iteration at the leaf -- the realistic shape of nested
section/row components in real apps.
""".
-include("arizona_stateful.hrl").

-export([mount/1]).
-export([render/1]).
-export([handle_update/2]).

-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings) ->
    %% Lazy default: only generate the 10 leaf items if no override was
    %% passed. Eager maps:merge would re-run default_items()/0 on every
    %% mount even when items were provided.
    Bindings1 = maps:merge(#{id => ~"chain_c"}, Bindings),
    Bindings2 =
        case Bindings1 of
            #{items := _} -> Bindings1;
            _ -> Bindings1#{items => default_items()}
        end,
    {Bindings2, #{}}.

-spec handle_update(az:bindings(), az:bindings()) -> az:handle_update_ret().
handle_update(Props, Bindings) ->
    {maps:merge(Bindings, Props), #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {'div', [{id, ?get(id)}, {class, ~"level-c"}], [
            {h2, [], [~"Level C"]},
            {ul, [], [
                ?each(
                    fun(Item) ->
                        {li, [], [arizona_template:get(text, Item)]}
                    end,
                    ?get(items)
                )
            ]}
        ]}
    ).

%% Literal default to avoid per-mount io_lib:format overhead -- the
%% stateful chain re-mounts on every SSR render, so any computation in
%% the default would dominate the profile.
default_items() ->
    [
        #{id => 1, text => ~"Item 1"},
        #{id => 2, text => ~"Item 2"},
        #{id => 3, text => ~"Item 3"},
        #{id => 4, text => ~"Item 4"},
        #{id => 5, text => ~"Item 5"},
        #{id => 6, text => ~"Item 6"},
        #{id => 7, text => ~"Item 7"},
        #{id => 8, text => ~"Item 8"},
        #{id => 9, text => ~"Item 9"},
        #{id => 10, text => ~"Item 10"}
    ].
