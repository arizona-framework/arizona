-module(arizona_bench_nested_each).
-moduledoc """
Bench fixture: 10 sections, each containing 10 items, rendered via two
levels of `?each`. Exercises nested list-comprehension rendering --
`arizona_eval:render_list_items_simple/2` runs at the section level,
then again at each section's item level. Total: 100 leaf renders, but
through a tree shape rather than a flat 100-item list.
""".
-include("arizona_view.hrl").

-export([mount/2]).
-export([render/1]).

-spec mount(az:bindings(), az:request()) -> az:mount_ret().
mount(Bindings, _Req) ->
    %% Lazy default for `sections` -- the bench workload pre-generates the
    %% 10x10 data once and passes it via bindings, so we never want to
    %% recompute `default_sections()` on hot-path renders.
    Sections =
        case Bindings of
            #{sections := V} -> V;
            #{} -> default_sections()
        end,
    {#{id => ~"page", sections => Sections}, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {main, [{id, ?get(id)}], [
            ?each(
                fun(Section) ->
                    {'div', [{class, ~"section"}], [
                        {h2, [], [arizona_template:get(title, Section)]},
                        {ul, [], [
                            ?each(
                                fun(Item) ->
                                    {li, [], [arizona_template:get(text, Item)]}
                                end,
                                arizona_template:get(items, Section)
                            )
                        ]}
                    ]}
                end,
                ?get(sections)
            )
        ]}
    ).

default_sections() ->
    [
        #{
            id => SectId,
            title => iolist_to_binary(io_lib:format("Section ~b", [SectId])),
            items => [
                #{
                    id => ItemId,
                    text => iolist_to_binary(io_lib:format("Item ~b-~b", [SectId, ItemId]))
                }
             || ItemId <- lists:seq(1, 10)
            ]
        }
     || SectId <- lists:seq(1, 10)
    ].
