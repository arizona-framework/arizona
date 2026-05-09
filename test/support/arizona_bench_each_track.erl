-module(arizona_bench_each_track).
-include("arizona_view.hrl").
-export([mount/2, render/1, handle_event/3]).

%% Bench fixture: a `?each` stream where every per-item dynamic uses
%% `arizona_template:get(K, Item)` for tracked reads. Five fields per
%% row, five dynamics per row -- so a single-field update lets the
%% per-item skip path elide four of the five closure invocations.

-spec mount(az:bindings(), az:request()) -> az:mount_ret().
mount(Bindings, _Req) ->
    Items = maps:get(items, Bindings, []),
    Stream = arizona_stream:new(fun(#{id := Id}) -> Id end, Items),
    {#{id => ~"bench_each", items => Stream}, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {'div', [{id, ?get(id)}], [
            ?each(
                fun(Item, Key) ->
                    {'div', [{az_key, Key}], [
                        {span, [], arizona_template:get(field_a, Item)},
                        {span, [], arizona_template:get(field_b, Item)},
                        {span, [], arizona_template:get(field_c, Item)},
                        {span, [], arizona_template:get(field_d, Item)},
                        {span, [], arizona_template:get(field_e, Item)},
                        {span, [], arizona_template:get(field_f, Item)},
                        {span, [], arizona_template:get(field_g, Item)},
                        {span, [], arizona_template:get(field_h, Item)},
                        {span, [], arizona_template:get(field_i, Item)},
                        {span, [], arizona_template:get(field_j, Item)},
                        {span, [], arizona_template:get(field_k, Item)},
                        {span, [], arizona_template:get(field_l, Item)},
                        {span, [], arizona_template:get(field_m, Item)},
                        {span, [], arizona_template:get(field_n, Item)},
                        {span, [], arizona_template:get(field_o, Item)},
                        {span, [], arizona_template:get(field_p, Item)},
                        {span, [], arizona_template:get(field_q, Item)},
                        {span, [], arizona_template:get(field_r, Item)},
                        {span, [], arizona_template:get(field_s, Item)},
                        {span, [], arizona_template:get(field_t, Item)}
                    ]}
                end,
                ?get(items)
            )
        ]}
    ).

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"update_field_a", #{~"id" := Id, ~"value" := Value}, Bindings) ->
    Items0 = maps:get(items, Bindings),
    OldItem = arizona_stream:get(Items0, Id),
    NewItem = OldItem#{field_a => Value},
    Items1 = arizona_stream:update(Items0, Id, NewItem),
    {Bindings#{items => Items1}, #{}, []};
handle_event(~"update_unchanged", #{~"id" := Id}, Bindings) ->
    Items0 = maps:get(items, Bindings),
    SameItem = arizona_stream:get(Items0, Id),
    Items1 = arizona_stream:update(Items0, Id, SameItem),
    {Bindings#{items => Items1}, #{}, []};
handle_event(~"reset_with_overlap", #{~"value" := Value}, Bindings) ->
    Items0 = maps:get(items, Bindings),
    %% Replace every item, changing only field_a -- maximal overlap with
    %% one differing field, exercising the per-item skip in smart_reset_items.
    NewItems = [
        Item#{field_a => Value}
     || Item <- arizona_stream:to_list(Items0)
    ],
    Items1 = arizona_stream:reset(Items0, NewItems),
    {Bindings#{items => Items1}, #{}, []}.
