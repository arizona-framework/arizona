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
    {Bindings#{id => ~"bench_each", items => Stream}, #{}}.

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
                        {span, [], arizona_template:get(field_e, Item)}
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
    {Bindings#{items => Items1}, #{}, []}.
