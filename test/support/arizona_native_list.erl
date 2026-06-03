-module(arizona_native_list).
-include("arizona_stateful.hrl").
-export([mount/1, render/1, handle_event/3]).

%% Native (JSON) stream view for diff-op tests. Mirrors arizona_todo but renders
%% via ?native, so stream insert/remove/move/update produce
%% OP_INSERT/OP_REMOVE/OP_MOVE/OP_ITEM_PATCH whose item payloads carry native
%% JSON statics.

-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings) ->
    Items = maps:get(items, Bindings, []),
    Stream = arizona_stream:new(fun(#{id := Id}) -> Id end, Items),
    {#{id => ~"native_list", items => Stream}, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?native(
        {'Column', [{id, ?get(id)}], [
            ?each(
                fun(#{text := Text}, Key) ->
                    {'Text', [{az_key, Key}], [Text]}
                end,
                ?get(items)
            )
        ]}
    ).

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"add", #{~"id" := Id, ~"text" := Text}, Bindings) ->
    S = arizona_stream:insert(maps:get(items, Bindings), #{id => Id, text => Text}),
    {Bindings#{items => S}, #{}, []};
handle_event(~"remove", #{~"id" := Id}, Bindings) ->
    S = arizona_stream:delete(maps:get(items, Bindings), Id),
    {Bindings#{items => S}, #{}, []};
handle_event(~"move", #{~"id" := Id, ~"pos" := Pos}, Bindings) ->
    S = arizona_stream:move(maps:get(items, Bindings), Id, Pos),
    {Bindings#{items => S}, #{}, []};
handle_event(~"update", #{~"id" := Id, ~"text" := Text}, Bindings) ->
    S = arizona_stream:update(maps:get(items, Bindings), Id, #{id => Id, text => Text}),
    {Bindings#{items => S}, #{}, []};
handle_event(~"reset", #{~"items" := Items}, Bindings) ->
    NewItems = [#{id => Id, text => Text} || #{~"id" := Id, ~"text" := Text} <:- Items],
    S = arizona_stream:reset(maps:get(items, Bindings), NewItems),
    {Bindings#{items => S}, #{}, []}.
