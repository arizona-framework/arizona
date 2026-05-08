-module(arizona_todo).
-include("arizona_view.hrl").
-export([mount/2, render/1, handle_event/3]).

-spec mount(az:bindings(), az:request()) -> az:mount_ret().
mount(Bindings, _Req) ->
    Items = maps:get(items, Bindings, []),
    Stream = arizona_stream:new(fun(#{id := Id}) -> Id end, Items),
    {#{id => ~"todo", items => Stream}, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {ul, [{id, ?get(id)}], [
            ?each(
                fun(#{text := Text}, Key) ->
                    {li, [{az_key, Key}], [Text]}
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
handle_event(~"update", #{~"id" := Id, ~"text" := Text}, Bindings) ->
    S = arizona_stream:update(maps:get(items, Bindings), Id, #{id => Id, text => Text}),
    {Bindings#{items => S}, #{}, []};
handle_event(~"clear", _Payload, Bindings) ->
    S = arizona_stream:reset(maps:get(items, Bindings)),
    {Bindings#{items => S}, #{}, []};
handle_event(~"move", #{~"id" := Id, ~"pos" := Pos}, Bindings) ->
    S = arizona_stream:move(maps:get(items, Bindings), Id, Pos),
    {Bindings#{items => S}, #{}, []};
handle_event(~"insert_at", #{~"id" := Id, ~"text" := Text, ~"pos" := Pos}, Bindings) ->
    S = arizona_stream:insert(maps:get(items, Bindings), #{id => Id, text => Text}, Pos),
    {Bindings#{items => S}, #{}, []};
handle_event(~"reset_with", #{~"items" := Items}, Bindings) ->
    S = arizona_stream:reset(maps:get(items, Bindings), Items),
    {Bindings#{items => S}, #{}, []}.
