-module(arizona_carry_leaf).
-include("arizona_stateful.hrl").
-export([mount/1]).
-export([render/1]).
-export([handle_event/3]).
-export([unmount/1]).

%% Grandchild whose unmount is observable and whose own events are addressable,
%% so a suite can tell "still live" from "unmounted and silently dropped".

-spec mount(az:bindings()) -> az:mount_ret().
mount(Props) ->
    {
        #{
            id => maps:get(id, Props),
            notify => maps:get(notify, Props),
            count => 0,
            %% A stream, so a stale copy of THIS view shows up as the destructive
            %% wholesale `?OP_UPDATE` rather than a merely redundant text op.
            items => arizona_stream:new(fun(#{id := Id}) -> Id end, [#{id => ~"a"}])
        },
        #{}
    }.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {'div', [{id, ?get(id)}], [
            {span, [], [?get(count)]},
            {ul, [], [
                ?each(fun(#{id := ItemId}, Key) -> {li, [{az_key, Key}], [ItemId]} end, ?get(items))
            ]}
        ]}
    ).

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"inc", _Payload, Bindings) ->
    {Bindings#{count => maps:get(count, Bindings) + 1}, #{}, []};
handle_event(~"add", #{~"id" := Id}, Bindings) ->
    {Bindings#{items => arizona_stream:insert(maps:get(items, Bindings), #{id => Id})}, #{}, []}.

-spec unmount(az:bindings()) -> ok.
unmount(#{id := Id, notify := Notify}) ->
    Notify ! {leaf_unmounted, Id},
    ok.
