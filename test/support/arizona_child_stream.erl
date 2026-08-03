-module(arizona_child_stream).
-include("arizona_stateful.hrl").
-export([mount/1]).
-export([render/1]).
-export([handle_event/3]).

%% Embeddable child that owns a stream, plus a `label` prop the parent drives.
%% Its own event patches the stream incrementally (an INSERT); a later parent
%% re-render must not undo that with a wholesale re-render of the container.

-spec mount(az:bindings()) -> az:mount_ret().
mount(Props) ->
    Items = maps:get(items, Props, [#{id => ~"i1"}]),
    {
        #{
            id => maps:get(id, Props),
            label => maps:get(label, Props, ~"L0"),
            items => arizona_stream:new(fun(#{id := Id}) -> Id end, Items)
        },
        #{}
    }.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {'div', [{id, ?get(id)}], [
            {h1, [], [?get(label)]},
            {ul, [], [
                ?each(
                    fun(#{id := ItemId}, Key) ->
                        {li, [{az_key, Key}], [ItemId]}
                    end,
                    ?get(items)
                )
            ]}
        ]}
    ).

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"add", #{~"id" := Id}, Bindings) ->
    Stream = arizona_stream:insert(maps:get(items, Bindings), #{id => Id}),
    {Bindings#{items => Stream}, #{}, []}.
