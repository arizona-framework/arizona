-module(arizona_stream_self_send).
-include("arizona_stateful.hrl").
-export([mount/1]).
-export([render/1]).
-export([handle_event/3]).
-export([handle_info/2]).

%% The documented `?send` self-message idiom on an order-dependent stream:
%% `handle_event/3` appends a key and enqueues a message to this same view, so
%% the live process replies with the INSERT and only THEN handles the info that
%% MOVEs the just-inserted key. A transport that folds the queued push in front
%% of the reply inverts the pair -- the client sees a MOVE for a key it has not
%% inserted yet, warns, and drops the move permanently.

-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings) ->
    Items = maps:get(items, Bindings, [#{id => ~"a"}, #{id => ~"b"}]),
    {
        #{
            id => ~"self_send",
            items => arizona_stream:new(fun(#{id := Id}) -> Id end, Items)
        },
        #{}
    }.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {ul, [{id, ?get(id)}], [
            ?each(
                fun(#{id := ItemId}, Key) ->
                    {li, [{az_key, Key}], [ItemId]}
                end,
                ?get(items)
            )
        ]}
    ).

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"add_then_move", #{~"id" := Id}, Bindings) ->
    Stream = arizona_stream:insert(maps:get(items, Bindings), #{id => Id}),
    %% `?send` returns the message it queued on this view's own mailbox; match it
    %% rather than drop it, and the match doubles as documentation of what lands
    %% there while this callback is still running.
    {arizona_view, _ViewId, {move_to_front, Id}} = ?send({move_to_front, Id}),
    {Bindings#{items => Stream}, #{}, []}.

-spec handle_info(term(), az:bindings()) -> az:handle_info_ret().
handle_info({move_to_front, Id}, Bindings) ->
    Stream = arizona_stream:move(maps:get(items, Bindings), Id, 0),
    {Bindings#{items => Stream}, #{}, []}.
