-module(arizona_native_stream_child).
-include("arizona_stateful.hrl").
-export([mount/1]).
-export([render/1]).
-export([handle_event/3]).

%% Native (JSON) stream whose ITEM conditionally renders a stateful child.
%% Opening an item ships an OP_ITEM_PATCH whose INNER op installs the child's
%% subtree -- so the child's view id arrives through an item-scoped op, not a
%% top-level one. Its own ops then come back addressed to that view id, which the
%% client can only resolve if an inner-op rebuild registers the view it created.

-spec mount(az:bindings()) -> az:mount_ret().
mount(_Bindings) ->
    Items = [#{id => ~"1", label => ~"One", open => false}],
    Stream = arizona_stream:new(fun(#{id := Id}) -> Id end, Items),
    {#{id => ~"native_stream_child", items => Stream}, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?native(
        {'Column', [{id, ?get(id)}], [
            {'Button', [{on_tap, arizona_android:push_event(~"open", #{~"id" => ~"1"})}], [
                ~"Open"
            ]},
            ?each(fun row/2, ?get(items))
        ]}
    ).

row(#{id := Id, label := Label, open := Open}, Key) ->
    {'Row', [{az_key, Key}], [
        Label,
        case Open of
            true ->
                ?stateful(arizona_native_child_counter, #{
                    id => <<"kid-", Id/binary>>, label => ~"K", count => 0
                });
            false ->
                ~""
        end
    ]}.

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"open", #{~"id" := Id}, Bindings) ->
    Stream = maps:get(items, Bindings),
    Updated = arizona_stream:update(Stream, Id, #{id => Id, label => ~"One", open => true}),
    {Bindings#{items => Updated}, #{}, []}.
