-module(arizona_stream_item_child).
-include("arizona_stateful.hrl").

-export([mount/1]).
-export([render/1]).
-export([handle_event/3]).

%% A stream `?each` whose items hold a `?stateful` child WHOSE PROPS COME FROM THE
%% ITEM. That is the shape that makes an item patch carry a child-view wrapper:
%% updating an item changes the child's props, the child re-renders, its inner
%% dynamics differ, and `arizona_diff:make_ops/5`'s child-view clause emits
%% `[ChildViewId, ChildOps]` INSIDE the item's inner ops.
%%
%% `arizona_stream_with_child` looks like this but passes the child a constant
%% (`count => 0`), so its child never re-renders and the wrapper is suppressed as
%% empty -- which is why no existing fixture reaches the shape.
-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings) ->
    Items = [#{id => 1, label => ~"one"}, #{id => 2, label => ~"two"}],
    {
        #{
            id => maps:get(id, Bindings, ~"sic"),
            root_label => maps:get(root_label, Bindings, ~"root"),
            items => arizona_stream:new(fun(#{id := Id}) -> Id end, Items, #{})
        },
        #{}
    }.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {'div', [{id, ?get(id)}], [
            %% A child at the ROOT level too, fed from a root binding, so one event
            %% exercises both the top-level child-view op and the in-item one.
            ?stateful(arizona_stream_item_child_badge, #{
                id => ~"badge-root",
                label => ?get(root_label)
            }),
            {ul, [], [
                ?each(
                    fun(#{id := Id, label := Label}, Key) ->
                        {li, [{az_key, Key}], [
                            ?stateful(arizona_stream_item_child_badge, #{
                                id => <<"badge-", (integer_to_binary(Id))/binary>>,
                                label => Label
                            })
                        ]}
                    end,
                    ?get(items)
                )
            ]}
        ]}
    ).

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"relabel", #{~"id" := Id, ~"label" := Label}, Bindings) ->
    Stream = arizona_stream:update(
        maps:get(items, Bindings), Id, #{id => Id, label => Label}
    ),
    {Bindings#{items => Stream, root_label => Label}, #{}, []}.
