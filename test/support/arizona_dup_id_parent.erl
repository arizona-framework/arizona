-module(arizona_dup_id_parent).
-include("arizona_stateful.hrl").
-export([mount/1]).
-export([render/1]).
-export([handle_event/3]).

%% Root view whose children can be made to share an id with another view in one
%% render: a sibling (`dup`), the root itself (`root_clash`), a stream item's child
%% (`extra => ~"t-a"`), or the grandchild a child renders on its own event
%% (`extra => ~"lc-g"`, then `grand_on` to `lc`). A live process keys its views
%% by id, so a second view under one id silently replaced the first.

-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings) ->
    {
        #{
            id => ~"dup_parent",
            notify => maps:get(notify, Bindings),
            dup => maps:get(dup, Bindings, false),
            root_clash => maps:get(root_clash, Bindings, false),
            extra => maps:get(extra, Bindings, undefined),
            items => arizona_stream:new(fun key/1, [#{id => ~"a"}])
        },
        #{}
    }.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {main, [{id, ?get(id)}], [
            ?stateful(arizona_toast, #{id => ~"toast"}),
            case ?get(dup) of
                true -> ?stateful(arizona_toast, #{id => ~"toast"});
                false -> ~""
            end,
            case ?get(root_clash) of
                true -> ?stateful(arizona_toast, #{id => ~"dup_parent"});
                false -> ~""
            end,
            case ?get(extra) of
                undefined -> ~"";
                Extra -> ?stateful(arizona_toast, #{id => Extra})
            end,
            ?stateful(arizona_stream_lifecycle_child, #{id => ~"lc", notify => ?get(notify)}),
            {ul, [], [
                ?each(
                    fun(_Item, Key) ->
                        {li, [{az_key, Key}], [
                            ?stateful(arizona_toast, #{id => <<"t-", Key/binary>>})
                        ]}
                    end,
                    ?get(items)
                )
            ]}
        ]}
    ).

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"dup", _Payload, Bindings) ->
    {Bindings#{dup => true}, #{}, []};
handle_event(~"root_clash", _Payload, Bindings) ->
    {Bindings#{root_clash => true}, #{}, []};
handle_event(~"insert_extra", #{~"key" := Key, ~"extra" := Extra}, #{items := S} = Bindings) ->
    {Bindings#{items => arizona_stream:insert(S, #{id => Key}), extra => Extra}, #{}, []}.

key(#{id := Id}) -> Id.
