-module(arizona_stream_once_root).
-include("arizona_stateful.hrl").
-export([mount/1]).
-export([render/1]).
-export([handle_event/3]).

%% A stream whose items each embed `arizona_stream_once_child` with the item's `v`
%% as a prop. Each event touches one key more than once in a single event, so a
%% suite can assert the diff renders that item -- and so runs its child's
%% lifecycle -- once, from the final item.

-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings) ->
    Opts = maps:get(opts, Bindings, #{}),
    {
        #{
            id => ~"sor",
            notify => maps:get(notify, Bindings),
            items => arizona_stream:new(
                fun key/1, [#{id => K, v => 0} || K <- maps:get(keys, Bindings, [~"a", ~"b"])], Opts
            )
        },
        #{}
    }.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {'div', [{id, ?get(id)}], [
            {ul, [], [
                ?each(
                    fun(Item, Key) ->
                        {li, [{az_key, Key}], [
                            arizona_template:get(label, Item, ~""),
                            ?stateful(arizona_stream_once_child, #{
                                id => <<"c-", Key/binary>>,
                                notify => ?get(notify),
                                v => arizona_template:get(v, Item)
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
handle_event(~"insert_update", #{~"id" := Id}, #{items := S} = Bindings) ->
    S1 = arizona_stream:update(arizona_stream:insert(S, #{id => Id, v => 1}), Id, #{
        id => Id, v => 2
    }),
    {Bindings#{items => S1}, #{}, []};
handle_event(~"update_twice", #{~"id" := Id}, #{items := S} = Bindings) ->
    S1 = arizona_stream:update(arizona_stream:update(S, Id, #{id => Id, v => 1}), Id, #{
        id => Id, v => 2
    }),
    {Bindings#{items => S1}, #{}, []};
handle_event(~"insert_delete_reinsert", #{~"id" := Id}, #{items := S} = Bindings) ->
    S1 = arizona_stream:insert(
        arizona_stream:delete(arizona_stream:insert(S, #{id => Id, v => 1}), Id), #{
            id => Id, v => 2
        }
    ),
    {Bindings#{items => S1}, #{}, []};
handle_event(~"insert_delete_upsert", #{~"id" := Id}, #{items := S} = Bindings) ->
    S1 = arizona_stream:update(
        arizona_stream:delete(arizona_stream:insert(S, #{id => Id, v => 1}), Id), Id, #{
            id => Id, v => 2
        }
    ),
    {Bindings#{items => S1}, #{}, []};
handle_event(~"update_delete_reinsert", #{~"id" := Id}, #{items := S} = Bindings) ->
    S1 = arizona_stream:insert(
        arizona_stream:delete(arizona_stream:update(S, Id, #{id => Id, v => 1}), Id), #{
            id => Id, v => 2
        }
    ),
    {Bindings#{items => S1}, #{}, []};
handle_event(~"update_two_fields", #{~"id" := Id}, #{items := S} = Bindings) ->
    S1 = arizona_stream:update(
        arizona_stream:update(S, Id, #{id => Id, v => 1}), Id, #{id => Id, v => 1, label => ~"x"}
    ),
    {Bindings#{items => S1}, #{}, []};
handle_event(~"insert_move_update", #{~"id" := Id}, #{items := S} = Bindings) ->
    S1 = arizona_stream:update(
        arizona_stream:move(arizona_stream:insert(S, #{id => Id, v => 1}), Id, 0), Id, #{
            id => Id, v => 2
        }
    ),
    {Bindings#{items => S1}, #{}, []}.

key(#{id := Id}) -> Id.
