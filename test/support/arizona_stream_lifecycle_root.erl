-module(arizona_stream_lifecycle_root).
-include("arizona_stateful.hrl").
-export([mount/1]).
-export([render/1]).
-export([handle_event/3]).

%% A stream whose items each embed `arizona_stream_lifecycle_child`. Each event
%% takes items out of (or back into) the visible window through a different
%% stream API, so a suite can assert the item children follow.

-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings) ->
    Keys = maps:get(keys, Bindings, [~"a", ~"b"]),
    Opts = maps:get(opts, Bindings, #{}),
    {
        #{
            id => ~"slr",
            notify => maps:get(notify, Bindings),
            items => arizona_stream:new(fun key/1, [#{id => K} || K <- Keys], Opts)
        },
        #{}
    }.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {'div', [{id, ?get(id)}], [
            {ul, [], [
                ?each(
                    fun(_Item, Key) ->
                        {li, [{az_key, Key}], [
                            ?stateful(arizona_stream_lifecycle_child, #{
                                id => <<"c-", Key/binary>>, notify => ?get(notify)
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
handle_event(~"delete", #{~"id" := Id}, #{items := S} = Bindings) ->
    {Bindings#{items => arizona_stream:delete(S, Id)}, #{}, []};
handle_event(~"insert", #{~"id" := Id}, #{items := S} = Bindings) ->
    {Bindings#{items => arizona_stream:insert(S, #{id => Id})}, #{}, []};
handle_event(~"insert_at", #{~"id" := Id, ~"pos" := Pos}, #{items := S} = Bindings) ->
    {Bindings#{items => arizona_stream:insert(S, #{id => Id}, Pos)}, #{}, []};
handle_event(~"reinsert", #{~"id" := Id}, #{items := S} = Bindings) ->
    S1 = arizona_stream:insert(arizona_stream:delete(S, Id), #{id => Id}),
    {Bindings#{items => S1}, #{}, []};
handle_event(~"reset", #{~"ids" := Ids}, #{items := S} = Bindings) ->
    {Bindings#{items => arizona_stream:reset(S, [#{id => K} || K <- Ids])}, #{}, []};
handle_event(~"renew", #{~"ids" := Ids}, Bindings) ->
    {Bindings#{items => arizona_stream:new(fun key/1, [#{id => K} || K <- Ids])}, #{}, []};
handle_event(~"sort_desc", _Payload, #{items := S} = Bindings) ->
    Desc = fun(#{id := A}, #{id := B}) -> A >= B end,
    {Bindings#{items => arizona_stream:sort(S, Desc)}, #{}, []}.

key(#{id := Id}) -> Id.
