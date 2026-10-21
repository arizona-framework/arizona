-module(arizona_stream_lifecycle_root).
-include("arizona_stateful.hrl").
-export([mount/1]).
-export([render/1]).
-export([handle_event/3]).

%% A stream whose items each embed `arizona_stream_lifecycle_child` while their
%% `on` field is true (the default, or the `on` mount binding for the initial
%% items); the item's `grand` field reaches the child as
%% a prop. Each event takes items, or an item's child, out of (or back into) the
%% visible window through a different stream API, so a suite can assert the item
%% children follow.

-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings) ->
    Keys = maps:get(keys, Bindings, [~"a", ~"b"]),
    Opts = maps:get(opts, Bindings, #{}),
    On = maps:get(on, Bindings, true),
    {
        #{
            id => ~"slr",
            notify => maps:get(notify, Bindings),
            items => arizona_stream:new(fun key/1, [#{id => K, on => On} || K <- Keys], Opts)
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
                            case arizona_template:get(on, Item, true) of
                                true ->
                                    ?stateful(arizona_stream_lifecycle_child, #{
                                        id => <<"c-", Key/binary>>,
                                        notify => ?get(notify),
                                        grand => arizona_template:get(grand, Item, false)
                                    });
                                false ->
                                    ~"off"
                            end
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
handle_event(~"insert_delete", #{~"id" := Id}, #{items := S} = Bindings) ->
    S1 = arizona_stream:delete(arizona_stream:insert(S, #{id => Id}), Id),
    {Bindings#{items => S1}, #{}, []};
handle_event(~"upsert_delete", #{~"id" := Id}, #{items := S} = Bindings) ->
    S1 = arizona_stream:delete(arizona_stream:update(S, Id, #{id => Id}), Id),
    {Bindings#{items => S1}, #{}, []};
handle_event(~"update_delete", #{~"id" := Id}, #{items := S} = Bindings) ->
    S1 = arizona_stream:delete(arizona_stream:update(S, Id, #{id => Id, grand => true}), Id),
    {Bindings#{items => S1}, #{}, []};
handle_event(~"reinsert", #{~"id" := Id}, #{items := S} = Bindings) ->
    S1 = arizona_stream:insert(arizona_stream:delete(S, Id), #{id => Id}),
    {Bindings#{items => S1}, #{}, []};
handle_event(~"replace", #{~"del" := Del, ~"ins" := Ins}, #{items := S0} = Bindings) ->
    S1 = lists:foldl(fun(Id, S) -> arizona_stream:delete(S, Id) end, S0, Del),
    S2 = lists:foldl(fun(Id, S) -> arizona_stream:insert(S, #{id => Id}) end, S1, Ins),
    {Bindings#{items => S2}, #{}, []};
handle_event(~"reset", #{~"ids" := Ids}, #{items := S} = Bindings) ->
    {Bindings#{items => arizona_stream:reset(S, [#{id => K} || K <- Ids])}, #{}, []};
handle_event(~"renew", #{~"ids" := Ids}, Bindings) ->
    {Bindings#{items => arizona_stream:new(fun key/1, [#{id => K} || K <- Ids])}, #{}, []};
handle_event(~"grand", #{~"id" := Id, ~"on" := On}, #{items := S} = Bindings) ->
    {Bindings#{items => arizona_stream:update(S, Id, #{id => Id, grand => On})}, #{}, []};
handle_event(~"switch_off", #{~"id" := Id}, #{items := S} = Bindings) ->
    {Bindings#{items => arizona_stream:update(S, Id, #{id => Id, on => false})}, #{}, []};
handle_event(~"sort_desc", _Payload, #{items := S} = Bindings) ->
    Desc = fun(#{id := A}, #{id := B}) -> A >= B end,
    {Bindings#{items => arizona_stream:sort(S, Desc)}, #{}, []}.

key(#{id := Id}) -> Id.
