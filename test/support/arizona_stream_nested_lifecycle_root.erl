-module(arizona_stream_nested_lifecycle_root).
-include("arizona_stateful.hrl").
-export([mount/1]).
-export([render/1]).
-export([handle_event/3]).

%% A stream of groups, each item holding an inner stream whose items embed
%% `arizona_stream_lifecycle_child` (id `c-<Group>-<Key>`). Each event replaces one
%% group's inner stream through a different stream API, so a suite can assert the
%% children of a stream nested in a stream item follow their items.

-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings) ->
    Keys = maps:get(keys, Bindings, [~"a", ~"b"]),
    Opts = maps:get(opts, Bindings, #{}),
    Inner = arizona_stream:new(fun key/1, [#{id => K} || K <- Keys], Opts),
    {
        #{
            id => ~"snl",
            notify => maps:get(notify, Bindings),
            groups => arizona_stream:new(fun key/1, [#{id => ~"g", inner => Inner}])
        },
        #{}
    }.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {'div', [{id, ?get(id)}], [
            {ul, [], [
                ?each(
                    fun(Group, GKey) ->
                        {li, [{az_key, GKey}], [
                            {ul, [], [
                                ?each(
                                    fun(_Item, Key) ->
                                        {li, [{az_key, Key}], [
                                            ?stateful(arizona_stream_lifecycle_child, #{
                                                id => <<"c-", GKey/binary, "-", Key/binary>>,
                                                notify => ?get(notify)
                                            })
                                        ]}
                                    end,
                                    arizona_template:get(inner, Group)
                                )
                            ]}
                        ]}
                    end,
                    ?get(groups)
                )
            ]}
        ]}
    ).

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"delete", #{~"id" := Id}, Bindings) ->
    {update_inner(fun(S) -> arizona_stream:delete(S, Id) end, Bindings), #{}, []};
handle_event(~"insert", #{~"id" := Id}, Bindings) ->
    {update_inner(fun(S) -> arizona_stream:insert(S, #{id => Id}) end, Bindings), #{}, []};
handle_event(~"insert_at", #{~"id" := Id, ~"pos" := Pos}, Bindings) ->
    Insert = fun(S) -> arizona_stream:insert(S, #{id => Id}, Pos) end,
    {update_inner(Insert, Bindings), #{}, []};
handle_event(~"insert_two", #{~"ids" := [First, Second]}, Bindings) ->
    Insert = fun(Id) -> fun(S) -> arizona_stream:insert(S, #{id => Id}) end end,
    {update_inner(Insert(Second), update_inner(Insert(First), Bindings)), #{}, []};
handle_event(~"reset", #{~"ids" := Ids}, Bindings) ->
    Reset = fun(S) -> arizona_stream:reset(S, [#{id => K} || K <- Ids]) end,
    {update_inner(Reset, Bindings), #{}, []}.

update_inner(Fun, #{groups := Groups} = Bindings) ->
    #{inner := Inner} = Group = arizona_stream:get(Groups, ~"g"),
    Bindings#{groups => arizona_stream:update(Groups, ~"g", Group#{inner => Fun(Inner)})}.

key(#{id := Id}) -> Id.
