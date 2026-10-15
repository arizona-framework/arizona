-module(arizona_stream_child_gate).
-include("arizona_stateful.hrl").
-export([mount/1]).
-export([render/1]).
-export([handle_event/3]).

%% A stream item whose `on` field, read with plain `maps:get/3`, decides whether the
%% item embeds a stateful child; the child's props read a view binding, so the
%% slot holding the decision has non-empty deps that do not name the field. A second
%% child depends on the item key alone, so a field change must leave it mounted.

-spec mount(az:bindings()) -> az:mount_ret().
mount(_Bindings) ->
    Stream = arizona_stream:new(fun(#{id := Id}) -> Id end, [#{id => 1, on => true}]),
    {#{id => ~"scg", items => Stream, start => 7}, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {'div', [{id, ?get(id)}], [
            {ul, [], [
                ?each(
                    fun(Item, Key) ->
                        {li, [{az_key, Key}], [
                            case maps:get(on, Item) of
                                true ->
                                    ?stateful(arizona_counter, #{
                                        id => <<"gate-", (integer_to_binary(Key))/binary>>,
                                        count => ?get(start)
                                    });
                                false ->
                                    ~"off"
                            end,
                            ?stateful(arizona_counter, #{
                                id => <<"keep-", (integer_to_binary(Key))/binary>>
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
handle_event(~"set_on", #{~"id" := Id, ~"on" := On}, Bindings) ->
    S = arizona_stream:update(maps:get(items, Bindings), Id, #{id => Id, on => On}),
    {Bindings#{items => S}, #{}, []}.
