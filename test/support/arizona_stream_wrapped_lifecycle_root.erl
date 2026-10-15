-module(arizona_stream_wrapped_lifecycle_root).
-include("arizona_stateful.hrl").
-export([mount/1]).
-export([render/1]).
-export([handle_event/3]).
-export([wrapper/1]).

%% A stream whose items hold `arizona_stream_lifecycle_child` children that are
%% not a direct dynamic of the item: one inside an element a conditional returns
%% (`cw-`), one inside a `?stateless` (`cs-`), one in a content-slot list (`cl-`).
%% Each is reached through a nested snapshot, so the item's own dynamics name no
%% view id for it. The item's `grand` field reaches the `cw-` child as a prop, so
%% an item update can make that child render a grandchild or drop it.

-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings) ->
    {
        #{
            id => ~"swl",
            notify => maps:get(notify, Bindings),
            items => arizona_stream:new(fun key/1, [#{id => K} || K <- [~"a", ~"b"]])
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
                                    {'div', [], [
                                        ?stateful(arizona_stream_lifecycle_child, #{
                                            id => <<"cw-", Key/binary>>,
                                            notify => ?get(notify),
                                            grand => arizona_template:get(grand, Item, false)
                                        })
                                    ]};
                                false ->
                                    ~"off"
                            end,
                            ?stateless(fun wrapper/1, #{
                                id => <<"cs-", Key/binary>>, notify => ?get(notify)
                            }),
                            [
                                ?stateful(arizona_stream_lifecycle_child, #{
                                    id => <<"cl-", Key/binary>>, notify => ?get(notify)
                                }),
                                ~"tail"
                            ]
                        ]}
                    end,
                    ?get(items)
                )
            ]}
        ]}
    ).

-spec wrapper(az:bindings()) -> az:template().
wrapper(Bindings) ->
    ?html(
        {section, [], [
            ?stateful(arizona_stream_lifecycle_child, #{id => ?get(id), notify => ?get(notify)})
        ]}
    ).

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"delete", #{~"id" := Id}, #{items := S} = Bindings) ->
    {Bindings#{items => arizona_stream:delete(S, Id)}, #{}, []};
handle_event(~"grand", #{~"id" := Id, ~"on" := On}, #{items := S} = Bindings) ->
    {Bindings#{items => arizona_stream:update(S, Id, #{id => Id, grand => On})}, #{}, []};
handle_event(~"switch_off", #{~"id" := Id}, #{items := S} = Bindings) ->
    {Bindings#{items => arizona_stream:update(S, Id, #{id => Id, on => false})}, #{}, []}.

key(#{id := Id}) -> Id.
