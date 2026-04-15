-module(arizona_stream_with_child).
-include("arizona_stateful.hrl").
-export([mount/1, render/1, handle_event/3]).

%% A handler with a stateful child (arizona_counter) inside stream items.
%% Used to test that child views survive dep-skipping of the stream.

-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings0) ->
    Items = [#{id => 1, label => ~"Item 1"}],
    Stream = arizona_stream:new(fun(#{id := Id}) -> Id end, Items),
    Bindings = maps:merge(
        #{id => ~"swc", items => Stream, title => ~"Hello"},
        Bindings0
    ),
    {Bindings, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {'div', [{id, ?get(id)}], [
            {h1, [], [?get(title)]},
            {ul, [], [
                ?each(
                    fun(#{id := Id, label := Label}, Key) ->
                        IdBin = integer_to_binary(Id),
                        {li, [{az_key, Key}], [
                            {span, [], [Label]},
                            ?stateful(arizona_counter, #{
                                id => <<"counter-", IdBin/binary>>,
                                count => 0
                            }),
                            ?stateful(arizona_counter, #{
                                id => <<"extra-", IdBin/binary>>,
                                count => 0
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
handle_event(~"set_title", #{~"title" := T}, Bindings) ->
    {Bindings#{title => T}, #{}, []};
handle_event(~"add_item", #{~"id" := Id, ~"label" := Label}, Bindings) ->
    S = arizona_stream:insert(maps:get(items, Bindings), #{id => Id, label => Label}),
    {Bindings#{items => S}, #{}, []};
handle_event(~"delete_item", #{~"id" := Id}, Bindings) ->
    S = arizona_stream:delete(maps:get(items, Bindings), Id),
    {Bindings#{items => S}, #{}, []};
handle_event(~"update_item", #{~"id" := Id, ~"label" := Label}, Bindings) ->
    S = arizona_stream:update(maps:get(items, Bindings), Id, #{id => Id, label => Label}),
    {Bindings#{items => S}, #{}, []}.
