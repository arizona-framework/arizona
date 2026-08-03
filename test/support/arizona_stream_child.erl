-module(arizona_stream_child).
-include("arizona_stateful.hrl").

-export([mount/1]).
-export([handle_update/3]).
-export([render/1]).

%% Embeddable stateful child whose stream is fed by PARENT PROPS: every parent
%% update appends the incoming `tick` as a stream item. Used to pin that the
%% eval path (arizona_eval:eval_stateful/3) clears the child's pending queue --
%% otherwise it accumulates one entry per root update for the process lifetime.
-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings) ->
    {
        #{
            id => maps:get(id, Bindings, ~"stream_child"),
            items => arizona_stream:new(
                fun(#{id := Id}) -> Id end, [], #{limit => 3, on_limit => drop}
            )
        },
        #{}
    }.

-spec handle_update(az:bindings(), az:bindings(), az:effects()) -> az:handle_update_ret().
handle_update(Props, Bindings, Effects) ->
    Tick = maps:get(tick, Props),
    Items = maps:get(items, Bindings),
    Item = #{id => Tick, text => integer_to_binary(Tick)},
    {Bindings#{items => arizona_stream:insert(Items, Item)}, #{}, Effects}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {ul, [{id, ?get(id)}], [
            ?each(
                fun(#{text := Text}, Key) ->
                    {li, [{az_key, Key}], [Text]}
                end,
                ?get(items)
            )
        ]}
    ).
