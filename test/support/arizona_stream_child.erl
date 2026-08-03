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
            %% A `seed` pre-populates the stream, so the queue is already
            %% non-empty at mount -- the state that exposes a snapshot built
            %% from a different stream value than the bindings stored beside it.
            items => arizona_stream:new(
                fun(#{id := Id}) -> Id end,
                maps:get(seed, Bindings, []),
                #{limit => 3, on_limit => drop}
            )
        },
        #{}
    }.

%% Mutates the stream only on ODD ticks. A child stream that does NOT change on
%% every parent update is the shape that exposes a disagreement between the
%% stored bindings and the snapshot built from them: the even ticks must emit
%% nothing at all.
-spec handle_update(az:bindings(), az:bindings(), az:effects()) -> az:handle_update_ret().
handle_update(Props, Bindings, Effects) ->
    Tick = maps:get(tick, Props),
    Items = maps:get(items, Bindings),
    Items1 =
        case Tick rem 2 of
            1 -> arizona_stream:insert(Items, #{id => Tick, text => integer_to_binary(Tick)});
            0 -> Items
        end,
    {Bindings#{items => Items1}, #{}, Effects}.

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
