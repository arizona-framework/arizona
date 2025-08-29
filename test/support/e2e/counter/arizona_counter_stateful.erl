-module(arizona_counter_stateful).
-behaviour(arizona_stateful).
-compile({parse_transform, arizona_parse_transform}).
-export([mount/1]).
-export([render/1]).
-export([handle_event/3]).

mount(Bindings) ->
    arizona_stateful:new(?MODULE, Bindings).

render(Bindings) ->
    arizona_template:from_string(~"""
    <div id="{arizona_template:get_binding(id, Bindings)}">
        <h1>Counter: <span data-testid="count">{arizona_template:get_binding(count, Bindings)}</span></h1>
        <button
            data-testid="increment"
            onclick="arizona.sendEventTo('{arizona_template:get_binding(id, Bindings)}', 'increment')"
        >
            +
        </button>
        <button
            data-testid="decrement"
            onclick="arizona.sendEventTo('{arizona_template:get_binding(id, Bindings)}', 'increment', \{incr: -1})"
        >
            -
        </button>
        <button
            data-testid="reset"
            onclick="arizona.sendEventTo('{arizona_template:get_binding(id, Bindings)}', 'reset')"
        >
            Reset
        </button>
    </div>
    """).

handle_event(~"increment", Payload, State) ->
    ok = broadcast_increment(Payload, State),
    Incr = maps:get(~"incr", Payload, 1),
    Count = arizona_stateful:get_binding(count, State),
    {noreply, arizona_stateful:put_binding(count, Count + Incr, State)};
handle_event(~"pubsub:increment", Payload, State) ->
    Incr = maps:get(~"incr", Payload, 1),
    Count = arizona_stateful:get_binding(count, State),
    {noreply, arizona_stateful:put_binding(count, Count + Incr, State)};
handle_event(~"reset", _Payload, State) ->
    {noreply, arizona_stateful:put_binding(count, 0, State)}.

broadcast_increment(Payload, State) ->
    case arizona_stateful:get_binding(is_realtime_enabled, State) of
        true ->
            arizona_pubsub:broadcast_from(self(), ~"increment", Payload);
        false ->
            ok
    end.
