-module(arizona_counter_view).
-behaviour(arizona_view).
-compile({parse_transform, arizona_parse_transform}).

-export([mount/1]).
-export([render/1]).
-export([handle_event/3]).

mount(_Req) ->
    arizona_stateful:new(?MODULE, #{
        id => ~"counter",
        count => 0,
        layout => {arizona_counter_layout, render, main_content}
    }).

render(Bindings) ->
    arizona_template:from_string(~"""
    <div id="{arizona_binder:get(id, Bindings)}">
        <h1>Counter: <span data-testid="count">
            {arizona_template:get_binding(count, Bindings)}
        </span></h1>
        <button data-testid="increment" onclick="arizona.sendEvent('increment')">+</button>
        <button data-testid="decrement" onclick="arizona.sendEvent('decrement')">-</button>
        <button data-testid="reset" onclick="arizona.sendEvent('reset')">Reset</button>
    </div>
    """).

handle_event(~"increment", _Payload, State) ->
    Count = arizona_stateful:get_binding(count, State),
    {noreply, arizona_stateful:put_binding(count, Count + 1, State)};
handle_event(~"decrement", _Payload, State) ->
    Count = arizona_stateful:get_binding(count, State),
    {noreply, arizona_stateful:put_binding(count, Count - 1, State)};
handle_event(~"reset", _Payload, State) ->
    {noreply, arizona_stateful:put_binding(count, 0, State)}.
