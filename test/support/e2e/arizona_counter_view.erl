-module(arizona_counter_view).
-behaviour(arizona_view).
-compile({parse_transform, arizona_parse_transform}).

-export([mount/1]).
-export([render/1]).
-export([handle_event/3]).

mount(_Req) ->
    Layout = {arizona_counter_layout, render, main_content, #{}},
    arizona_view:new(
        ?MODULE,
        #{
            id => ~"counter",
            count => 0
        },
        Layout
    ).

render(Bindings) ->
    arizona_template:from_string(~"""
    <div id="{arizona_template:get_binding(id, Bindings)}">
        <h1>Counter: <span data-testid="count">
            {arizona_template:get_binding(count, Bindings)}
        </span></h1>
        <button data-testid="increment" onclick="arizona.sendEvent('increment')">+</button>
        <button data-testid="decrement" onclick="arizona.sendEvent('decrement')">-</button>
        <button data-testid="reset" onclick="arizona.sendEvent('reset')">Reset</button>
    </div>
    """).

handle_event(~"increment", _Payload, View) ->
    State = arizona_view:get_state(View),
    Count = arizona_stateful:get_binding(count, State),
    NewState = arizona_stateful:put_binding(count, Count + 1, State),
    {noreply, arizona_view:update_state(NewState, View)};
handle_event(~"decrement", _Payload, View) ->
    State = arizona_view:get_state(View),
    Count = arizona_stateful:get_binding(count, State),
    NewState = arizona_stateful:put_binding(count, Count - 1, State),
    {noreply, arizona_view:update_state(NewState, View)};
handle_event(~"reset", _Payload, View) ->
    State = arizona_view:get_state(View),
    NewState = arizona_stateful:put_binding(count, 0, State),
    {noreply, arizona_view:update_state(NewState, View)}.
