-module(arizona_counter_live).
-behaviour(arizona_live).
-compile({parse_transform, arizona_parse_transform}).

-export([mount/2, render/1, handle_event/3]).

% Simple counter component for E2E testing
mount(_Params, Socket) ->
    Socket1 = arizona_socket:put_binding(count, 0, Socket),
    arizona_socket:set_layout({arizona_counter_layout, render, main_content}, Socket1).

render(Socket) ->
    arizona_html:render_live(~"""
    <div id="root">
        <h1>Counter: <span data-testid="count">{arizona_socket:get_binding(count, Socket)}</span></h1>
        <button data-testid="increment" onclick="arizona.sendEvent('increment')">+</button>
        <button data-testid="decrement" onclick="arizona.sendEvent('decrement')">-</button>
        <button data-testid="reset" onclick="arizona.sendEvent('reset')">Reset</button>
    </div>
    """, Socket).

handle_event(~"increment", _Payload, Socket) ->
    Count = arizona_socket:get_binding(count, Socket),
    {noreply, arizona_socket:put_binding(count, Count + 1, Socket)};
handle_event(~"decrement", _Payload, Socket) ->
    Count = arizona_socket:get_binding(count, Socket),
    {noreply, arizona_socket:put_binding(count, Count - 1, Socket)};
handle_event(~"reset", _Payload, Socket) ->
    {noreply, arizona_socket:put_binding(count, 0, Socket)};
handle_event(_Event, _Payload, Socket) ->
    {noreply, Socket}.
