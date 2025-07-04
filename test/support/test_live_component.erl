-module(test_live_component).
-behaviour(arizona_live).

%% arizona_live callbacks
-export([mount/2, render/1, handle_event/3]).

%% Test component for arizona_live testing

mount(_Req, Socket) ->
    % Set up layout
    SocketWithLayout = arizona_socket:set_layout({test_layout, render, main_content}, Socket),

    % For arizona_live, we need to create a root stateful state first
    RootState = arizona_stateful:new(root, test_live_component, #{
        name => ~"World",
        count => 0
    }),
    SocketWithState = arizona_socket:put_stateful_state(RootState, SocketWithLayout),

    {SocketWithState, #{}}.

render(Socket) ->
    arizona_html:render_live(~"""
    <div class="live-component">
        <h1>Hello, {arizona_socket:get_binding(name, Socket)}!</h1>
        <p>You clicked {arizona_socket:get_binding(count, Socket)} times</p>
        <button>+</button>
        <button>-</button>
        <button>Reset</button>
    </div>
    """, Socket).

handle_event(~"increment", _Params, Socket) ->
    CurrentCount = arizona_socket:get_binding(count, Socket),
    UpdatedSocket = arizona_socket:put_binding(count, CurrentCount + 1, Socket),
    {noreply, UpdatedSocket};
handle_event(~"decrement", _Params, Socket) ->
    CurrentCount = arizona_socket:get_binding(count, Socket),
    NewCount = max(0, CurrentCount - 1),
    UpdatedSocket = arizona_socket:put_binding(count, NewCount, Socket),
    {noreply, UpdatedSocket};
handle_event(~"reset", _Params, Socket) ->
    UpdatedSocket = arizona_socket:put_binding(count, 0, Socket),
    {noreply, UpdatedSocket};
handle_event(_Event, _Params, Socket) ->
    {noreply, Socket}.
