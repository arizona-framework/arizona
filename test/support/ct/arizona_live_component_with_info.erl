-module(arizona_live_component_with_info).
-behaviour(arizona_live).
%-compile({parse_transform, arizona_parse_transformform}).
-arizona_parse_transformform([render/1]).

%% arizona_live callbacks
-export([mount/2, render/1, handle_event/3, handle_info/2]).

%% Test component with handle_info callback for arizona_live testing

mount(_Req, Socket) ->
    % Set up layout
    SocketWithLayout = arizona_socket:set_layout(
        {arizona_test_layout, render, main_content}, Socket
    ),

    % For arizona_live, we need to create a root stateful state first
    RootState = arizona_stateful:new(root, arizona_live_component_with_info, #{
        name => ~"World",
        count => 0,
        messages => []
    }),
    SocketWithState = arizona_socket:put_stateful_state(RootState, SocketWithLayout),

    SocketWithState.

render(Socket) ->
    arizona_html:render_live(~"""
    <div class="live-component-with-info">
        <h1>Hello, {arizona_socket:get_binding(name, Socket)}!</h1>
        <p>You clicked {arizona_socket:get_binding(count, Socket)} times</p>
        <button>+</button>
        <button>-</button>
        <button>Reset</button>
        <button>Reply Test</button>
        <div class="messages">
            <h3>Messages:</h3>
            <ul>
                <li>Message count: {length(arizona_socket:get_binding(messages, Socket))}</li>
            </ul>
        </div>
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
handle_event(~"reply_test", _Params, Socket) ->
    {reply, ~"test_reply", Socket};
handle_event(_Event, _Params, Socket) ->
    {noreply, Socket}.

handle_info({add_message, Message}, Socket) ->
    CurrentMessages = arizona_socket:get_binding(messages, Socket),
    UpdatedMessages = [Message | CurrentMessages],
    UpdatedSocket = arizona_socket:put_binding(messages, UpdatedMessages, Socket),
    {noreply, UpdatedSocket};
handle_info(_Info, Socket) ->
    {noreply, Socket}.
