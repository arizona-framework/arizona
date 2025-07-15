-module(arizona_stateful_module_with_mount).
-behaviour(arizona_stateful).
-compile({parse_transform, arizona_parse_transform}).
-arizona_parse_transform([render/1]).
-export([mount/1, render/1]).

mount(Socket) ->
    %% Mock mount behavior - add status binding to current stateful state
    arizona_socket:put_binding(status, ~"mounted", Socket).

render(Socket) ->
    arizona_html:render_stateful(~"""
    <div class="mounted-component">
        <h1>Mounted Component</h1>
        <p>Status: {arizona_socket:get_binding(status, Socket)}</p>
    </div>
    """, Socket).
