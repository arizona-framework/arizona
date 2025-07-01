-module(test_stateful_module_with_unmount).
-behaviour(arizona_stateful).
-compile({parse_transform, arizona_parse_transform}).
-export([mount/1, render/1, unmount/1]).

mount(Socket) ->
    arizona_socket:put_bindings(
        #{
            mounted => true,
            unmounted => false
        },
        Socket
    ).

render(Socket) ->
    arizona_html:render_stateful(~"""
    <div class="unmountable-component">
        <h1>Component with Unmount</h1>
        <p>Mounted: {arizona_socket:get_binding(mounted, Socket)}</p>
        <p>Unmounted: {arizona_socket:get_binding(unmounted, Socket)}</p>
    </div>
    """, Socket).

unmount(Socket) ->
    %% Mock unmount behavior - set unmounted flag
    arizona_socket:put_binding(unmounted, true, Socket).
