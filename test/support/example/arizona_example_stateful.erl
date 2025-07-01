-module(arizona_example_stateful).

-export([mount/1]).
-export([render/1]).

mount(Socket) ->
    Socket.

render(Socket) ->
    arizona_html:render_stateful(~"""
    <div id={arizona_socket:get_binding(id, Socket)}>
        {arizona_component:call_stateless(arizona_example_stateless, button, #{
            text => ~"foo"
        }, Socket)}
    </div>
    """, Socket).
