-module(arizona_example_root).

-export([mount/1]).
-export([render/1]).

mount(Socket) ->
    Socket.

render(Socket) ->
    arizona_html:render_stateful(~"""
    {arizona_component:call_stateful(arizona_example_stateful, #{
        id => ~"foo"
    }, Socket)}
    """, Socket).
