-module(arizona_example_stateless).

-export([button/1]).

button(Socket) ->
    arizona_html:render_stateless(~"""
    <button type="button">
      {arizona_socket:get_binding(text, Socket)}
    </button>
    """, Socket).
