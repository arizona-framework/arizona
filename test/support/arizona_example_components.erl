-module(arizona_example_components).
-export([button/2]).

button(View, Socket) ->
    arizona_render:component_template(View, Socket, ~"""
    <button>
        {arizona_view:get_assign(text, View)}
    </button>
    """).
