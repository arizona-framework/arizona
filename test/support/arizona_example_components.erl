-module(arizona_example_components).
-compile({parse_transform, arizona_transform}).
-export([button/1]).

button(View) ->
    arizona_render:component_template(View, ~"""
    <button>
        {arizona_view:get_assign(text, View)}
    </button>
    """).
