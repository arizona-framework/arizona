-module(arizona_example_components).
-compile({parse_transform, arizona_transform}).
-export([button/1]).
-export([list/1]).

button(View) ->
    arizona_render:component_template(View, ~"""
    <button>
        {arizona_view:get_assign(text, View)}
    </button>
    """).

list(View) ->
    arizona_render:component_template(View, ~""""
    <ul>
        {arizona_render:list(fun(Item) ->
            arizona_render:nested_template(#{'View' => View, 'Item' => Item}, ~"""
            <li>
                {integer_to_binary(Item)}
                <br/>
                {integer_to_binary(Item)}
            </li>
            """)
         end, arizona_view:get_assign(list, View))}
    </ul>
    """").
