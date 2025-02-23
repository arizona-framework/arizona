-module(arizona_example_components).
-compile({parse_transform, arizona_transform}).
-export([button/1]).
-export([list/1]).
-export([table/1]).

button(View) ->
    arizona_renderer:component_template(View, ~"""
    <button>
        {arizona_view:get_assign(text, View)}
    </button>
    """).

list(View) ->
    arizona_renderer:component_template(View, ~""""
    <ul>
        {arizona_renderer:list(fun(Item) ->
            arizona_renderer:nested_template(~"""
            <li>{Item}</li>
            """)
         end, arizona_view:get_assign(list, View))}
    </ul>
    """").

table(View) ->
    arizona_renderer:component_template(View, ~"""""
    <table>
        <tr>
            {arizona_renderer:list(fun(Col) ->
                arizona_renderer:nested_template(~"""
                <th>{maps:get(label, Col)}</th>
                """)
             end, arizona_view:get_assign(columns, View))}
        </tr>
        {arizona_renderer:list(fun(Row) ->
            arizona_renderer:nested_template(~""""
            <tr>
                {arizona_renderer:list(fun(Col) ->
                    arizona_renderer:nested_template(~"""
                    <td>
                        {erlang:apply(maps:get(callback, Col), [Row])}
                    </td>
                    """)
                 end, arizona_view:get_assign(columns, View))}
            </tr>
            """")
         end, arizona_view:get_assign(rows, View))}
    </table>
    """"").
