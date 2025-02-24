-module(arizona_example_components).
-compile({parse_transform, arizona_transform}).
-export([button/1]).
-export([list/1]).
-export([table/1]).

button(View) ->
    arizona:render_component_template(View, ~"""
    <button>
        {arizona:get_binding(text, View)}
    </button>
    """).

list(View) ->
    arizona:render_component_template(View, ~""""
    <ul>
        {arizona:render_list(fun(Item) ->
            arizona:render_nested_template(~"""
            <li>{Item}</li>
            """)
         end, arizona:get_binding(list, View))}
    </ul>
    """").

table(View) ->
    arizona:render_component_template(View, ~"""""
    <table>
        <tr>
            {arizona:render_list(fun(Col) ->
                arizona:render_nested_template(~"""
                <th>{maps:get(label, Col)}</th>
                """)
             end, arizona:get_binding(columns, View))}
        </tr>
        {arizona:render_list(fun(Row) ->
            arizona:render_nested_template(~""""
            <tr>
                {arizona:render_list(fun(Col) ->
                    arizona:render_nested_template(~"""
                    <td>
                        {erlang:apply(maps:get(callback, Col), [Row])}
                    </td>
                    """)
                 end, arizona:get_binding(columns, View))}
            </tr>
            """")
         end, arizona:get_binding(rows, View))}
    </table>
    """"").
