-module(arizona_example_counter).
-compile({parse_transform, arizona_transform}).
-behaviour(arizona_view).

-export([mount/2]).
-export([render/1]).
-export([handle_event/3]).

mount(Bindings, _Socket) ->
    View = arizona:new_view(?MODULE, Bindings#{
        id => maps:get(id, Bindings, ~"counter")
    }),
    {ok, View}.

render(View) ->
    arizona:render_view_template(View, ~""""
    <div id="{arizona:get_binding(id, View)}">
        {integer_to_binary(arizona:get_binding(count, View))}
        {arizona:render_component(arizona_example_components, button, #{
            text => arizona:get_binding(btn_text, View, ~"Increment")
        })}
    </div>
    """").

handle_event(_Event, _Payload, View) ->
    View.
