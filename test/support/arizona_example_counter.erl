-module(arizona_example_counter).
-compile({parse_transform, arizona_transform}).
-behaviour(arizona_view).

-export([mount/2]).
-export([render/1]).
-export([handle_event/3]).

mount(Assigns, _Socket) ->
    View = arizona_view:new(?MODULE, Assigns#{
        id => maps:get(id, Assigns, ~"counter")
    }),
    {ok, View}.

render(View) ->
    arizona:render_view_template(View, ~""""
    <div id="{arizona:get_assign(id, View)}">
        {integer_to_binary(arizona:get_assign(count, View))}
        {arizona:render_component(arizona_example_components, button, #{
            text => arizona:get_assign(btn_text, View, ~"Increment")
        })}
    </div>
    """").

handle_event(_Event, _Payload, View) ->
    View.
