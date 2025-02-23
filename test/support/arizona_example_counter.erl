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
    arizona_renderer:view_template(View, ~""""
    <div id="{arizona_view:get_assign(id, View)}">
        {integer_to_binary(arizona_view:get_assign(count, View))}
        {arizona_renderer:component(arizona_example_components, button, #{
            text => arizona_view:get_assign(btn_text, View, ~"Increment")
        })}
    </div>
    """").

handle_event(_Event, _Payload, View) ->
    View.
