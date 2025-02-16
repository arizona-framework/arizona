-module(arizona_example_counter).
-behaviour(arizona_view).

-export([mount/2]).
-export([render/1]).

mount(Assigns, _Socket) ->
    View = arizona_view:new(?MODULE, Assigns#{
        id => maps:get(id, Assigns, ~"counter")
    }),
    {ok, View}.

render(View) ->
    arizona_render:view_template(View, ~""""
    <div id="{arizona_view:get_assign(id, View)}">
        {integer_to_binary(arizona_view:get_assign(count, View))}
        {arizona_render:component(arizona_example_components, button, #{
            text => ~"Increment"
        })}
    </div>
    """").
