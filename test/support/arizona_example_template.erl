-module(arizona_example_template).
-behaviour(arizona_view).

-export([mount/2]).
-export([render/2]).

mount(Assigns, _Socket) ->
    View = arizona_view:new(?MODULE, Assigns),
    {ok, View}.

render(View, Socket) ->
    arizona_render:view_template(View, Socket, ~""""
    <html>
        <head></head>
        <body id="{arizona_view:get_assign(id, View)}">
            {arizona_render:view(arizona_example_counter, #{
                id => ~"counter",
                count => arizona_view:get_assign(count, View)
            })}
        </body>
    </html>
    """").
