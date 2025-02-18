-module(arizona_example_template).
-compile({parse_transform, arizona_transform}).
-behaviour(arizona_view).

-export([mount/2]).
-export([render/1]).

mount(Assigns, _Socket) ->
    View = arizona_view:new(?MODULE, Assigns),
    {ok, View}.

render(View) ->
    arizona_render:view_template(View, ~""""
    <html>
        <head></head>
        <body id="{arizona_view:get_assign(id, View)}">
            {arizona_render:view(arizona_example_counter, #{
                id => ~"counter",
                count => arizona_view:get_assign(count, View),
                btn_text => arizona_view:get_assign(btn_text, View, ~"Increment")
            })}
        </body>
    </html>
    """").
