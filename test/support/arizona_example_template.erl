-module(arizona_example_template).
-compile({parse_transform, arizona_transform}).
-behaviour(arizona_view).

-export([mount/2]).
-export([render/1]).
-export([handle_event/3]).

mount(Assigns, _Socket) ->
    View = arizona_view:new(?MODULE, Assigns),
    {ok, View}.

render(View) ->
    arizona:render_view_template(View, ~""""
    <html>
        <head></head>
        <body id="{arizona:get_assign(id, View)}">
            {arizona:render_view(arizona_example_counter, #{
                id => ~"counter",
                count => arizona:get_assign(count, View),
                btn_text => arizona:get_assign(btn_text, View, ~"Increment")
            })}
        </body>
    </html>
    """").

handle_event(_Event, _Payload, View) ->
    View.
