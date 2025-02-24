-module(arizona_example_template).
-compile({parse_transform, arizona_transform}).
-behaviour(arizona_view).

-export([mount/2]).
-export([render/1]).
-export([handle_event/3]).

mount(Bindings, _Socket) ->
    View = arizona:new_view(?MODULE, Bindings),
    {ok, View}.

render(View) ->
    arizona:render_view_template(View, ~""""
    <html>
        <head></head>
        <body id="{arizona:get_binding(id, View)}">
            {arizona:render_view(arizona_example_counter, #{
                id => ~"counter",
                count => arizona:get_binding(count, View),
                btn_text => arizona:get_binding(btn_text, View, ~"Increment")
            })}
        </body>
    </html>
    """").

handle_event(_Event, _Payload, View) ->
    View.
