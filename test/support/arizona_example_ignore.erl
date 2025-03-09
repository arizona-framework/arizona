-module(arizona_example_ignore).
-behaviour(arizona_view).

-export([mount/2]).
-export([render/1]).
-export([handle_event/4]).

mount(_Bindings, _Socket) ->
    ignore.

render(View) ->
    arizona:render_view_template(View, ~""""
    ignored
    """").

handle_event(_Event, _Payload, _From, View) ->
    {noreply, View}.
