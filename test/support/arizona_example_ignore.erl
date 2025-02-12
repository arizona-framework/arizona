-module(arizona_example_ignore).
-behaviour(arizona_view).

-export([mount/2]).
-export([render/2]).

mount(_Assigns, _Socket) ->
    ignore.

render(View, Socket) ->
    arizona_render:template(View, Socket, ~""""
    ignored
    """").
