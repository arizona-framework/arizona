-module(arizona_example_ignore).
-behaviour(arizona_view).

-export([mount/2]).
-export([render/1]).

mount(_Assigns, _Socket) ->
    ignore.

render(View) ->
    arizona_render:view_template(View, ~""""
    ignored
    """").
