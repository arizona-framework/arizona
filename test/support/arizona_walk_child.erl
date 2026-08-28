-module(arizona_walk_child).
-moduledoc """
Walk fixture reached through the transform-recorded dependency edge.
""".
-include("arizona_stateless.hrl").

-export([render/1]).

-spec render(az:bindings()) -> az:template().
render(_Bindings) ->
    ?html({p, [{az_contextmenu, arizona_js:push_event(~"c")}], [~"child"]}).
