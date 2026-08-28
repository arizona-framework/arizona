-module(arizona_walk_extra).
-moduledoc """
Walk fixture reached ONLY through the root's manual `-arizona_az_deps`
declaration -- nothing renders it literally.
""".
-include("arizona_stateless.hrl").

-export([render/1]).

-spec render(az:bindings()) -> az:template().
render(_Bindings) ->
    ?html({p, [{az_cut, arizona_js:push_event(~"x")}], [~"extra"]}).
