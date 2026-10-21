-module(arizona_command_attr).
-moduledoc """
Fixture: an event attribute whose command is a binding, so a test renders any
command a template could hold -- built at render time, not folded at compile time.
""".
-include("arizona_stateless.hrl").
-export([render/1]).

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html({button, [{az_click, ?get(cmd)}], [~"b"]}).
