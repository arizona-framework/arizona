-module(arizona_opaque_layout).
-moduledoc """
A layout whose command reaches its attribute OPAQUELY (through a binding, no
builder call in sight) -- the one shape neither proof can deliver: not
compile-proven, and a layout renders once at SSR where render-time observation
has no frame to ride. Rendering it must warn.
""".
-include("arizona_stateless.hrl").

-export([render/1]).

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html([
        ~"<!DOCTYPE html>",
        {html, [], [
            {body, [], [
                {button, [{az_contextmenu, maps:get(chrome_cmd, Bindings)}], [~"chrome"]},
                ?inner_content
            ]}
        ]}
    ]).
