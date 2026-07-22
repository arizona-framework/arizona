-module(arizona_static_attr_escape).
-moduledoc """
Fixture for the static-attribute-value escaping regression (P4).

A static attribute value is a compile-time literal, yet it is element text that
must be HTML-escaped for the attribute context: a literal `"` would otherwise
terminate the attribute and `<`/`&` corrupt markup. The backend's `attr/2` is the
single escaping boundary, so a static literal and a dynamic scalar are escaped
identically -- and exactly once. A folded effect command (already escaped by
`arizona_effect:encode/1`) and a `?raw` opt-out must NOT be re-escaped.
""".
-include("arizona_stateless.hrl").
-export([render/1]).

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {'div',
            [
                {title, ~"say \"hi\" & <b>"},
                {'data-x', ~"a&b"},
                {'data-dyn', ?get(dyn)},
                {az_click, arizona_js:push_event(~"inc")}
            ],
            [~"x"]}
    ).
