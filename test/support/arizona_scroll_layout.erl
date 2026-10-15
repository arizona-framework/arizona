-module(arizona_scroll_layout).
-moduledoc """
An inner layout wrapping `/scroll-section`. Its layout list differs from the
other scroll fixtures', so an SPA navigate there degrades to a full page load.
""".
-include("arizona_stateless.hrl").
-export([render/1]).

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html({section, [{class, ~"scroll-section"}], [?inner_content]}).
