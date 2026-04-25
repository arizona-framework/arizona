-module(arizona_outer_layout).
-include("arizona_stateless.hrl").
-export([render/1]).

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html([
        ~"<outer>",
        ?inner_content,
        ~"</outer>"
    ]).
