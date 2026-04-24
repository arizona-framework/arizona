-module(arizona_inner_layout).
-include("arizona_stateless.hrl").
-export([render/1]).

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html([
        ~"<inner>",
        ?inner_content,
        ~"</inner>"
    ]).
