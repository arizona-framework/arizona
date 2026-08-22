-module(arizona_stateful_layout_indirect).
-include("arizona_stateless.hrl").
-export([render/1]).

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {body, [], [
            ?stateless(arizona_stateful_chrome, render, #{}),
            ?inner_content
        ]}
    ).
