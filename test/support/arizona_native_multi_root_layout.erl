-module(arizona_native_multi_root_layout).
-include("arizona_stateless.hrl").
-export([render/1]).

%% A `?native` layout with two top-level nodes: a banner beside the page. A
%% native document is one JSON node, so the render refuses it.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?native([{'Banner', [], [~"top"]}, ?inner_content]).
