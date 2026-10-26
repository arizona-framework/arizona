-module(arizona_json_multi_root_layout).
-include("arizona_stateless.hrl").
-export([render/1]).

%% A `?json` layout with two top-level nodes: a banner beside the page. A
%% JSON document is one JSON node, so the render refuses it.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?json([{'Banner', [], [~"top"]}, ?inner_content]).
