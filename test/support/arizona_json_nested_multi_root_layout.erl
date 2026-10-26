-module(arizona_json_nested_multi_root_layout).
-include("arizona_stateless.hrl").
-export([render/1]).

%% The two top-level nodes of arizona_json_multi_root_layout, held one list
%% deeper: the body's one item is a literal list, which compiles to a nested
%% template, so the document root is still two nodes.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?json([[{'Banner', [], [~"top"]}, ?inner_content]]).
