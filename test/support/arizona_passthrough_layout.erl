-module(arizona_passthrough_layout).
-include("arizona_stateless.hrl").
-export([render/1]).

%% A layout that returns `?inner_content` bare: a backendless template, so it
%% adds no render target of its own and the document keeps the page's.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?inner_content.
