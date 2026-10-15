-module(arizona_native_passthrough_layout).
-include("arizona_stateless.hrl").
-export([render/1]).

%% A `?native` layout whose body is a one-item list holding `?inner_content`. It
%% records a backend but compiles to one bare dynamic with no node of its own (a
%% layout is nodiff, so no slot node wraps it), so the document root is whatever
%% it wraps.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?native([?inner_content]).
