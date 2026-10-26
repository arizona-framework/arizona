-module(arizona_json_passthrough_layout).
-include("arizona_stateless.hrl").
-export([render/1]).

%% A `?json` layout whose body is a one-item list holding `?inner_content`. It
%% records a backend but compiles to one bare dynamic with no node of its own (a
%% layout is nodiff, so no slot node wraps it), so the document root is whatever
%% it wraps.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?json([?inner_content]).
