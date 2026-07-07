-module(arizona_each_in_stateless).
-include("arizona_stateless.hrl").
-export([render/1]).
-export([listcard/1]).

%% Regression fixture: a repeated stateless whose body holds an ?each. The each
%% CONTAINER is scoped per instance (so container ops route to the right card),
%% while item ids stay container-relative (shared across items, resolved within
%% their own container) -- so the two cards' lists are independent diff targets.
%% Rendered directly via render/1 in arizona_render_SUITE.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {main, [{id, ?get(id)}], [
            ?stateless(listcard, #{items => ?get(xs)}),
            ?stateless(listcard, #{items => ?get(ys)})
        ]}
    ).

-spec listcard(az:bindings()) -> az:template().
listcard(Props) ->
    ?html(
        {ul, [{class, ~"list"}], [
            ?each(fun(Item) -> {li, [], [Item]} end, maps:get(items, Props))
        ]}
    ).
