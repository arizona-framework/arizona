%% Embeddable stateful child whose template holds a markerless raw-text slot
%% (the `textarea` content compiles to an `Az = undefined` dynamic). Used by
%% `arizona_diff_SUITE` to assert the child-view diff never emits an op
%% targeting `undefined`.
-module(arizona_textarea_child).
-include("arizona_stateful.hrl").
-export([mount/1]).
-export([render/1]).

-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings) ->
    {
        #{
            id => maps:get(id, Bindings, ~"textarea_child"),
            text => maps:get(text, Bindings, ~"one")
        },
        #{}
    }.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {'div', [{id, ?get(id)}], [
            {label, [], [~"Note"]},
            {textarea, [], [?get(text)]}
        ]}
    ).
