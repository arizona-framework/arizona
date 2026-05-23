-module(arizona_native_badge).
-include("arizona_stateful.hrl").
-export([mount/1, render/1]).

%% Native (JSON) stateful child for nested-component tests.

-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings) ->
    {
        #{
            id => maps:get(id, Bindings, ~"badge"),
            count => maps:get(count, Bindings, 0)
        },
        #{}
    }.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?native({'Badge', [{id, ?get(id)}], [?get(count)]}).
