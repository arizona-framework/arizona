-module(arizona_login).
-include("arizona_stateful.hrl").
-export([mount/1, render/1]).

-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings) ->
    Flash = maps:get(flash, Bindings, #{}),
    {
        #{
            id => maps:get(id, Bindings, ~"login"),
            flash_msg => maps:get(~"error", Flash, ~"")
        },
        #{}
    }.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {main, [{id, ?get(id)}], [
            {h1, [], [~"Sign in"]},
            {p, [{id, ~"flash"}], [?get(flash_msg)]}
        ]}
    ).
