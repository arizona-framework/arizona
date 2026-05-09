-module(arizona_leaky_mount).
-include("arizona_stateful.hrl").
-export([mount/1, render/1]).

%% Test fixture: mount/1 does an eager binding read against a synthetic map.
%% Used to verify that eval_stateful brackets the mount lifecycle so the
%% read does not leak into the outer dynamic's tracked deps.
-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings) ->
    eager_value = arizona_template:get(eager_mount_key, #{eager_mount_key => eager_value}),
    {
        #{
            id => maps:get(id, Bindings, ~"leaky"),
            count => maps:get(count, Bindings, 0)
        },
        #{}
    }.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html({'div', [{id, ?get(id)}], [~"leaky"]}).
