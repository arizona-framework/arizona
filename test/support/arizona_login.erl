-module(arizona_login).
-include("arizona_view.hrl").
-export([mount/2, render/1]).

-spec mount(az:bindings(), az:request()) -> az:mount_ret().
mount(Bindings, _Req) ->
    {#{id => maps:get(id, Bindings, ~"login")}, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {main, [{id, ?get(id)}], [
            {h1, [], [~"Sign in"]}
        ]}
    ).
