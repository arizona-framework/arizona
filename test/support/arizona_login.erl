-module(arizona_login).
-include("arizona_view.hrl").
-export([mount/2, render/1]).

-spec mount(az:bindings(), az:request()) -> az:mount_ret().
mount(Bindings, _Req) ->
    {maps:merge(#{id => ~"login"}, Bindings), #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {main, [{id, ?get(id)}], [
            {h1, [], [~"Sign in"]}
        ]}
    ).
