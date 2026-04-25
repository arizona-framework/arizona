-module(arizona_navigate_halt).
-include("arizona_view.hrl").
-export([mount/2, render/1]).

-spec mount(az:bindings(), az:request()) -> az:mount_ret().
mount(Bindings, _Req) ->
    {maps:merge(#{id => ~"navigate-halt"}, Bindings), #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {main, [{id, ?get(id)}], [
            {h1, [], [~"Navigate Halt Demo"]},
            {a, [{id, ~"protected-link"}, {href, ~"/protected"}, az_navigate], [
                ~"Go to protected"
            ]}
        ]}
    ).
