-module(arizona_navigate_halt).
-include("arizona_stateful.hrl").
-export([mount/1, render/1]).

-spec mount(az:bindings()) -> az:mount_ret().
mount(_Bindings) ->
    {#{id => ~"navigate-halt"}, #{}}.

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
