-module(arizona_opaque_nested_child).
-moduledoc """
Embedded child of `arizona_opaque_nested`: its command arrives as a prop and
reaches the attribute opaquely, so only render-time observation declares it.
""".
-include("arizona_stateful.hrl").

-export([mount/1]).
-export([render/1]).

-spec mount(az:bindings()) -> az:mount_ret().
mount(Props) ->
    {#{id => maps:get(id, Props), cmd => maps:get(cmd, Props)}, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html({'div', [{id, ?get(id)}, {az_wheel, ?get(cmd)}], [~"child"]}).
