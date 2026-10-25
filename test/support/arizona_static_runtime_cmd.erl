-module(arizona_static_runtime_cmd).
-moduledoc """
Fixture: a page whose only event attribute holds a command built in `mount/1`,
so the compiler cannot see it: only rendering proves `az-dblclick` an event.
""".
-include("arizona_stateful.hrl").
-export([mount/1, render/1]).

-spec mount(az:bindings()) -> az:mount_ret().
mount(_Bindings) ->
    {#{id => ~"runtime_cmd", cmd => arizona_js:toggle(~"#panel")}, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html({'div', [{id, ?get(id)}], [{button, [{az_dblclick, ?get(cmd)}], [~"b"]}]}).
