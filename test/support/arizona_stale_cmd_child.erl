-module(arizona_stale_cmd_child).
-moduledoc """
Embedded child of `arizona_stale_cmd_root`: `az-mouseleave` starts as `false` and
becomes a command on the `arm` event, reaching the attribute opaquely.
""".
-include("arizona_stateful.hrl").

-export([mount/1]).
-export([render/1]).
-export([handle_event/3]).

-spec mount(az:bindings()) -> az:mount_ret().
mount(Props) ->
    {#{id => maps:get(id, Props), cmd => false}, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html({'div', [{id, ?get(id)}, {az_mouseleave, ?get(cmd)}], [~"child"]}).

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"arm", _Payload, Bindings) ->
    {Bindings#{cmd => arizona_js:push_event(~"left")}, #{}, []}.
