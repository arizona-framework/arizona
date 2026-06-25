-module(arizona_os_caps_probe).
-moduledoc """
Test fixture for the native-shell capability negotiation chain.

Reads `?capability(~"window_title")` and `?reconnected` in `mount/1` and renders
`CAP_YES`/`CAP_NO` + `RECONNECT_YES`/`RECONNECT_NO`, so a CT test can assert the
full path end-to-end: the `_az_caps` / `_az_reconnect` WS upgrade params ->
`arizona_ws:prepare/3` -> the live process's `$arizona_capabilities` /
`$arizona_reconnected` dict -> `?capability/1` / `?reconnected` -> rendered output.
(The browser e2e covers this with a fake shell; this exercises it at the Erlang
layer without a browser.)
""".
-include("arizona_stateful.hrl").

-export([mount/1]).
-export([render/1]).

-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings) ->
    {
        #{
            id => maps:get(id, Bindings, ~"caps_probe"),
            cap => ?capability(~"window_title"),
            reconnected => ?reconnected
        },
        #{}
    }.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {'div', [{id, ?get(id)}], [
            case ?get(cap) of
                true -> ~"CAP_YES";
                false -> ~"CAP_NO"
            end,
            case ?get(reconnected) of
                true -> ~"RECONNECT_YES";
                false -> ~"RECONNECT_NO"
            end
        ]}
    ).
