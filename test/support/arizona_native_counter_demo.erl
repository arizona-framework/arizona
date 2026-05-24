-module(arizona_native_counter_demo).
-include("arizona_view.hrl").
-export([mount/2, render/1, handle_event/3]).

%% Drivable native (JSON) counter view for the device/protocol e2e: a Text
%% showing the count plus +/- Buttons whose `on_tap` carries a push_event
%% command, so a native client tap drives handle_event over the WebSocket.

-spec mount(az:bindings(), az:request()) -> az:mount_ret().
mount(Bindings, _Req) ->
    {#{id => ~"native_counter", count => maps:get(count, Bindings, 0)}, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?native(
        {'Column', [{id, ?get(id)}], [
            {'Text', [], [~"Count: ", ?get(count)]},
            {'Button', [{on_tap, arizona_android:push_event(~"inc")}], [~"+"]},
            {'Button', [{on_tap, arizona_android:push_event(~"dec")}], [~"-"]}
        ]}
    ).

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"inc", _Payload, Bindings) ->
    {Bindings#{count => maps:get(count, Bindings) + 1}, #{}, []};
handle_event(~"dec", _Payload, Bindings) ->
    {Bindings#{count => maps:get(count, Bindings) - 1}, #{}, []}.
