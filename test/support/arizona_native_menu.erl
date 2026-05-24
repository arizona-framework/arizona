-module(arizona_native_menu).
-include("arizona_view.hrl").
-export([mount/2, render/1, handle_event/3]).

%% Native (JSON) menu: each Button navigates to another example on the SAME
%% socket (arizona_android:navigate -> the server's handle_navigate re-mounts and
%% replies with OP_REPLACE). The "Counter (effect)" button instead push_events,
%% and handle_event returns a navigate effect -- exercising the handler-returned
%% effect ("e" array) path.

-spec mount(az:bindings(), az:request()) -> az:mount_ret().
mount(_Bindings, _Req) ->
    {#{id => ~"native_menu"}, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?native(
        {'Column', [{id, ?get(id)}], [
            {'Button', [{on_tap, arizona_android:navigate(~"/native/counter")}], [~"Counter"]},
            {'Button', [{on_tap, arizona_android:navigate(~"/native/list")}], [~"List"]},
            {'Button', [{on_tap, arizona_android:navigate(~"/native/tabs")}], [~"Tabs"]},
            {'Button', [{on_tap, arizona_android:navigate(~"/native/ticker")}], [~"Ticker"]},
            {'Button', [{on_tap, arizona_android:navigate(~"/native/multi")}], [~"Multi"]},
            {'Button', [{on_tap, arizona_android:push_event(~"open_counter")}], [
                ~"Counter (effect)"
            ]}
        ]}
    ).

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"open_counter", _Payload, Bindings) ->
    {Bindings, #{}, [arizona_android:navigate(~"/native/counter")]}.
