-module(arizona_json_menu).
-include("arizona_stateful.hrl").
-export([mount/1, render/1, handle_event/3]).

%% JSON menu: each Button navigates to another example on the SAME
%% socket (arizona_android:navigate -> the server's handle_navigate re-mounts and
%% replies with OP_REPLACE). The "Counter (effect)" button instead push_events,
%% and handle_event returns a navigate effect -- exercising the handler-returned
%% effect ("e" array) path.

-spec mount(az:bindings()) -> az:mount_ret().
mount(_Bindings) ->
    {#{id => ~"json_menu"}, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?json(
        {'Column', [{id, ?get(id)}], [
            {'Button', [{on_tap, arizona_android:navigate(~"/json/counter")}], [~"Counter"]},
            {'Button', [{on_tap, arizona_android:navigate(~"/json/list")}], [~"List"]},
            {'Button', [{on_tap, arizona_android:navigate(~"/json/tabs")}], [~"Tabs"]},
            {'Button', [{on_tap, arizona_android:navigate(~"/json/ticker")}], [~"Ticker"]},
            {'Button', [{on_tap, arizona_android:navigate(~"/json/multi")}], [~"Multi"]},
            {'Button', [{on_tap, arizona_android:navigate(~"/json/nested")}], [~"Nested"]},
            {'Button', [{on_tap, arizona_android:push_event(~"open_counter")}], [
                ~"Counter (effect)"
            ]},
            {'Button', [{on_tap, arizona_android:navigate(~"/json/removable")}], [~"Removable"]}
        ]}
    ).

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"open_counter", _Payload, Bindings) ->
    {Bindings, #{}, [arizona_android:navigate(~"/json/counter")]}.
