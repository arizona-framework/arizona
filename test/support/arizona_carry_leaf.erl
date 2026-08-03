-module(arizona_carry_leaf).
-include("arizona_stateful.hrl").
-export([mount/1]).
-export([render/1]).
-export([handle_event/3]).
-export([unmount/1]).

%% Grandchild whose unmount is observable and whose own events are addressable,
%% so a suite can tell "still live" from "unmounted and silently dropped".

-spec mount(az:bindings()) -> az:mount_ret().
mount(Props) ->
    {#{id => maps:get(id, Props), notify => maps:get(notify, Props), count => 0}, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html({span, [{id, ?get(id)}], [?get(count)]}).

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"inc", _Payload, Bindings) ->
    {Bindings#{count => maps:get(count, Bindings) + 1}, #{}, []}.

-spec unmount(az:bindings()) -> ok.
unmount(#{id := Id, notify := Notify}) ->
    Notify ! {leaf_unmounted, Id},
    ok.
