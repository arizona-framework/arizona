-module(arizona_update_effect_child).
-include("arizona_stateful.hrl").
-export([mount/1, render/1, handle_update/3]).

-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings) ->
    {#{id => maps:get(id, Bindings), value => maps:get(value, Bindings, 0)}, #{}}.

%% On a parent prop change, fold a push_event effect onto the accumulator. The
%% framework threads it through process_root_change so it rides the originating
%% root event's reply -- proving handle_update/3 effects reach the wire.
-spec handle_update(az:bindings(), az:bindings(), az:effects()) -> az:handle_update_ret().
handle_update(Props, Bindings, Effects) ->
    {maps:merge(Bindings, Props), #{}, [arizona_js:push_event(~"child_updated") | Effects]}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html({span, [{id, ?get(id)}], [integer_to_binary(?get(value))]}).
