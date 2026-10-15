-module(arizona_repaint_child).
-include("arizona_stateful.hrl").
-export([mount/1]).
-export([render/1]).
-export([handle_event/3]).
-export([handle_update/3]).

%% Embeddable counter reporting each mount/1 and handle_update/3 to the `notify`
%% pid, so a suite can count the lifecycle calls a code path makes.

-spec mount(az:bindings()) -> az:mount_ret().
mount(Props) ->
    Notify = maps:get(notify, Props),
    Notify ! {mount, arizona_repaint_child},
    {#{id => maps:get(id, Props), notify => Notify, count => 0}, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html({span, [{id, ?get(id)}], [~"count=", ?get(count)]}).

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"inc", _Payload, #{count := Count} = Bindings) ->
    {Bindings#{count => Count + 1}, #{}, []}.

-spec handle_update(map(), az:bindings(), az:effects()) -> az:handle_update_ret().
handle_update(_Props, #{notify := Notify} = Bindings, Effects) ->
    Notify ! {handle_update, arizona_repaint_child},
    {Bindings, #{}, Effects}.
