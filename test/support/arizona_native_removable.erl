-module(arizona_native_removable).
-include("arizona_view.hrl").
-export([mount/2]).
-export([render/1]).
-export([handle_event/3]).

%% Native (JSON) view exercising the `remove` sentinel: the banner text dynamic
%% returns `remove` when hidden, so the diff emits OP_REMOVE_NODE -- the op the
%% native clients must handle (drop the node) rather than crash on. Removal is
%% one-way here (no toggle back); bringing it back would need a parent re-render.

-spec mount(az:bindings(), az:request()) -> az:mount_ret().
mount(_Bindings, _Req) ->
    {#{id => ~"native_removable", banner => ~"Banner!"}, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?native(
        {'Column', [{id, ?get(id)}], [
            {'Text', [], [?get(banner)]},
            {'Button', [{on_tap, arizona_android:push_event(~"hide")}], [~"Hide"]}
        ]}
    ).

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"hide", _Payload, Bindings) ->
    {Bindings#{banner => remove}, #{}, []}.
