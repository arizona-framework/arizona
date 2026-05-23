-module(arizona_native_parent).
-include("arizona_view.hrl").
-export([mount/2, render/1]).

%% Native (JSON) parent view embedding a native stateful child, for testing
%% nested stateful rendering through the live mount_and_render frame path.

-spec mount(az:bindings(), az:request()) -> az:mount_ret().
mount(Bindings, _Req) ->
    {#{id => ~"native_parent", count => maps:get(count, Bindings, 5)}, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?native(
        {'Column', [{id, ?get(id)}], [
            ?stateful(arizona_native_badge, #{id => ~"badge", count => ?get(count)})
        ]}
    ).
