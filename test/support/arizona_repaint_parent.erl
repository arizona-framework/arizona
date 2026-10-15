-module(arizona_repaint_parent).
-include("arizona_stateful.hrl").
-export([mount/1]).
-export([render/1]).

%% Root view embedding arizona_repaint_child, for asserting that a full-frame
%% repaint (`arizona_live:render_current/1`) reads the live tree without running
%% any child lifecycle callback.

-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings) ->
    {#{id => ~"repaint_parent", notify => maps:get(notify, Bindings)}, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {main, [{id, ?get(id)}], [
            ?stateful(arizona_repaint_child, #{id => ~"repaint_child", notify => ?get(notify)})
        ]}
    ).
