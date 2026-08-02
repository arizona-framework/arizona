-module(arizona_unmount_parent).
-include("arizona_stateful.hrl").
-export([mount/1]).
-export([render/1]).
-export([unmount/1]).

%% Root view embedding arizona_unmount_child; both report their unmount to the
%% `notify` pid so the suite can assert child-before-root unmount ordering on
%% navigate and on terminate.

-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings) ->
    {#{id => ~"uparent", notify => maps:get(notify, Bindings)}, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {main, [{id, ?get(id)}], [
            ?stateful(arizona_unmount_child, #{id => ~"uchild", notify => ?get(notify)})
        ]}
    ).

-spec unmount(az:bindings()) -> ok.
unmount(#{id := Id, notify := Notify}) ->
    Notify ! {root_unmounted, Id},
    ok.
