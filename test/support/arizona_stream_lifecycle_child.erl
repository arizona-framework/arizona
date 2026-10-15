-module(arizona_stream_lifecycle_child).
-include("arizona_stateful.hrl").
-export([mount/1]).
-export([render/1]).
-export([unmount/1]).

%% Stream item child whose whole lifecycle is observable: it messages the pid
%% handed down as the `notify` prop on mount and on unmount, so a suite can
%% assert an item that leaves the stream unmounts its child and one that comes
%% back mounts a fresh one.

-spec mount(az:bindings()) -> az:mount_ret().
mount(Props) ->
    Id = maps:get(id, Props),
    Notify = maps:get(notify, Props),
    Notify ! {child_mounted, Id},
    {#{id => Id, notify => Notify}, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html({span, [{id, ?get(id)}], [~"child"]}).

-spec unmount(az:bindings()) -> ok.
unmount(#{id := Id, notify := Notify}) ->
    Notify ! {child_unmounted, Id},
    ok.
