-module(arizona_stream_once_child).
-include("arizona_stateful.hrl").
-export([mount/1]).
-export([render/1]).
-export([handle_update/3]).
-export([unmount/1]).

%% Stream item child that reports every lifecycle callback to the `notify` pid,
%% with the `v` prop it saw, so a suite can count how often one diff ran each
%% callback for an item and from which state.

-spec mount(az:bindings()) -> az:mount_ret().
mount(Props) ->
    Id = maps:get(id, Props),
    Notify = maps:get(notify, Props),
    V = maps:get(v, Props),
    Notify ! {child_mounted, Id, V},
    {#{id => Id, notify => Notify, v => V}, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html({span, [{id, ?get(id)}], [?get(v)]}).

-spec handle_update(az:bindings(), az:bindings(), [arizona_effect:effect()]) ->
    {az:bindings(), az:bindings(), [arizona_effect:effect()]}.
handle_update(#{v := New}, #{id := Id, notify := Notify, v := Old}, Effects) ->
    Notify ! {child_updated, Id, Old, New},
    {#{id => Id, notify => Notify, v => New}, #{}, Effects}.

-spec unmount(az:bindings()) -> ok.
unmount(#{id := Id, notify := Notify, v := V}) ->
    Notify ! {child_unmounted, Id, V},
    ok.
