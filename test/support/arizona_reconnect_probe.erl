-module(arizona_reconnect_probe).
-include("arizona_stateful.hrl").
-export([mount/1]).
-export([render/1]).
-export([handle_update/3]).

%% Route-level page recording what `?reconnected` reads at each lifecycle step:
%% at mount (a reconnect's mount, or a page reached by navigate) and in
%% handle_update/3 (a patch of the mounted view).

-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings) ->
    {
        #{
            id => maps:get(id, Bindings, ~"reconnect_probe"),
            at_mount => ?reconnected,
            at_patch => undefined
        },
        #{}
    }.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html({main, [{id, ?get(id)}], [atom_to_binary(?get(at_mount))]}).

-spec handle_update(map(), az:bindings(), az:effects()) -> az:handle_update_ret().
handle_update(_Params, Bindings, Effects) ->
    {Bindings#{at_patch => ?reconnected}, #{}, Effects}.
