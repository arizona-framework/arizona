-module(arizona_unmount_raise_child).
-include("arizona_stateful.hrl").
-export([mount/1]).
-export([render/1]).
-export([unmount/1]).

%% Embeddable child that reports its unmount and, with `raise` set, then raises
%% from it.

-spec mount(az:bindings()) -> az:mount_ret().
mount(Props) ->
    {
        #{
            id => maps:get(id, Props),
            notify => maps:get(notify, Props),
            raise => maps:get(raise, Props, false)
        },
        #{}
    }.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html({span, [{id, ?get(id)}], [?get(id)]}).

-spec unmount(az:bindings()) -> ok.
unmount(#{id := Id, notify := Notify, raise := Raise}) ->
    Notify ! {unmounted, Id},
    case Raise of
        true -> error(unmount_boom);
        false -> ok
    end.
