-module(arizona_unmount_child).
-include("arizona_stateful.hrl").
-export([mount/1]).
-export([render/1]).
-export([unmount/1]).

%% Embeddable child whose unmount side effect is observable: it messages the
%% pid handed down as the `notify` prop, so a suite can assert unmount/1 fired
%% (and in which order relative to the root's).

-spec mount(az:bindings()) -> az:mount_ret().
mount(Props) ->
    {#{id => maps:get(id, Props), notify => maps:get(notify, Props)}, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html({span, [{id, ?get(id)}], [~"child"]}).

-spec unmount(az:bindings()) -> ok.
unmount(#{id := Id, notify := Notify}) ->
    Notify ! {child_unmounted, Id},
    ok.
