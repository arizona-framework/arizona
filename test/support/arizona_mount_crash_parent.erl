-module(arizona_mount_crash_parent).
-include("arizona_stateful.hrl").
-export([mount/1]).
-export([render/1]).
-export([handle_event/3]).

%% Root view that, once `show` is on, renders two children in one pass: the
%% first mounts fine (arizona_swap_a, which reports its unmount), the second's
%% mount/1 raises. `show` comes from the mount bindings, so the same pair fails
%% either the mount itself or the diff after `show`.

-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings) ->
    {
        #{
            id => ~"mount_crash_parent",
            show => maps:get(show, Bindings, false),
            notify => maps:get(notify, Bindings)
        },
        #{}
    }.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {main, [{id, ?get(id)}], [
            case ?get(show) of
                true ->
                    {'div', [], [
                        ?stateful(arizona_swap_a, #{id => ~"mounted_ok", notify => ?get(notify)}),
                        ?stateful(arizona_crashable, #{id => ~"crashable", crash_on_mount => true})
                    ]};
                false ->
                    <<>>
            end
        ]}
    ).

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"show", _Payload, Bindings) ->
    {Bindings#{show => true}, #{}, []}.
