-module(arizona_unmount_raise_parent).
-include("arizona_stateful.hrl").
-export([mount/1]).
-export([render/1]).
-export([handle_event/3]).
-export([unmount/1]).

%% Root view rendering two children, the second of which raises from unmount/1
%% (unless `raise_child` is false). `hide` removes both in one diff; `replace`
%% removes both and mounts a third in the same diff. The root reports its own
%% unmount, and with `raise_root` then raises from it.

-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings) ->
    {
        #{
            id => ~"unmount_raise_parent",
            show => pair,
            notify => maps:get(notify, Bindings),
            raise_child => maps:get(raise_child, Bindings, true),
            raise_root => maps:get(raise_root, Bindings, false)
        },
        #{}
    }.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {main, [{id, ?get(id)}], [
            case ?get(show) of
                pair ->
                    {'div', [], [
                        ?stateful(arizona_unmount_raise_child, #{
                            id => ~"plain", notify => ?get(notify)
                        }),
                        ?stateful(arizona_unmount_raise_child, #{
                            id => ~"raising", notify => ?get(notify), raise => ?get(raise_child)
                        })
                    ]};
                fresh ->
                    {'div', [], [
                        ?stateful(arizona_unmount_raise_child, #{
                            id => ~"fresh", notify => ?get(notify)
                        })
                    ]};
                none ->
                    <<>>
            end
        ]}
    ).

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"hide", _Payload, Bindings) ->
    {Bindings#{show => none}, #{}, []};
handle_event(~"replace", _Payload, Bindings) ->
    {Bindings#{show => fresh}, #{}, []}.

-spec unmount(az:bindings()) -> ok.
unmount(#{id := Id, notify := Notify, raise_root := Raise}) ->
    Notify ! {unmounted, Id},
    case Raise of
        true -> error(root_unmount_boom);
        false -> ok
    end.
