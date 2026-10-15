-module(arizona_stream_lifecycle_child).
-include("arizona_stateful.hrl").
-export([mount/1]).
-export([render/1]).
-export([handle_event/3]).
-export([unmount/1]).

%% Stream item child whose whole lifecycle is observable: it messages the pid
%% handed down as the `notify` prop on mount and on unmount, so a suite can
%% assert an item that leaves the stream unmounts its child and one that comes
%% back mounts a fresh one. It renders a grandchild of the same kind (`<Id>-g`)
%% while `grand` is true: set by the `grand` prop (the default `handle_update/3`
%% merges it on a parent update), or by its own `grand_on` event, which no
%% enclosing container is re-evaluated for.

-spec mount(az:bindings()) -> az:mount_ret().
mount(Props) ->
    Id = maps:get(id, Props),
    Notify = maps:get(notify, Props),
    Notify ! {child_mounted, Id},
    {#{id => Id, notify => Notify, grand => maps:get(grand, Props, false)}, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {span, [{id, ?get(id)}], [
            ~"child",
            case ?get(grand) of
                true ->
                    ?stateful(arizona_stream_lifecycle_child, #{
                        id => <<(?get(id))/binary, "-g">>, notify => ?get(notify)
                    });
                false ->
                    ~""
            end
        ]}
    ).

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"grand_on", _Payload, Bindings) ->
    {Bindings#{grand => true}, #{}, []}.

-spec unmount(az:bindings()) -> ok.
unmount(#{id := Id, notify := Notify}) ->
    Notify ! {child_unmounted, Id},
    ok.
