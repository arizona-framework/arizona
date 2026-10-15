-module(arizona_swap_parent).
-include("arizona_stateful.hrl").
-export([mount/1]).
-export([render/1]).
-export([handle_event/3]).

%% Root view rendering one child at a fixed id whose HANDLER depends on `which`:
%% `swap` keeps the id and switches arizona_swap_a for arizona_swap_b in one diff.
%% The same event first queues a message for that id, addressed to the instance
%% the swap removes.

-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings) ->
    {#{id => ~"swap_parent", which => a, notify => maps:get(notify, Bindings)}, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {main, [{id, ?get(id)}], [
            case ?get(which) of
                a -> ?stateful(arizona_swap_a, #{id => ~"swapped", notify => ?get(notify)});
                b -> ?stateful(arizona_swap_b, #{id => ~"swapped", notify => ?get(notify)})
            end
        ]}
    ).

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"swap", _Payload, Bindings) ->
    ?send(~"swapped", queued_for_a),
    {Bindings#{which => b}, #{}, []}.
