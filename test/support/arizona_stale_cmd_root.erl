-module(arizona_stale_cmd_root).
-moduledoc """
Fixture: a root embedding `arizona_stale_cmd_child`, whose attribute becomes a
command on the child's own event. When that event comes from a caller other than
the transport, the root's copy of the child stays stale, and the next root diff
that re-renders the child (its `n` prop changed) re-emits the attribute op.
""".
-include("arizona_stateful.hrl").

-export([mount/1]).
-export([render/1]).
-export([handle_event/3]).

-spec mount(az:bindings()) -> az:mount_ret().
mount(_Bindings) ->
    {#{id => ~"scr", n => 0}, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {main, [{id, ?get(id)}], [
            {p, [], [integer_to_binary(?get(n))]},
            ?stateful(arizona_stale_cmd_child, #{id => ~"scc", n => ?get(n)})
        ]}
    ).

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"bump", _Payload, Bindings) ->
    {Bindings#{n => maps:get(n, Bindings) + 1}, #{}, []}.
