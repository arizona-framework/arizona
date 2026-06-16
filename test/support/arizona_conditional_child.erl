-module(arizona_conditional_child).
-moduledoc """
Parent view that toggles a `?stateful` child in a content slot.

Models the idiomatic `case ?get(flag) of true -> ?stateful(...); false ->
~"" end` pattern: a stateful root with stable sibling elements before and
after a conditional child slot. At mount the flag is `false`, so the slot
renders the empty string; a `toggle` event flips it `true`, mounting the
child. The diff of that transition must patch only the slot (an ?OP_TEXT on
the marker content), leaving every sibling -- and the root element -- intact.
""".
-include("arizona_stateful.hrl").
-export([mount/1, render/1, handle_event/3]).

-spec mount(az:bindings()) -> az:mount_ret().
mount(Init) ->
    Bindings = #{
        id => maps:get(id, Init, ~"app"),
        show => false
    },
    {Bindings, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {main, [{id, ?get(id)}], [
            {header, [], [~"Header"]},
            case ?get(show) of
                true -> ?stateful(arizona_counter, #{id => ~"child", count => 0});
                false -> ~""
            end,
            {footer, [], [~"Footer"]}
        ]}
    ).

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"toggle", _Payload, Bindings) ->
    {Bindings#{show => not maps:get(show, Bindings, false)}, #{}, []}.
