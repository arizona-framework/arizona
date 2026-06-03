-module(arizona_local_nested).
-include("arizona_stateful.hrl").
-export([mount/1, render/1, handle_event/3]).

-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings) ->
    {
        #{
            id => maps:get(id, Bindings, ~"local_nested"),
            label => ~"v1"
        },
        #{}
    }.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {main, [{id, ?get(id)}], [
            {h1, [], [~"Nested ?local"]},

            %% Parent-owned client slot + a client-only edit button.
            {p, [{class, ~"pnote"}], [~"Parent note: ", ?local(~"pnote", ~"parent")]},
            {button,
                [{class, ~"edit-pnote"}, {az_click, arizona_js:set(~"pnote", ~"parent-edited")}], [
                    ~"Edit parent note"
                ]},

            %% A server event that changes a prop propagated down to both children.
            {button, [{class, ~"relabel"}, {az_click, arizona_js:push_event(~"relabel")}], [
                ~"Relabel children"
            ]},

            %% A server event that returns a ?local update as a HANDLER EFFECT
            %% (set_all), resetting both children's client-owned notes server-side.
            {button, [{class, ~"reset-notes"}, {az_click, arizona_js:push_event(~"reset_notes")}], [
                    ~"Reset notes"
                ]},

            %% Two stateful children, each with its own client-owned `note` slot.
            ?stateful(arizona_counter_local, #{id => ~"child_a", label => ?get(label)}),
            ?stateful(arizona_counter_local, #{id => ~"child_b", label => ?get(label)})
        ]}
    ).

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"relabel", _Payload, Bindings) ->
    {Bindings#{label => ~"v2"}, #{}, []};
handle_event(~"reset_notes", _Payload, Bindings) ->
    {Bindings, #{}, [arizona_js:set_all(~"note", ~"reset")]}.
