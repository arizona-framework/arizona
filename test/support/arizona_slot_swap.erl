-module(arizona_slot_swap).
-include("arizona_stateful.hrl").
-export([mount/1, render/1, handle_event/3]).
-export([drawer/1, row/1]).

%% Regression fixture for the "cleared slot content stays on screen and the next
%% fill draws it twice" bug.
%%
%% Two properties together are what trigger it, and both are load-bearing:
%%
%% 1. The drawer is NOT the first content slot of its element, so its slot az is
%%    compound (`<ElemAz>:1`). `arizona_template:scope_slot/2` namespaces the
%%    slot's value by that az made colon-free, so every marker inside the drawer
%%    carries a BARE NUMERIC segment: `<Fp>-0-1-<Fp2>-<id>`.
%% 2. The drawer's `content` prop is a whole template whose items are themselves
%%    `?stateless` descriptors, so the content's own marker pairs land as
%%    SIBLINGS inside the drawer's content slot rather than inside a wrapper
%%    element -- which is what makes the client's slot walker have to count
%%    marker nesting at all.
%%
%% The client's `MARKER_OPEN` modelled an az as strict `<Fp>-<id>` alternation,
%% which rejects (1). The walker then read the first NESTED closer as the slot's
%% own, so clearing the drawer emptied only up to that closer and stranded the
%% rest of the content outside the slot, where the next fill drew a second copy
%% beside it. Nothing logged.

-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings) ->
    {
        #{
            id => ~"swap",
            tab => maps:get(tab, Bindings, ~"a"),
            drawers => maps:get(drawers, Bindings, #{})
        },
        #{}
    }.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {'div', [{id, ?get(id)}], [
            %% The first content slot. Its only job is to push the drawer off
            %% slot 0 and onto the compound `:1` az.
            ?get(tab),
            ~" | ",
            ?stateless(drawer, #{
                content => maps:get(~"debug", ?get(drawers), ~"")
            })
        ]}
    ).

-spec drawer(az:bindings()) -> az:template().
drawer(Props) ->
    ?html(
        {section, [{class, ~"drawer"}], [maps:get(content, Props)]}
    ).

-spec row(az:bindings()) -> az:template().
row(Props) ->
    ?html(
        {p, [{class, ~"row"}], [maps:get(label, Props)]}
    ).

%% The drawer body: a template whose items are `?stateless` descriptors, so its
%% marker pairs are siblings inside the drawer's content slot. Stashed in a plain
%% map under `drawers` and handed over unchanged across renders, as the reported
%% app does.
-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"open", _Payload, Bindings) ->
    {Bindings#{drawers => #{~"debug" => body()}}, #{}, []};
handle_event(~"close", _Payload, Bindings) ->
    {Bindings#{drawers => #{}}, #{}, []}.

body() ->
    ?html([
        ~"lead ",
        ?stateless(row, #{label => ~"one"}),
        ?stateless(row, #{label => ~"two"})
    ]).
