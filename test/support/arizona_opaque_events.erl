-module(arizona_opaque_events).
-moduledoc """
Fixture for the render-time half of az-* delivery (`arizona_event_attrs`).

Every command here reaches its attribute as an OPAQUE dynamic -- a `?get` the
parse transform cannot prove is a command -- so none of these names sit in the
module's compile-time attribute set. Delivery relies on the renderer observing
the evaluated effect value: `az-mouseenter` at the connect mount (so it rides
the connect frame), `az-dblclick` on the frame whose branch first renders it
(the delta).
""".
-include("arizona_stateful.hrl").

-export([mount/1]).
-export([render/1]).
-export([handle_event/3]).

-spec mount(az:bindings()) -> az:mount_ret().
mount(Init) ->
    Bindings = #{
        id => ~"page",
        title => maps:get(title, Init, ~"Opaque"),
        %% The F1 repro: app data whose az-* name is a real DOM event and whose
        %% value parses as a structurally valid command list. Dynamic, so only
        %% render-time classification can tell it from a command -- and it is not
        %% one, so the name must never be delegated.
        ids => ~"[1,2,3]",
        enter_cmd => arizona_js:push_event(~"entered"),
        late_cmd => arizona_js:push_event(~"doubled"),
        %% Starts as data (false strips the attribute) and BECOMES a command on
        %% the arm_cmd event -- the in-place OP_SET_ATTR transition, which must
        %% carry the name as that frame's delta.
        flip_cmd => false,
        show_late => false,
        entered => 0,
        doubled => 0
    },
    {Bindings, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {main, [{id, ?get(id)}], [
            {h1, [], [?get(title)]},
            {p, [{id, ~"enter"}, {az_mouseenter, ?get(enter_cmd)}], [~"hover me"]},
            {p, [{id, ~"data"}, {az_select, ?get(ids)}], [~"data"]},
            {p, [{id, ~"flip"}, {az_mouseleave, ?get(flip_cmd)}], [~"flip"]},
            case ?get(show_late) of
                true -> {p, [{id, ~"late"}, {az_dblclick, ?get(late_cmd)}], [~"late"]};
                false -> <<>>
            end,
            {button, [{id, ~"reveal"}, {az_click, arizona_js:push_event(~"reveal")}], [
                ~"Reveal"
            ]},
            {p, [{id, ~"entered"}], [integer_to_binary(?get(entered))]},
            {p, [{id, ~"doubled"}], [integer_to_binary(?get(doubled))]}
        ]}
    ).

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"reveal", _Payload, Bindings) ->
    %% The effect makes the reply an ops+effects+delta frame, the shape that
    %% must not lose its effects to the delta.
    {Bindings#{show_late => true}, #{}, [arizona_js:set_title(~"revealed")]};
handle_event(~"arm_cmd", _Payload, Bindings) ->
    {Bindings#{flip_cmd => arizona_js:push_event(~"left")}, #{}, []};
handle_event(~"left", _Payload, Bindings) ->
    {Bindings#{entered => maps:get(entered, Bindings) + 1}, #{}, []};
handle_event(~"entered", _Payload, Bindings) ->
    {Bindings#{entered => maps:get(entered, Bindings) + 1}, #{}, []};
handle_event(~"doubled", _Payload, Bindings) ->
    {Bindings#{doubled => maps:get(doubled, Bindings) + 1}, #{}, []}.
