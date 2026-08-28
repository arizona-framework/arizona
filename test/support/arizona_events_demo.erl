-module(arizona_events_demo).
-moduledoc """
Runtime discovery of a delegated event type, over a real socket.

The SSR page declares no non-bubbling event, so `toggle` is a type the client has
never bound. Arming renders a `<details az-toggle>` that reaches the browser as a
patch: the worker scans that markup, reports the name, and the main thread
delegates it before the element can be used. Every other test of that chain mocks
the worker, so this is the only one that exercises it end to end.
""".
-include("arizona_stateful.hrl").

-export([mount/1]).
-export([render/1]).
-export([handle_event/3]).

-spec mount(az:bindings()) -> az:mount_ret().
mount(Init) ->
    Bindings = #{
        id => ~"page",
        title => maps:get(title, Init, ~"Events"),
        armed => false,
        toggles => 0
    },
    {Bindings, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {main, [{id, ?get(id)}], [
            {h1, [], [?get(title)]},
            {button, [{id, ~"arm"}, {az_click, arizona_js:push_event(~"arm")}], [~"Arm"]},
            {'div', [{id, ~"slot"}], [
                case ?get(armed) of
                    true ->
                        {details, [{id, ~"det"}, {az_toggle, arizona_js:push_event(~"toggled")}], [
                            {summary, [{id, ~"sum"}], [~"Open"]},
                            ~"body"
                        ]};
                    false ->
                        <<>>
                end
            ]},
            {p, [{id, ~"toggles"}], [integer_to_binary(?get(toggles))]}
        ]}
    ).

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"arm", _Payload, Bindings) ->
    {Bindings#{armed => true}, #{}, []};
handle_event(~"toggled", _Payload, Bindings) ->
    {Bindings#{toggles => maps:get(toggles, Bindings) + 1}, #{}, []}.
