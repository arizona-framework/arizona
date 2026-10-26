-module(arizona_drain_event_handler).
-moduledoc """
A roadrunner event handler for `arizona_ws_SUITE`: forwards the deadline of
each `drain_acknowledged` event to the process registered as
`arizona_drain_event_probe`, and drops every other event.
""".

-export([handle_event/2]).

-spec handle_event(atom(), term()) -> ok.
handle_event(drain_acknowledged, {_Req, Deadline}) ->
    arizona_drain_event_probe ! {drain_acknowledged, Deadline},
    ok;
handle_event(_Event, _Args) ->
    ok.
