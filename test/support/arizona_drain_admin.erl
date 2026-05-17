-module(arizona_drain_admin).
-moduledoc """
**TEST FIXTURE — DO NOT EXPOSE IN PRODUCTION.** Unauthenticated handler
that triggers a soft drain on any named listener; mounting it on a
public route is an instant DoS vector. Lives in `test/support/` and is
only wired into the e2e fixture in `arizona_test_server`.

Triggers `roadrunner_listener:notify_drain/2` so the e2e drain spec can
broadcast `{roadrunner_drain, Deadline}` to in-flight WS sessions
without stopping the listener.

Query params:

- `listener` -- listener atom name (default `http`).
- `deadline_ms` -- grace window in milliseconds from now (default `5000`).

Always replies `204 No Content`.
""".

-behaviour(roadrunner_handler).

-export([handle/1]).

-spec handle(Req) -> {Response, Req} when
    Req :: roadrunner_req:request(),
    Response :: roadrunner_handler:response().
handle(Req) ->
    Qs = roadrunner_req:parse_qs(Req),
    Listener = binary_to_existing_atom(proplists:get_value(~"listener", Qs, ~"http")),
    DeadlineMs = binary_to_integer(proplists:get_value(~"deadline_ms", Qs, ~"5000")),
    Deadline = erlang:monotonic_time(millisecond) + DeadlineMs,
    ok = roadrunner_listener:notify_drain(Listener, Deadline),
    {roadrunner_resp:no_content(), Req}.
