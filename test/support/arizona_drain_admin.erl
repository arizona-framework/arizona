-module(arizona_drain_admin).
-moduledoc """
**TEST FIXTURE — DO NOT EXPOSE IN PRODUCTION.** Unauthenticated handler
that triggers a soft drain on any named listener; mounting it on a
public route is an instant DoS vector. Lives in `test/support/` and is
only wired into the e2e fixture in `arizona_test_server`.

Calls the route state's `adapter`'s `notify_drain/2` (`arizona_server`) so the
e2e drain spec can tell in-flight WS sessions to wind down without stopping
the listener.

Query params:

- `listener` -- listener atom name (default `http`).
- `deadline_ms` -- grace window in milliseconds from now (default `5000`).

Always replies `204 No Content`.
""".

-export([handle/2]).

-spec handle(Req, Ctx) -> {Reply, Req} when
    Req :: arizona_req:request(),
    Ctx :: arizona_controller:context(),
    Reply :: arizona_controller:reply().
handle(Req0, #{state := #{adapter := Adapter}}) ->
    {Qs, Req1} = arizona_req:params(Req0),
    Listener = binary_to_existing_atom(proplists:get_value(~"listener", Qs, ~"http")),
    DeadlineMs = binary_to_integer(proplists:get_value(~"deadline_ms", Qs, ~"5000")),
    ok = Adapter:notify_drain(Listener, DeadlineMs),
    {{204, [], <<>>}, Req1}.
