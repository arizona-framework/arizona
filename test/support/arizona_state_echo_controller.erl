-module(arizona_state_echo_controller).
-moduledoc """
**TEST FIXTURE.** Returns its route-configured `state` marker as the response body,
to verify `arizona_controller:dispatch/2` runs the handler through the middleware
pipeline and restores the app `state` (read via `arizona_controller:state/1`).
""".

-export([handle/1]).

-spec handle(Req) -> {Reply, Req} when
    Req :: arizona_req:request(),
    Reply :: arizona_controller:reply().
handle(Req) ->
    #{marker := Marker} = arizona_controller:state(Req),
    {{200, [{~"content-type", ~"text/plain"}], Marker}, Req}.
