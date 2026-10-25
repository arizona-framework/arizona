-module(arizona_state_echo_controller).
-moduledoc """
**TEST FIXTURE.** Returns its route-configured `state` marker as the response body,
to verify `arizona_controller:dispatch/2` runs the handler through the middleware
pipeline and restores the app `state` (the `state` of the action's context).
""".

-export([handle/2]).

-spec handle(Req, Ctx) -> {Reply, Req} when
    Req :: arizona_req:request(),
    Ctx :: arizona_controller:context(),
    Reply :: arizona_controller:reply().
handle(Req, #{state := #{marker := Marker}}) ->
    {{200, [{~"content-type", ~"text/plain"}], Marker}, Req}.
