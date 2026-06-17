-module(arizona_state_echo_controller).
-moduledoc """
**TEST FIXTURE.** Returns its route-configured `state` marker as the response body,
to verify `arizona_roadrunner_controller` runs the handler through the middleware
pipeline and restores the app `state` (read via `roadrunner_req:state/1`).
""".

-behaviour(roadrunner_handler).

-export([handle/1]).

-spec handle(Req) -> {Response, Req} when
    Req :: roadrunner_req:request(),
    Response :: roadrunner_handler:response().
handle(Req) ->
    #{marker := Marker} = roadrunner_req:state(Req),
    {roadrunner_resp:text(200, Marker), Req}.
