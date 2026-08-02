-module(arizona_session_echo_controller).
-moduledoc """
**TEST FIXTURE.** Echoes the middleware-produced session back, to verify
`arizona_roadrunner_controller` threads the middleware pipeline's product to the
action: the post-middleware `arizona_req:request()` (`arizona_controller:req/1`)
and the produced bindings (`arizona_controller:bindings/1`).
""".

-behaviour(roadrunner_handler).

-export([handle/1]).

-spec handle(Req) -> {Response, Req} when
    Req :: roadrunner_req:request(),
    Response :: roadrunner_handler:response().
handle(Req) ->
    %% Via the post-middleware request: fetch_session already read the cookie.
    ArzReq = arizona_controller:req(Req),
    FromReq = arizona_req:get_session(ArzReq, user_id, ~"anon"),
    %% Via the middleware-produced bindings: fetch_session bound `session`.
    #{session := Session} = arizona_controller:bindings(Req),
    FromBinding = maps:get(~"user_id", Session, ~"anon"),
    Body = <<"session=", FromReq/binary, " binding=", FromBinding/binary>>,
    {roadrunner_resp:text(200, Body), Req}.
