-module(arizona_session_echo_controller).
-moduledoc """
**TEST FIXTURE.** Echoes the middleware-produced session back, to verify
`arizona_controller:dispatch/3` threads the middleware pipeline's product to the
action: the post-middleware `arizona_req:request()` (`arizona_controller:req/1`)
and the produced bindings (the `bindings` of the action's context).
""".

-export([handle/2]).

-spec handle(Req, Ctx) -> {Reply, Req} when
    Req :: arizona_req:request(),
    Ctx :: arizona_controller:context(),
    Reply :: arizona_controller:reply().
handle(Req, #{bindings := #{session := Session}}) ->
    %% Via the request: fetch_session already read the cookie.
    FromReq = arizona_req:get_session(Req, user_id, ~"anon"),
    %% Via the middleware-produced bindings: fetch_session bound `session`.
    FromBinding = maps:get(~"user_id", Session, ~"anon"),
    Body = <<"session=", FromReq/binary, " binding=", FromBinding/binary>>,
    {{200, [{~"content-type", ~"text/plain"}], Body}, Req}.
