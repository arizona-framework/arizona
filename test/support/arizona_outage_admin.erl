-module(arizona_outage_admin).
-moduledoc """
**TEST FIXTURE — DO NOT EXPOSE IN PRODUCTION.** Unauthenticated handler that
stops the e2e listener and starts it again after `ms` milliseconds (default
`3000`): a server outage and restart, with every socket on the listener dropped
and every reconnect attempt refused meanwhile. Lives in `test/support/` and is
only wired into the e2e fixture in `arizona_test_server`.

Always replies `204 No Content`, before the listener goes down.
""".

-export([handle/2]).

-spec handle(Req, Ctx) -> {Reply, Req} when
    Req :: arizona_req:request(),
    Ctx :: arizona_controller:context(),
    Reply :: arizona_controller:reply().
handle(Req0, _Ctx) ->
    {Qs, Req1} = arizona_req:params(Req0),
    Ms = binary_to_integer(proplists:get_value(~"ms", Qs, ~"3000")),
    _Pid = spawn(fun() -> outage(Ms) end),
    {{204, [], <<>>}, Req1}.

%% After the reply has gone out: the listener stops under its own request.
outage(Ms) ->
    timer:sleep(100),
    ok = arizona_roadrunner_server:stop(http),
    timer:sleep(Ms),
    {ok, _} = arizona_test_server:start_listener().
