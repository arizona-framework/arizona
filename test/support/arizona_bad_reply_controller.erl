-module(arizona_bad_reply_controller).
-moduledoc """
**TEST FIXTURE.** Actions whose reply `arizona_controller:dispatch/3` must refuse:
a term that is no reply kind, and a `{loop, ...}` from a module with no
`handle_info/3`. Both write the session first, so a refusal that came after the
flush would have committed it.
""".

-export([handle/2]).
-export([loop/2]).

-spec handle(Req, Ctx) -> {not_a_reply, Req} when
    Req :: arizona_req:request(),
    Ctx :: arizona_controller:context().
handle(Req, _Ctx) ->
    {not_a_reply, arizona_req:put_session(Req, ~"k", ~"v")}.

-spec loop(Req, Ctx) -> {Reply, Req} when
    Req :: arizona_req:request(),
    Ctx :: arizona_controller:context(),
    Reply :: arizona_controller:reply().
loop(Req, _Ctx) ->
    {{loop, 200, [], state}, arizona_req:put_session(Req, ~"k", ~"v")}.
