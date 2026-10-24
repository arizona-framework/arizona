-module(arizona_bad_reply_controller).
-moduledoc """
**TEST FIXTURE.** Actions whose reply `arizona_controller:dispatch/2` must refuse:
a term that is no reply kind, and a `{loop, ...}` from a module with no
`handle_info/3`. Both write the session first, so a refusal that came after the
flush would have committed it.
""".

-export([handle/1]).
-export([loop/1]).

-spec handle(Req) -> {not_a_reply, Req} when
    Req :: arizona_req:request().
handle(Req) ->
    {not_a_reply, arizona_req:put_session(Req, ~"k", ~"v")}.

-spec loop(Req) -> {Reply, Req} when
    Req :: arizona_req:request(),
    Reply :: arizona_controller:reply().
loop(Req) ->
    {{loop, 200, [], state}, arizona_req:put_session(Req, ~"k", ~"v")}.
