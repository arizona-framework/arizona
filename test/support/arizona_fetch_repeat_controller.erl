-module(arizona_fetch_repeat_controller).
-moduledoc """
**TEST FIXTURE.** The endpoints `arizona_fetch_repeat` posts to. `handle/2` counts the
request under the `token` its body carries and answers after `delay_ms` (route `state`),
so a repeat lands while the first request is still in flight; `count/2` answers how
many requests a `token` query parameter was counted for. The table is created by
`init/0`, from the test server's long-lived process. Drives
`e2e/parallel/arizona_fetch_repeat.spec.js`.
""".

-export([init/0]).
-export([handle/2]).
-export([count/2]).

-define(TABLE, ?MODULE).

-spec init() -> ok.
init() ->
    ?TABLE = ets:new(?TABLE, [named_table, public, set]),
    ok.

-spec handle(Req, Ctx) -> {Reply, Req} when
    Req :: arizona_req:request(),
    Ctx :: arizona_controller:context(),
    Reply :: arizona_controller:reply().
handle(Req0, #{state := #{delay_ms := Ms}}) ->
    {ok, Body, Req1} = arizona_req:body(Req0),
    Token = proplists:get_value(~"token", uri_string:dissect_query(Body)),
    _Count = ets:update_counter(?TABLE, Token, 1, {Token, 0}),
    receive
    after Ms -> ok
    end,
    {arizona_controller:reply_effects([]), Req1}.

-spec count(Req, Ctx) -> {Reply, Req} when
    Req :: arizona_req:request(),
    Ctx :: arizona_controller:context(),
    Reply :: arizona_controller:reply().
count(Req0, _Ctx) ->
    {Qs, Req1} = arizona_req:params(Req0),
    Token = proplists:get_value(~"token", Qs),
    N =
        case ets:lookup(?TABLE, Token) of
            [{Token, Count}] -> Count;
            [] -> 0
        end,
    {{200, [{~"content-type", ~"text/plain"}], integer_to_binary(N)}, Req1}.
