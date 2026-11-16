-module(arizona_fetch_redirect_controller).
-moduledoc """
**TEST FIXTURE.** A controller answering with the reply its route state names:
`{redirect, Location}` is `arizona_controller:reply_redirect/1` to a relative
`Location`, which the client must resolve against the endpoint's URL as an HTTP 3xx
would be; `{effects, Effects}` is `reply_effects/1`, whose SPA navigate/patch URLs stay
the page's. Drives `e2e/parallel/arizona_base_href.spec.js`.
""".

-export([handle/2]).

-spec handle(Req, Ctx) -> {Reply, Req} when
    Req :: arizona_req:request(),
    Ctx :: arizona_controller:context(),
    Reply :: arizona_controller:reply().
handle(Req, #{state := {redirect, Location}}) ->
    {arizona_controller:reply_redirect(Location), Req};
handle(Req, #{state := {effects, Effects}}) ->
    {arizona_controller:reply_effects(Effects), Req}.
