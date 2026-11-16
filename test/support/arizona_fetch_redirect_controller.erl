-module(arizona_fetch_redirect_controller).
-moduledoc """
**TEST FIXTURE.** A controller answering with `arizona_controller:reply_redirect/1` to the
relative `Location` in its route state, so the client must resolve it against the
endpoint's URL, as an HTTP 3xx would be. Drives `e2e/parallel/arizona_base_href.spec.js`.
""".

-export([handle/2]).

-spec handle(Req, Ctx) -> {Reply, Req} when
    Req :: arizona_req:request(),
    Ctx :: arizona_controller:context(),
    Reply :: arizona_controller:reply().
handle(Req, #{state := Location}) ->
    {arizona_controller:reply_redirect(Location), Req}.
