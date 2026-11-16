-module(arizona_bindings_echo_controller).
-moduledoc """
**TEST FIXTURE.** Returns the request's path bindings as the response body, in
the external term format: a capture need not be UTF-8, so it cannot be text.
""".

-export([handle/2]).

-spec handle(Req, Ctx) -> {Reply, Req} when
    Req :: arizona_req:request(),
    Ctx :: arizona_controller:context(),
    Reply :: arizona_controller:reply().
handle(Req, _Ctx) ->
    {Bindings, Req1} = arizona_req:bindings(Req),
    {{200, [{~"content-type", ~"application/octet-stream"}], term_to_binary(Bindings)}, Req1}.
