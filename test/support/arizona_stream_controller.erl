-module(arizona_stream_controller).
-moduledoc """
**TEST FIXTURE.** Answers with a `{stream, ...}` response -- one of the
non-buffered `t:roadrunner_handler:response/0` shapes -- so a route whose
middleware stashed a response header/cookie proves the dispatcher can flush the
stash onto something other than the buffered `{Status, Headers, Body}` triple.
""".

-behaviour(roadrunner_handler).

-export([handle/1]).

-spec handle(Req) -> {Response, Req} when
    Req :: roadrunner_req:request(),
    Response :: roadrunner_handler:response().
handle(Req) ->
    Resp =
        {stream, 200, [{~"content-type", ~"text/plain"}], fun(Send) ->
            ok = Send(~"STREAM_OK", fin)
        end},
    {Resp, Req}.
