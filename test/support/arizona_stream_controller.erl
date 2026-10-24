-module(arizona_stream_controller).
-moduledoc """
**TEST FIXTURE.** Answers with a `{stream, ...}` response -- one of the
non-buffered `t:arizona_controller:reply/0` kinds -- so a route whose
middleware stashed a response header/cookie proves the dispatcher can flush the
stash onto something other than the buffered `{Status, Headers, Body}` triple.
""".

-export([handle/1]).

-spec handle(Req) -> {Reply, Req} when
    Req :: arizona_req:request(),
    Reply :: arizona_controller:reply().
handle(Req) ->
    Reply =
        {stream, 200, [{~"content-type", ~"text/plain"}], fun(Send) ->
            ok = Send(~"STREAM_OK", fin)
        end},
    {Reply, Req}.
