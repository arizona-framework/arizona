-module(arizona_sendfile_controller).
-moduledoc """
**TEST FIXTURE.** A `{sendfile, ...}` reply of the file the route `state` names
(`#{path => Path}`), skipping its first byte so the range is observable.
""".

-export([handle/1]).

-spec handle(Req) -> {Reply, Req} when
    Req :: arizona_req:request(),
    Reply :: arizona_controller:reply().
handle(Req) ->
    #{path := Path} = arizona_controller:state(Req),
    Size = filelib:file_size(Path),
    Headers = [
        {~"content-type", ~"text/plain"}, {~"content-length", integer_to_binary(Size - 1)}
    ],
    {{sendfile, 200, Headers, {Path, 1, Size - 1}}, Req}.
