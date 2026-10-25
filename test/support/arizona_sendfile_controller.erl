-module(arizona_sendfile_controller).
-moduledoc """
**TEST FIXTURE.** A `{sendfile, ...}` reply of the file the route `state` names
(`#{path => Path}`), skipping its first byte so the range is observable.
""".

-export([handle/2]).

-spec handle(Req, Ctx) -> {Reply, Req} when
    Req :: arizona_req:request(),
    Ctx :: arizona_controller:context(),
    Reply :: arizona_controller:reply().
handle(Req, #{state := #{path := Path}}) ->
    Size = filelib:file_size(Path),
    Headers = [
        {~"content-type", ~"text/plain"}, {~"content-length", integer_to_binary(Size - 1)}
    ],
    {{sendfile, 200, Headers, {Path, 1, Size - 1}}, Req}.
