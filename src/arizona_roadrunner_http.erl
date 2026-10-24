-module(arizona_roadrunner_http).
-moduledoc """
Roadrunner handler that runs an Arizona handler for the initial
server-side render.

Wired up by `arizona_roadrunner_router` for every Arizona route. The
handler delegates the full render pipeline (wrap roadrunner req in an
`arizona_req:request()`, run middlewares, call
`arizona_render:render_view/2`, handle crashes + stashed
hot-reload compile errors) to `arizona_http:reply/3`, and maps
its result onto roadrunner's `{Response, Req}` reply shape.
""".

-behaviour(roadrunner_handler).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([handle/1]).

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc """
Roadrunner `handle/1` callback. Renders the configured Arizona
handler and replies with the document under its render target's media type.
""".
-spec handle(Req) -> {Response, Req} when
    Req :: roadrunner_req:request(),
    Response :: roadrunner_handler:response().
handle(Req) ->
    #{arizona := State} = roadrunner_req:state(Req),
    #{handler := H} = State,
    %% Hand roadrunner back the *threaded* raw request, never the one we came in
    %% with. Reading the body mutates the raw req in `body_buffering => manual`
    %% mode (it advances the body_reader), and roadrunner's finishing phase
    %% drains the reader off whatever req the handler returns -- give it the
    %% stale one and it re-reads the already consumed body out of the next
    %% pipelined request, corrupting keep-alive framing.
    {Status, Headers, Body, ArzReq1} = arizona_http:reply(
        H, arizona_roadrunner_req:new(Req), State
    ),
    ContentLength = {~"content-length", integer_to_binary(iolist_size(Body))},
    {{Status, Headers ++ [ContentLength], Body}, arizona_req:raw(ArzReq1)}.
