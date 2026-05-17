-module(arizona_roadrunner_http).
-moduledoc """
Roadrunner handler that runs an Arizona handler for the initial
server-side render.

Wired up by `arizona_roadrunner_router` for every Arizona route. The
handler delegates the full render pipeline (wrap roadrunner req in an
`arizona_req:request()`, run middlewares, call
`arizona_render:render_view_to_iolist/3`, handle crashes + stashed
hot-reload compile errors) to `arizona_http:render/3`, and translates
its result into roadrunner's `{Response, Req}` reply shape.
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
handler and replies with HTML.
""".
-spec handle(Req) -> {Response, Req} when
    Req :: roadrunner_http1:request(),
    Response :: roadrunner_handler:response().
handle(Req) ->
    #{arizona := State} = roadrunner_req:state(Req),
    #{handler := H} = State,
    ArzReq = arizona_roadrunner_req:new(Req),
    case arizona_http:render(H, ArzReq, State) of
        {halt, RawReq} ->
            %% Middleware emitted its own response via direct roadrunner
            %% calls (rare). Pass through as a 204 to satisfy the result
            %% shape; the middleware should have already shipped bytes.
            {roadrunner_resp:no_content(), RawReq};
        {redirect, Status, Location} ->
            {roadrunner_resp:redirect(Status, Location), Req};
        {ok, Status, Body} ->
            {reply(Status, Body), Req};
        {error, Status, Body} ->
            {reply(Status, Body), Req}
    end.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

reply(Status, Body) ->
    %% Match arizona_cowboy_http's bare `text/html` content-type (no
    %% charset suffix) so HTTP responses look identical between
    %% adapters. roadrunner accepts iodata bodies all the way to
    %% gen_tcp:send, so pass the iolist through unflattened — saves a
    %% full copy of the rendered page per request.
    {Status,
        [
            {~"content-type", ~"text/html"},
            {~"content-length", integer_to_binary(iolist_size(Body))}
        ],
        Body}.
