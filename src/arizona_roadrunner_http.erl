-module(arizona_roadrunner_http).
-moduledoc """
Roadrunner handler that runs an Arizona handler for the initial
server-side render.

Wired up by `arizona_roadrunner_router` for every Arizona route. The
handler delegates the full render pipeline (wrap roadrunner req in an
`arizona_req:request()`, run middlewares, call
`arizona_render:render_view_to_iolist/2`, handle crashes + stashed
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
    Req :: roadrunner_req:request(),
    Response :: roadrunner_handler:response().
handle(Req) ->
    #{arizona := State} = roadrunner_req:state(Req),
    #{handler := H} = State,
    ArzReq = arizona_roadrunner_req:new(Req),
    case arizona_http:render(H, ArzReq, State) of
        {halt, HaltReq} ->
            {arizona_roadrunner_resp:halt(HaltReq), arizona_req:raw(HaltReq)};
        {ok, Status, Body, ArzReq1} ->
            {arizona_roadrunner_resp:flush(ArzReq1, roadrunner_resp:html(Status, Body)), Req};
        {error, Status, Body, ArzReq1} ->
            {arizona_roadrunner_resp:flush(ArzReq1, roadrunner_resp:html(Status, Body)), Req}
    end.
