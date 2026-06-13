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
            {halt_response(HaltReq), arizona_req:raw(HaltReq)};
        {ok, Status, Body, ArzReq1} ->
            {flush_resp(ArzReq1, roadrunner_resp:html(Status, Body)), Req};
        {error, Status, Body, ArzReq1} ->
            {flush_resp(ArzReq1, roadrunner_resp:html(Status, Body)), Req}
    end.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% Middleware halted before render. Ship a stashed redirect, or a 204 when
%% the middleware emitted its own response via direct roadrunner calls, then
%% flush any stashed response headers/cookies (put_resp_header/put_resp_cookie).
halt_response(HaltReq) ->
    Resp =
        case arizona_req:halted_redirect(HaltReq) of
            {Status, Location} -> roadrunner_resp:redirect(Status, Location);
            undefined -> roadrunner_resp:no_content()
        end,
    flush_resp(HaltReq, Resp).

%% Fold stashed response headers and cookies onto Resp.
flush_resp(ArzReq, Resp0) ->
    Resp1 = lists:foldl(
        fun({Name, Value}, R) -> roadrunner_resp:add_header(R, Name, Value) end,
        Resp0,
        arizona_req:resp_headers(ArzReq)
    ),
    lists:foldl(
        fun({Name, Value, Opts}, R) -> roadrunner_resp:set_cookie(R, Name, Value, Opts) end,
        Resp1,
        arizona_req:resp_cookies(ArzReq)
    ).
