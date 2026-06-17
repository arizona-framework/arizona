-module(arizona_roadrunner_controller).
-moduledoc """
Roadrunner handler that runs the Arizona middleware pipeline before a plain
controller handler.

A `{controller, Path, Handler, Opts}` route compiles to this dispatcher (the way
`{live, ...}` routes go through `arizona_roadrunner_http`). It wraps the request in an
`arizona_req:request()`, runs the route's `middlewares` (so CSRF `check_origin` and any
app middleware apply to controllers too), and on `{cont, ...}` restores the controller's
`state` into the request and calls the app `Handler:handle/1`, flushing any
middleware-stashed cookies/headers onto its response. On `{halt, ...}` it ships the
halt (e.g. `check_origin`'s `403`) without invoking the handler.
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
Roadrunner `handle/1` callback. Runs the route middlewares, then the app controller
handler with its configured `state` restored.
""".
-spec handle(Req) -> {Response, Req} when
    Req :: roadrunner_req:request(),
    Response :: roadrunner_handler:response().
handle(Req) ->
    #{arizona := #{handler := Handler, state := State, middlewares := Middlewares}} =
        roadrunner_req:state(Req),
    ArzReq = arizona_roadrunner_req:new(Req),
    case arizona_middleware:apply_middlewares(Middlewares, ArzReq, #{}) of
        {halt, HaltReq} ->
            {halt_response(HaltReq), arizona_req:raw(HaltReq)};
        {cont, ArzReq1, _Bindings} ->
            %% Restore the controller's own state into the raw request (the dispatcher
            %% overwrote it with the arizona meta), run the handler, then flush any
            %% middleware-stashed cookies/headers onto the handler's response.
            Raw = arizona_req:raw(ArzReq1),
            {Resp, Req1} = Handler:handle(Raw#{state => State}),
            {flush_resp(ArzReq1, Resp), Req1}
    end.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% A middleware halted before the handler. Ship a stashed redirect or status (e.g.
%% check_origin's 403), else a bare 400, then flush stashed headers/cookies.
halt_response(HaltReq) ->
    Resp =
        case arizona_req:halted_redirect(HaltReq) of
            {Status, Location} ->
                roadrunner_resp:redirect(Status, Location);
            undefined ->
                case arizona_req:resp_status(HaltReq) of
                    undefined -> roadrunner_resp:bad_request();
                    Status -> roadrunner_resp:status(Status)
                end
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
