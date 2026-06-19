-module(arizona_roadrunner_controller).
-moduledoc """
Roadrunner handler that runs the Arizona middleware pipeline before a plain
controller handler.

A controller route (`{get, ...}`/`{post, ...}`/.../`{match, ...}`) compiles to this
dispatcher (the way `{live, ...}` routes go through `arizona_roadrunner_http`). It wraps
the request in an `arizona_req:request()`, runs the route's `middlewares` (so CSRF
`check_origin` and any app middleware apply to controllers too), and on `{cont, ...}`
restores the controller's `state` into the request and calls the app controller action
(`Handler:Action/1`, the route's `action` option defaulting to `handle`), flushing any
middleware-stashed cookies/headers onto its response. On `{halt, ...}` it ships the
halt (e.g. `check_origin`'s `403`) without invoking the action.
""".

-behaviour(roadrunner_handler).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([handle/1]).
-export([format_error/2]).

%% Called by `erl_error` via the `error_info` annotation, not directly.
-ignore_xref([format_error/2]).

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
    #{
        arizona := #{
            handler := Handler,
            action := Action,
            state := State,
            middlewares := Middlewares
        }
    } = roadrunner_req:state(Req),
    ArzReq = arizona_roadrunner_req:new(Req),
    case arizona_middleware:apply_middlewares(Middlewares, ArzReq, #{}) of
        {halt, HaltReq} ->
            {halt_response(HaltReq), arizona_req:raw(HaltReq)};
        {cont, ArzReq1, _Bindings} ->
            %% Restore the controller's own state into the raw request (the dispatcher
            %% overwrote it with the arizona meta), run the action, then flush any
            %% middleware-stashed cookies/headers onto the action's response.
            Raw = arizona_req:raw(ArzReq1),
            {Resp, Req1} = dispatch_action(Handler, Action, Raw#{state => State}),
            {flush_resp(ArzReq1, Resp), Req1}
    end.

-doc """
Renders a friendly message when a route names an `action` the controller module
does not export. Picked up by `erl_error` via the `error_info` annotation.
""".
-spec format_error(Reason, Stacktrace) -> ErrorInfo when
    Reason :: term(),
    Stacktrace :: [tuple()],
    ErrorInfo :: #{general := iolist()}.
format_error({missing_action, Handler, Action, Arity}, _ST) ->
    #{
        general => io_lib:format(
            "controller ~s does not export the action ~s/~b. "
            "Check the route's `action` option (default `handle`), or define the function.",
            [Handler, Action, Arity]
        )
    }.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% Dispatch to the controller's action (Handler:Action/1, `action` defaulting to
%% `handle` at compile time). A missing/unexported action is re-tagged into a clear
%% `{missing_action, ...}` error (rendered by format_error/2); an `undef` from
%% *inside* the action body propagates untagged.
dispatch_action(Handler, Action, Req) ->
    try
        Handler:Action(Req)
    catch
        error:undef:ST ->
            arizona_error:raise_or_propagate(
                undef,
                ST,
                Handler,
                Action,
                {missing_action, Handler, Action, 1},
                [Handler, Action, Req],
                ?MODULE
            )
    end.

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
