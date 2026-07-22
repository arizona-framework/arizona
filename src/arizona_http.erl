-module(arizona_http).
-moduledoc """
Shared HTTP render pipeline for transport adapters that serve an
Arizona view as the page response.

Runs route middlewares against an already-wrapped
`arizona_req:request()`, then dispatches to
`arizona_render:render_view_to_iolist/2` and returns either a halt
signal (middleware emitted its own reply), the rendered page body,
or a crash/error page body. The caller is responsible for wrapping
the native request beforehand and translating the result into its
transport's native reply shape.

## Flow

1. Run `arizona_middleware:apply_middlewares/3` with the route's `bindings`.
2. On `{halt, HaltReq}` -- return `{halt, HaltReq}` unchanged; the
   caller decodes the stashed redirect (`arizona_req:halted_redirect/1`)
   and ships the reply (or a 204/400 when the middleware wrote its own).
3. On `{cont, Req1, Bindings1}` -- call
   `arizona_render:render_view_to_iolist/2` with the route's
   `layout`/`on_mount`. Return `{ok, 200, Page, Req1}` or, on crash or a
   stashed hot-reload error, `{error, 500, ErrorPage, Req1}` (the threaded
   req carries any response headers/cookies for the transport to flush).
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([render/3]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([error_info/0]).
-export_type([result/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-nominal error_info() :: #{
    class := error | exit | throw,
    reason := term(),
    stacktrace := erlang:stacktrace(),
    reload_url => binary() | undefined
}.

-nominal result() ::
    {halt, arizona_req:request()}
    | {ok, arizona_req:resp_status(), iolist(), arizona_req:request()}
    | {error, 500, iolist(), arizona_req:request()}.

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc """
Runs the full render pipeline for `Handler` against an already-wrapped
`arizona_req:request()`.
""".
-spec render(Handler, ArzReq, Opts) -> result() when
    Handler :: module(),
    ArzReq :: arizona_req:request(),
    Opts :: arizona_live:route_opts().
render(Handler, ArzReq, Opts) ->
    Bindings = maps:get(bindings, Opts, #{}),
    Middlewares = maps:get(middlewares, Opts, []),
    case arizona_middleware:apply_middlewares(Middlewares, ArzReq, Bindings) of
        {halt, HaltReq} ->
            %% Leave the halt intact -- the transport decodes the stashed
            %% redirect (and any response headers/cookies) off the req.
            {halt, HaltReq};
        {cont, ArzReq1, Bindings1} ->
            %% Thread the req so the transport can flush response headers/cookies
            %% a middleware stashed before rendering.
            do_render(Handler, ArzReq1, Bindings1, Opts)
    end.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% Dev-mode hot-reload SSE endpoint URL (set in `persistent_term` by
%% the dev reloader wiring). Returns `undefined` in production.
reload_url() ->
    persistent_term:get(arizona_reload_url, undefined).

do_render(H, ArzReq, Bindings, Opts) ->
    case arizona_reloader:get_error() of
        undefined ->
            try
                RenderOpts = #{
                    bindings => Bindings,
                    layouts => maps:get(layouts, Opts, []),
                    on_mount => maps:get(on_mount, Opts, [])
                },
                Page = arizona_render:render_view_to_iolist(H, RenderOpts),
                %% A view or middleware may stash a non-200 status (e.g. 401)
                %% while still rendering a body; default to 200.
                OkStatus =
                    case arizona_req:resp_status(ArzReq) of
                        undefined -> 200;
                        Status -> Status
                    end,
                {ok, OkStatus, Page, ArzReq}
            catch
                Class:Reason:Stacktrace ->
                    %% arizona catches the render crash here, before roadrunner's
                    %% own crash logger would ever see it, so without this the
                    %% exception is swallowed and the operator gets no record of
                    %% the 500. Log it, then serve the (dev-gated) error body.
                    logger:error("~s: ~p~n~p", [Class, Reason, Stacktrace]),
                    {error, 500, error_body(Bindings, Class, Reason, Stacktrace, Opts), ArzReq}
            end;
        #{errors := Errors} ->
            {error, 500, error_body(Bindings, error, {compile_error, Errors}, [], Opts), ArzReq}
    end.

%% The rich error page (exception class, reason, and full stack with file:line)
%% is a development aid -- serving it in production leaks internals to any visitor
%% who can trigger a crash. Gate it on the dev signal (`reload_url` is set only
%% when the dev reloader is wired) and default production to a minimal 500 with no
%% internals. The compile-error branch only fires under a running reloader, so it
%% always resolves to the rich page.
error_body(Bindings, Class, Reason, Stacktrace, Opts) ->
    case reload_url() of
        undefined ->
            minimal_error_body();
        ReloadUrl ->
            ErrorInfo = #{
                class => Class,
                reason => Reason,
                stacktrace => Stacktrace,
                reload_url => ReloadUrl
            },
            render_error_body(Bindings, ErrorInfo, Opts)
    end.

render_error_body(Bindings, ErrorInfo, Opts) ->
    {Mod, Fun} = error_page(Opts),
    Tmpl = Mod:Fun(Bindings#{error_info => ErrorInfo}),
    arizona_render:render_to_iolist(Tmpl).

%% Production 500 body: no class/reason/stack and no reload script.
minimal_error_body() ->
    ~"""
    <!DOCTYPE html>
    <html lang="en">
    <head><meta charset="utf-8"><title>500 -- Server Error</title></head>
    <body><h1>Internal Server Error</h1></body>
    </html>
    """.

%% The error page module/function is baked into the route's `Opts` by the router
%% (from the owning listener's `error_page` config), so a multi-listener setup
%% renders each listener's own error page and no shared global term is clobbered.
error_page(Opts) ->
    maps:get(error_page, Opts, {arizona_error_page, render}).
