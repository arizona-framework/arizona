-module(arizona_http).
-moduledoc """
Shared HTTP render pipeline for transport adapters that serve an
Arizona view as the page response.

Runs route middlewares against an already-wrapped
`arizona_req:request()`, then dispatches to
`arizona_render:render_view/2` and returns either a halt
signal (middleware refused to continue), the rendered page body,
or a crash/error page body. The caller is responsible for wrapping
the native request beforehand and translating the result into its
transport's native reply shape.

## Flow

1. Run `arizona_middleware:apply_middlewares/3` with the route's `bindings`.
2. On `{halt, HaltReq}` -- return `{halt, HaltReq}` unchanged; the
   caller decodes the stashed redirect (`arizona_req:halted_redirect/1`)
   or status and ships the reply (a `403` when the middleware stashed
   neither).
3. On `{cont, Req1, Bindings1}` -- call
   `arizona_render:render_view/2` with the route's `layout`/`on_mount`.
   Return `{ok, 200, ContentType, Page, Req1}` or, on crash or a stashed
   hot-reload error, `{error, 500, ContentType, ErrorPage, Req1}` (the
   threaded req carries any response headers/cookies for the transport to
   flush). `ContentType` is the rendered document's media type, answered by
   its render backend, so a `?native` route serves JSON and an HTML one HTML.
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([render/3]).
-export([reply/3]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

%% Called by the server adapters, which live in their own packages.
-ignore_xref([reply/3]).
%% The step under `reply/3`, kept public for an adapter that frames its own reply.
-ignore_xref([render/3]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([error_info/0]).
-export_type([result/0]).
-export_type([reply/0]).

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
    | {ok, arizona_req:resp_status(), ContentType :: binary(), iolist(), arizona_req:request()}
    | {error, 500, ContentType :: binary(), iolist(), arizona_req:request()}.

%% A page response as plain data: status, header lines in send order, body, and
%% the threaded request (its raw value is what the adapter hands back to its
%% server).
-nominal reply() ::
    {arizona_req:resp_status(), [{binary(), binary()}], iodata(), arizona_req:request()}.

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

-doc """
`render/3` folded into the response a server adapter sends: a halt becomes its
`arizona_req:halt_reply/1` (empty body), a rendered page (or its error page)
gets `arizona_req:flush/1`'s lines then its `content-type`. Either way the
session is committed and the header lines are in send order, so the adapter
only adds its own framing (a `content-length`) and replies.
""".
-spec reply(Handler, ArzReq, Opts) -> reply() when
    Handler :: module(),
    ArzReq :: arizona_req:request(),
    Opts :: arizona_live:route_opts().
reply(Handler, ArzReq, Opts) ->
    case render(Handler, ArzReq, Opts) of
        {halt, HaltReq} ->
            {Status, Headers} = arizona_req:halt_reply(HaltReq),
            {Status, Headers, <<>>, HaltReq};
        {Outcome, Status, ContentType, Body, ArzReq1} when Outcome =:= ok; Outcome =:= error ->
            Headers = arizona_req:flush(ArzReq1) ++ [{~"content-type", ContentType}],
            {Status, Headers, Body, ArzReq1}
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
                {ContentType, Page} = render_view(H, RenderOpts),
                %% A view or middleware may stash a non-200 status (e.g. 401)
                %% while still rendering a body; default to 200.
                OkStatus =
                    case arizona_req:resp_status(ArzReq) of
                        undefined -> 200;
                        Status -> Status
                    end,
                {ok, OkStatus, ContentType, Page, ArzReq}
            catch
                Class:Reason:Stacktrace ->
                    %% arizona catches the render crash here, before roadrunner's
                    %% own crash logger would ever see it, so without this the
                    %% exception is swallowed and the operator gets no record of
                    %% the 500. Log it, then serve the (dev-gated) error body.
                    logger:error("~s: ~p~n~p", [Class, Reason, Stacktrace]),
                    {ErrorType, ErrorBody} = error_body(Bindings, Class, Reason, Stacktrace, Opts),
                    {error, 500, ErrorType, ErrorBody, ArzReq}
            end;
        #{errors := Errors} ->
            {ErrorType, ErrorBody} = error_body(Bindings, error, {compile_error, Errors}, [], Opts),
            {error, 500, ErrorType, ErrorBody, ArzReq}
    end.

%% The page is served as UTF-8, and a binary value is emitted as the bytes it
%% holds -- checking each one was measured too costly for every render. In dev
%% mode the page renders with every value checked instead (arizona_utf8:
%% render_view/2, same output, one mount), so a bad one is raised at its template
%% line into the catch above and reaches the dev error page. Production pays the
%% one dev-flag read.
render_view(H, RenderOpts) ->
    case reload_url() of
        undefined -> arizona_render:render_view(H, RenderOpts);
        _Dev -> arizona_utf8:render_view(H, RenderOpts)
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
            {arizona_html:content_type(), minimal_error_body()};
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
    {error_page_type(Tmpl), arizona_render:render_to_iolist(Tmpl)}.

%% The error page is whatever template the listener configured; its backend names
%% its media type, defaulting to HTML for a hand-built template that carries none.
error_page_type(#{backend := Backend}) -> Backend:content_type();
error_page_type(_Tmpl) -> arizona_html:content_type().

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
