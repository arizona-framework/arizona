-module(arizona_http).
-moduledoc """
Shared HTTP render pipeline for transport adapters that serve an
Arizona view as the page response.

Runs route middlewares against an already-wrapped
`arizona_req:request()`, then dispatches to
`arizona_render:render_view_to_iolist/3` and returns either a halt
signal (middleware emitted its own reply), the rendered page body,
or a crash/error page body. The caller is responsible for wrapping
the native request beforehand and translating the result into its
transport's native reply shape.

## Flow

1. Run `arizona_req:apply_middlewares/3` with the route's `bindings`.
2. On `{halt, HaltReq}` -- return `{halt, RawReq}`; the caller ships
   the reply the middleware already wrote.
3. On `{cont, Req1, Bindings1}` -- call
   `arizona_render:render_view_to_iolist/3` with the route's
   `layout`/`on_mount`. Return `{ok, 200, Page}` or, on crash or a
   stashed hot-reload error, `{error, 500, ErrorPage}`.
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
    {halt, arizona_req:raw()}
    | {redirect, arizona_req:redirect_status(), binary()}
    | {ok, 200, iolist()}
    | {error, 500, iolist()}.

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
    case arizona_req:apply_middlewares(Middlewares, ArzReq, Bindings) of
        {halt, HaltReq} ->
            case arizona_req:halted_redirect(HaltReq) of
                {Status, Location} -> {redirect, Status, Location};
                undefined -> {halt, arizona_req:raw(HaltReq)}
            end;
        {cont, ArzReq1, Bindings1} ->
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
                Page = arizona_render:render_view_to_iolist(H, ArzReq, RenderOpts),
                {ok, 200, Page}
            catch
                Class:Reason:Stacktrace ->
                    ErrorInfo = #{
                        class => Class,
                        reason => Reason,
                        stacktrace => Stacktrace,
                        reload_url => reload_url()
                    },
                    {error, 500, render_error_body(Bindings, ErrorInfo)}
            end;
        #{errors := Errors} ->
            ErrorInfo = #{
                class => error,
                reason => {compile_error, Errors},
                stacktrace => [],
                reload_url => reload_url()
            },
            {error, 500, render_error_body(Bindings, ErrorInfo)}
    end.

render_error_body(Bindings, ErrorInfo) ->
    {Mod, Fun} = error_page(),
    Tmpl = Mod:Fun(Bindings#{error_info => ErrorInfo}),
    arizona_render:render_to_iolist(Tmpl).

error_page() ->
    persistent_term:get(arizona_error_page, {arizona_error_page, render}).
