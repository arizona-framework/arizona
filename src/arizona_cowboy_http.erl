-module(arizona_cowboy_http).
-moduledoc """
Cowboy HTTP handler that runs an Arizona handler for the initial
server-side render.

Wired up by `arizona_cowboy_router` for every Arizona route. The
handler:

1. Wraps the cowboy request in an `arizona_req:request()` and takes
   the initial bindings from the route's `bindings` option. URL-derived
   data (path bindings, query params) is NOT flat-merged -- handlers
   reach it through `arizona_req` accessors or a middleware that
   projects what it wants into the bindings.
2. Runs any configured middlewares (`arizona_req:apply_middlewares/3`)
3. Calls `arizona_render:render_to_iolist/2` to produce the page HTML
4. On crash, renders the dev error page (or the configured override)
   with status 500

If `arizona_reloader` has a stashed compile error from a failed hot
reload, the error page is rendered immediately without invoking the
handler.
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([init/2]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

-ignore_xref([init/2]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([error_info/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-nominal error_info() :: #{
    class := error | exit | throw,
    reason := term(),
    stacktrace := erlang:stacktrace(),
    reload_url => binary() | undefined
}.

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc """
Cowboy `init/2` callback. Renders the configured Arizona handler and
replies with HTML.
""".
-spec init(Req, State) -> {ok, Req1, State} when
    Req :: cowboy_req:req(),
    State :: map(),
    Req1 :: cowboy_req:req().
init(Req, #{handler := H} = State) ->
    Bindings = maps:get(bindings, State, #{}),
    Middlewares = maps:get(middlewares, State, []),
    ArzReq = arizona_cowboy_req:new(Req),
    case arizona_req:apply_middlewares(Middlewares, ArzReq, Bindings) of
        {halt, HaltReq} ->
            {ok, arizona_req:raw(HaltReq), State};
        {cont, ArzReq1, Bindings1} ->
            render_page(H, ArzReq1, Bindings1, State)
    end.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

render_page(H, ArzReq, Bindings, State) ->
    case arizona_reloader:get_error() of
        undefined ->
            try
                RenderOpts = #{
                    bindings => Bindings,
                    layout => maps:get(layout, State, undefined),
                    on_mount => maps:get(on_mount, State, [])
                },
                Page = arizona_render:render_view_to_iolist(H, ArzReq, RenderOpts),
                reply(200, Page, arizona_req:raw(ArzReq), State)
            catch
                Class:Reason:Stacktrace ->
                    ErrorInfo = #{
                        class => Class,
                        reason => Reason,
                        stacktrace => Stacktrace,
                        reload_url => reload_url()
                    },
                    reply_error(Bindings, ErrorInfo, arizona_req:raw(ArzReq), State)
            end;
        #{errors := Errors} ->
            ErrorInfo = #{
                class => error,
                reason => {compile_error, Errors},
                stacktrace => [],
                reload_url => reload_url()
            },
            reply_error(Bindings, ErrorInfo, arizona_req:raw(ArzReq), State)
    end.

reply_error(Bindings, ErrorInfo, Req, State) ->
    {Mod, Fun} = error_page(),
    Tmpl = Mod:Fun(Bindings#{error_info => ErrorInfo}),
    reply(500, arizona_render:render_to_iolist(Tmpl), Req, State).

reply(Status, Body, Req, State) ->
    Req2 = cowboy_req:reply(
        Status,
        #{~"content-type" => ~"text/html"},
        Body,
        Req
    ),
    {ok, Req2, State}.

error_page() ->
    persistent_term:get(arizona_error_page, {arizona_error_page, render}).

reload_url() ->
    persistent_term:get(arizona_reload_url, undefined).
