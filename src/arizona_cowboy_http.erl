-module(arizona_cowboy_http).
-moduledoc """
Cowboy HTTP handler that runs an Arizona handler for the initial
server-side render.

Wired up by `arizona_cowboy_router` for every Arizona route. The
handler:

1. Merges path bindings, query params, and any static `bindings` from
   the route opts into a single `Bindings` map
2. Runs any configured middlewares (`arizona_cowboy_req:apply_middlewares/3`)
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
    PathBindings = cowboy_req:bindings(Req),
    QueryParams = maps:from_list(cowboy_req:parse_qs(Req)),
    Bindings = maps:merge(maps:merge(maps:get(bindings, State, #{}), PathBindings), QueryParams),
    Middlewares = maps:get(middlewares, State, []),
    case arizona_cowboy_req:apply_middlewares(Middlewares, Req, Bindings) of
        {halt, Req1} ->
            {ok, Req1, State};
        {cont, Req1, Bindings1} ->
            render_page(H, Req1, Bindings1, State)
    end.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

render_page(H, Req, Bindings, State) ->
    case arizona_reloader:get_error() of
        undefined ->
            RenderOpts = #{
                bindings => Bindings,
                layout => maps:get(layout, State, undefined),
                on_mount => maps:get(on_mount, State, [])
            },
            try arizona_render:render_to_iolist(H, RenderOpts) of
                Page ->
                    reply(200, Page, Req, State)
            catch
                Class:Reason:Stacktrace ->
                    ErrorInfo = #{
                        class => Class,
                        reason => Reason,
                        stacktrace => Stacktrace,
                        reload_url => reload_url()
                    },
                    reply_error(Bindings, ErrorInfo, Req, State)
            end;
        #{errors := Errors} ->
            ErrorInfo = #{
                class => error,
                reason => {compile_error, Errors},
                stacktrace => [],
                reload_url => reload_url()
            },
            reply_error(Bindings, ErrorInfo, Req, State)
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
