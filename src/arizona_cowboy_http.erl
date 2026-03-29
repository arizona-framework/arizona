-module(arizona_cowboy_http).
-export([init/2]).
-ignore_xref([init/2]).

init(Req, #{handler := H} = State) ->
    Params = maps:from_list(cowboy_req:parse_qs(Req)),
    Bindings = maps:merge(maps:get(bindings, State, #{}), Params),
    Middlewares = maps:get(middlewares, State, []),
    case arizona_cowboy_req:apply_middlewares(Middlewares, Req, Bindings) of
        {halt, Req1} ->
            {ok, Req1, State};
        {cont, Req1, Bindings1} ->
            render_page(H, Req1, Bindings1, State)
    end.

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
        #{<<"content-type">> => <<"text/html">>},
        Body,
        Req
    ),
    {ok, Req2, State}.

error_page() ->
    persistent_term:get(arizona_error_page, {arizona_error_page, render}).

reload_url() ->
    persistent_term:get(arizona_reload_url, undefined).
