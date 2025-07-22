-module(arizona_handler).
-behaviour(cowboy_handler).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([init/2]).

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-spec init(Req, State) -> {ok, Req1, State} when
    Req :: cowboy_req:req(),
    State :: map(),
    Req1 :: cowboy_req:req().
init(Req, State) when is_map(State) ->
    Handler = maps:get(handler, State),
    handle_live_request(Handler, Req, State).

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% Handle LiveView requests with complete lifecycle management
-spec handle_live_request(ViewModule, Req, State) -> {ok, Req1, State} when
    ViewModule :: module(),
    Req :: cowboy_req:req(),
    State :: map(),
    Req1 :: cowboy_req:req().
handle_live_request(ViewModule, Req, State) ->
    try
        ArizonaReq = arizona_request:from_cowboy(Req),
        ViewState = arizona_view:call_mount_callback(ViewModule, ArizonaReq),
        Bindings = arizona_stateful:get_bindings(ViewState),
        View = arizona_view:new(ViewModule, ViewState, render, undefined),
        {Html, _RenderView} = arizona_renderer:render_stateful(ViewModule, Bindings, View),
        Req1 = cowboy_req:reply(200, #{~"content-type" => ~"text/html"}, Html, Req),
        {ok, Req1, State}
    catch
        Error:Reason:Stacktrace ->
            ErrorMsg = io_lib:format("LiveView Error: ~p:~p~nStacktrace: ~p", [
                Error, Reason, Stacktrace
            ]),
            Req2 = cowboy_req:reply(500, #{}, iolist_to_binary(ErrorMsg), Req),
            {ok, Req2, State}
    end.
