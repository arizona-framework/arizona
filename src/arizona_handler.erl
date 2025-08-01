-module(arizona_handler).
-behaviour(cowboy_handler).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([init/2]).

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-spec init(CowboyRequest, State) -> {ok, CowboyRequest1, State} when
    CowboyRequest :: cowboy_req:req(),
    State :: map(),
    CowboyRequest1 :: cowboy_req:req().
init(CowboyRequest, State) when is_map(State) ->
    Handler = maps:get(handler, State),
    handle_live_request(Handler, CowboyRequest, State).

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% Handle LiveView requests with complete lifecycle management
-spec handle_live_request(ViewModule, CowboyRequest, State) -> {ok, CowboyRequest1, State} when
    ViewModule :: module(),
    CowboyRequest :: cowboy_req:req(),
    State :: map(),
    CowboyRequest1 :: cowboy_req:req().
handle_live_request(ViewModule, CowboyRequest, State) ->
    try
        ArizonaRequest = arizona_cowboy_request:new(CowboyRequest),
        ViewState = arizona_view:call_mount_callback(ViewModule, ArizonaRequest),
        Bindings = arizona_stateful:get_bindings(ViewState),
        View = arizona_view:new(ViewState, render, undefined),
        {Html, _RenderView} = arizona_renderer:render_stateful(ViewModule, Bindings, View),
        CowboyRequest1 = cowboy_req:reply(
            200, #{~"content-type" => ~"text/html"}, Html, CowboyRequest
        ),
        {ok, CowboyRequest1, State}
    catch
        Error:Reason:Stacktrace ->
            ErrorMsg = io_lib:format("LiveView Error: ~p:~p~nStacktrace: ~p", [
                Error, Reason, Stacktrace
            ]),
            CowboyRequest2 = cowboy_req:reply(500, #{}, iolist_to_binary(ErrorMsg), CowboyRequest),
            {ok, CowboyRequest2, State}
    end.
