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
    State :: {ViewModule, MountArg},
    ViewModule :: module(),
    MountArg :: dynamic(),
    CowboyRequest1 :: cowboy_req:req().
init(CowboyRequest, State) ->
    try
        {ViewModule, MountArg} = State,
        ArizonaRequest = arizona_cowboy_request:new(CowboyRequest),
        View = arizona_view:call_mount_callback(ViewModule, MountArg, ArizonaRequest),
        {Html, _RenderView} = arizona_renderer:render_layout(View),
        CowboyRequest1 = cowboy_req:reply(
            200, #{~"content-type" => ~"text/html; charset=utf-8"}, Html, CowboyRequest
        ),
        {ok, CowboyRequest1, State}
    catch
        Error:Reason:Stacktrace ->
            ErrorMsg = io_lib:format("Error: ~p:~p~nStacktrace: ~p", [
                Error, Reason, Stacktrace
            ]),
            CowboyRequest2 = cowboy_req:reply(500, #{}, iolist_to_binary(ErrorMsg), CowboyRequest),
            {ok, CowboyRequest2, State}
    end.
