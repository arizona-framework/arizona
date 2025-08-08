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
        View = arizona_view:call_mount_callback(ViewModule, ArizonaRequest),
        {Html, _RenderView} = render_view(View),
        CowboyRequest1 = cowboy_req:reply(
            200, #{~"content-type" => ~"text/html; charset=utf-8"}, Html, CowboyRequest
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

render_view(View) ->
    case arizona_view:get_layout(View) of
        {LayoutModule, LayoutRenderFun, SlotName, SlotBindings} ->
            Slot = view,
            LayoutBindings = arizona_binder:put(SlotName, Slot, SlotBindings),
            State = arizona_view:get_state(View),
            Id = arizona_stateful:get_binding(id, State),
            arizona_renderer:render_stateless(
                LayoutModule, LayoutRenderFun, LayoutBindings, Id, View
            );
        none ->
            arizona_renderer:render_view(View)
    end.
