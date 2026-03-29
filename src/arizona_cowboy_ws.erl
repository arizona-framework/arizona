-module(arizona_cowboy_ws).
-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2]).
-ignore_xref([init/2, websocket_init/1, websocket_handle/2, websocket_info/2]).

init(Req, Opts) ->
    QS = cowboy_req:parse_qs(Req),
    Path = proplists:get_value(<<"path">>, QS, <<"/">>),
    Reconnect = proplists:get_value(<<"reconnect">>, QS, <<"0">>) =:= <<"1">>,
    Params =
        case proplists:get_value(<<"params">>, QS) of
            undefined -> #{};
            JSON -> json:decode(JSON)
        end,
    LiveReq = Req#{path => Path},
    {_CowboyHandler, RouteOpts} = arizona_cowboy_adapter:resolve_cowboy_route(LiveReq),
    H = maps:get(handler, RouteOpts),
    IB = maps:merge(maps:get(bindings, RouteOpts, #{}), Params),
    OnMount = maps:get(on_mount, RouteOpts, []),
    Middlewares = maps:get(middlewares, RouteOpts, []),
    case arizona_cowboy_req:apply_middlewares(Middlewares, Req, IB) of
        {halt, Req1} ->
            {ok, Req1, #{}};
        {cont, _Req1, Bindings1} ->
            State = #{
                handler => H,
                bindings => Bindings1,
                on_mount => OnMount,
                req => Req,
                reconnect => Reconnect
            },
            {cowboy_websocket, Req, State, Opts}
    end.

websocket_init(#{handler := H, bindings := IB, on_mount := OM, req := Req, reconnect := R}) ->
    Opts = #{
        reconnect => R, on_mount => OM, adapter => arizona_cowboy_adapter, adapter_state => Req
    },
    to_cowboy(arizona_socket:init(H, IB, Opts)).

websocket_handle({text, Data}, #{socket := Sock}) ->
    to_cowboy(arizona_socket:handle_in(Data, Sock));
websocket_handle(_Frame, State) ->
    {ok, State}.

websocket_info(Msg, #{socket := Sock}) ->
    to_cowboy(arizona_socket:handle_info(Msg, Sock)).

to_cowboy({ok, Sock}) ->
    {ok, #{socket => Sock}};
to_cowboy({reply, Data, Sock}) ->
    {reply, {text, Data}, #{socket => Sock}};
to_cowboy({close, Code, Reason, _Sock}) ->
    {reply, {close, Code, Reason}, #{}}.
