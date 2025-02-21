-module(arizona_websocket).
-behaviour(cowboy_websocket).

%% --------------------------------------------------------------------
%% Behaviour (cowboy_websocket) exports
%% --------------------------------------------------------------------

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).

%% --------------------------------------------------------------------
%% Libs
%% --------------------------------------------------------------------

-include_lib("kernel/include/logger.hrl").

%% --------------------------------------------------------------------
%% Types (and their exports)
%% --------------------------------------------------------------------

-opaque init_state() :: {
    HandlerState :: arizona_view_handler:state(),
    Params :: [{binary(), binary() | true}]
}.
-export_type([init_state/0]).

%% --------------------------------------------------------------------
%% Behaviour (cowboy_websocket) callbacks
%% --------------------------------------------------------------------

-spec init(Req0, []) -> Result when
    Req0 :: cowboy_req:req(),
    Result :: {cowboy_websocket, Req1, InitState},
    Req1 :: cowboy_req:req(),
    InitState :: init_state().
init(Req0, []) ->
    {ok, Req, Env} = arizona_server:req_route(Req0),
    InitState = init_state(Req, Env),
    {cowboy_websocket, Req, InitState}.

-spec websocket_init(InitState) -> {Events, Socket} when
    InitState :: init_state(),
    Events :: cowboy_websocket:commands(),
    Socket :: arizona_socket:socket().
websocket_init({{Mod, Assigns, _Opts}, Params}) ->
    ?LOG_INFO(#{
        text => ~"init",
        in => ?MODULE,
        view_module => Mod,
        assigns => Assigns,
        params => Params
    }),
    Socket0 = arizona_socket:new(render),
    {ok, View0} = arizona_view:mount(Mod, Assigns, Socket0),
    Token = arizona_view:render(View0),
    {View, Socket} = arizona_render:render(Token, View0, View0, Socket0),
    Html = arizona_view:rendered_to_iolist(View),
    Msg = json:encode([[~"init", Html]]),
    Events = [{text, Msg}],
    {Events, Socket}.

-spec websocket_handle(Event, Socket0) -> {Events, Socket1} when
    Event :: {text, binary()},
    Events :: cowboy_websocket:commands(),
    Socket0 :: arizona_socket:socket(),
    Socket1 :: arizona_socket:socket().
websocket_handle({text, Msg}, Socket) ->
    ?LOG_INFO(#{
        text => ~"message received",
        in => ?MODULE,
        messge => Msg,
        socket => Socket
    }),
    Events = [],
    {Events, Socket}.

-spec websocket_info(Msg, Socket0) -> {Events, Socket1} when
    Msg :: dynamic(),
    Events :: cowboy_websocket:commands(),
    Socket0 :: arizona_socket:socket(),
    Socket1 :: arizona_socket:socket().
websocket_info(Msg, Socket) ->
    ?LOG_INFO(#{
        text => ~"info received",
        in => ?MODULE,
        messge => Msg,
        socket => Socket
    }),
    {[], Socket}.

-spec terminate(Reason, Req, Socket) -> ok when
    Reason :: term(),
    Req :: cowboy_req:req(),
    Socket :: arizona_socket:socket().
terminate(Reason, _Req, Socket) ->
    ?LOG_INFO(#{
        text => ~"terminated",
        in => ?MODULE,
        reason => Reason,
        socket => Socket
    }),
    ok.

%% --------------------------------------------------------------------
%% Private functions
%% --------------------------------------------------------------------

init_state(Req, Env) ->
    HandlerState = maps:get(handler_opts, Env),
    Params = cowboy_req:parse_qs(Req),
    {HandlerState, Params}.
