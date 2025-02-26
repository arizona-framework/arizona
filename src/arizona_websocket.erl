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
    PathParams :: path_params(),
    QueryString :: query_string(),
    HandlerState :: arizona_view_handler:state()
}.
-export_type([init_state/0]).

-type path_params() :: cowboy_router:bindings().
-export_type([path_params/0]).

-type query_string() :: binary().
-export_type([query_string/0]).

-type query_params() :: [{binary(), binary() | true}].
-export_type([query_params/0]).

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
    HandlerState = maps:get(handler_opts, Env),
    PathParams = cowboy_req:bindings(Req),
    QueryString = cowboy_req:qs(Req),
    {cowboy_websocket, Req, {PathParams, QueryString, HandlerState}}.

-spec websocket_init(InitState) -> {Events, Socket} when
    InitState :: init_state(),
    Events :: cowboy_websocket:commands(),
    Socket :: arizona_socket:socket().
websocket_init({PathParams, QueryString, {Mod, Bindings, _Opts}}) ->
    ?LOG_INFO(#{
        text => ~"init",
        in => ?MODULE,
        view_module => Mod,
        bindings => Bindings,
        path_params => PathParams,
        query_string => QueryString
    }),
    Socket0 = arizona_socket:new(render),
    {_View, Socket1} = arizona_view:init_root(Mod, PathParams, QueryString, Bindings, Socket0),
    Socket = arizona_socket:set_render_context(diff, Socket1),
    Events = put_init_event(Socket, []),
    {Events, Socket}.

-spec websocket_handle(Event, Socket0) -> {Events, Socket1} when
    Event :: {text, binary()},
    Events :: cowboy_websocket:commands(),
    Socket0 :: arizona_socket:socket(),
    Socket1 :: arizona_socket:socket().
websocket_handle({text, Msg}, Socket0) ->
    ?LOG_INFO(#{
        text => ~"message received",
        in => ?MODULE,
        messge => Msg,
        socket => Socket0
    }),
    [ViewId, Event, Payload] = json:decode(Msg),
    {ok, View0} = arizona_socket:get_view(ViewId, Socket0),
    View1 = arizona_view:handle_event(Event, Payload, View0),
    ?LOG_INFO(#{
        text => ~"view updated",
        in => ?MODULE,
        id => ViewId,
        views => View1
    }),
    Token = arizona_view:render(View1),
    {View2, Socket1} = arizona_diff:diff(Token, 0, View1, Socket0),
    Diff = arizona_view:diff(View2),
    Events = put_diff_event(Diff, ViewId, []),
    View = arizona_view:merge_changed_bindings(View2),
    Socket = arizona_socket:put_view(View, Socket1),
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

put_init_event(Socket, Events) ->
    Views = #{Id => arizona_view:rendered(View) || Id := View <- arizona_socket:views(Socket)},
    Msg = json:encode([[~"init", Views]]),
    [{text, Msg} | Events].

put_diff_event([], _ViewId, Events) ->
    Events;
put_diff_event(Diff, ViewId, Events) ->
    ?LOG_INFO(#{
        text => ~"view diff",
        in => ?MODULE,
        id => ViewId,
        views => Diff
    }),
    Msg = encode_diff([[~"patch", [ViewId, Diff]]]),
    [{text, Msg} | Events].

encode_diff(Diff) ->
    json:encode(Diff, fun
        ([{_, _} | _] = Proplist, Encode) ->
            json:encode(proplists:to_map(Proplist), Encode);
        (Other, Encode) ->
            json:encode_value(Other, Encode)
    end).
