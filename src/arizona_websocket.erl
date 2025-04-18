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
    Socket0 = arizona_socket:new(render, self()),
    {_View, Socket1} = arizona_view:init_root(Mod, PathParams, QueryString, Bindings, Socket0),
    Socket = arizona_socket:set_render_context(diff, Socket1),
    Events = put_init_event(Socket, []),
    Cmds = commands(Events),
    {Cmds, Socket}.

-spec websocket_handle(Event, Socket0) -> {Events, Socket1} when
    Event :: {text, Msg},
    Msg :: binary(),
    Socket0 :: arizona_socket:socket(),
    Events :: cowboy_websocket:commands(),
    Socket1 :: arizona_socket:socket().
websocket_handle({text, Msg}, Socket) ->
    ?LOG_INFO(#{
        text => ~"message received",
        in => ?MODULE,
        messge => Msg,
        socket => Socket
    }),
    [Subject, Attachment] = json:decode(Msg),
    handle_message(Subject, Attachment, Socket).

-spec websocket_info(Msg, Socket0) -> {Events, Socket1} when
    Msg :: dynamic(),
    Events :: cowboy_websocket:commands(),
    Socket0 :: arizona_socket:socket(),
    Socket1 :: arizona_socket:socket().
websocket_info({broadcast, From, ViewId, EventName, Payload}, Socket0) ->
    ?LOG_INFO(#{
        text => ~"broadcast message received",
        in => ?MODULE,
        from => From,
        view_id => ViewId,
        event_name => EventName,
        payload => Payload,
        socket => Socket0
    }),
    {ok, View0} = arizona_socket:get_view(ViewId, Socket0),
    Ref = undefined,
    {Events0, View1} = handle_event(Ref, ViewId, EventName, Payload, From, View0),
    {Events, Socket} = handle_diff(Ref, ViewId, Events0, View1, Socket0),
    Cmds = commands(Events),
    {Cmds, Socket};
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

handle_message(~"event", [Ref, ViewId, EventName, Payload], Socket0) ->
    {ok, View0} = arizona_socket:get_view(ViewId, Socket0),
    {Events0, View1} = handle_event(Ref, ViewId, EventName, Payload, self(), View0),
    {Events, Socket} = handle_diff(Ref, ViewId, Events0, View1, Socket0),
    Cmds = commands(Events),
    {Cmds, Socket};
handle_message(~"join", [Ref, ViewId, EventName, Payload], Socket0) ->
    {ok, View0} = arizona_socket:get_view(ViewId, Socket0),
    {Events0, View} = handle_join(Ref, ViewId, EventName, Payload, View0),
    {Events, Socket} = handle_diff(Ref, ViewId, Events0, View, Socket0),
    Cmds = commands(Events),
    {Cmds, Socket}.

handle_event(Ref, ViewId, EventName, Payload, From, View) ->
    Result = arizona_view:handle_event(EventName, Payload, From, View),
    ?LOG_INFO(#{
        text => ~"handle_event result",
        in => ?MODULE,
        result => Result,
        view_id => ViewId,
        view => View
    }),
    norm_handle_event_result(Result, Ref, ViewId, EventName).

norm_handle_event_result({noreply, View}, _Ref, _ViewId, _EventName) ->
    {[], View};
norm_handle_event_result({reply, Payload, View}, Ref, ViewId, EventName) ->
    Event = event_tuple(EventName, Ref, ViewId, Payload),
    {[Event], View}.

handle_diff(Ref, ViewId, Events0, View0, Socket0) ->
    Token = arizona_view:render(View0),
    {View2, Socket1} = arizona_diff:diff(Token, 0, View0, Socket0),
    Diff = arizona_view:diff(View2),
    Events = put_diff_event(Diff, Ref, ViewId, Events0),
    View = arizona_view:merge_changed_bindings(View2),
    Socket = arizona_socket:put_view(View, Socket1),
    {Events, Socket}.

put_init_event(Socket, Events) ->
    Views = #{Id => arizona_view:rendered(View) || Id := View <- arizona_socket:views(Socket)},
    Event = event_tuple(~"init", undefined, undefined, Views),
    [Event | Events].

put_diff_event([], _Ref, _ViewId, Events) ->
    Events;
put_diff_event(Diff, Ref, ViewId, Events) ->
    ?LOG_INFO(#{
        text => ~"view diff",
        in => ?MODULE,
        diff => Diff,
        ref => Ref,
        id => ViewId,
        views => Diff
    }),
    [event_tuple(~"patch", Ref, ViewId, {diff, Diff}) | Events].

handle_join(Ref, ViewId, EventName, Payload, View) ->
    Mod = arizona_view:module(View),
    Result = arizona_view:handle_join(Mod, EventName, Payload, View),
    ?LOG_INFO(#{
        text => ~"handle_join result",
        in => ?MODULE,
        ref => Ref,
        view_id => ViewId,
        event_name => EventName,
        payload => Payload,
        result => Result,
        view => View
    }),
    norm_handle_join_result(Result, Ref, ViewId, EventName).

norm_handle_join_result({ok, Payload, View}, Ref, ViewId, EventName) ->
    ok = arizona_pubsub:subscribe(EventName, self()),
    ?LOG_INFO(#{
        text => ~"joined",
        in => ?MODULE,
        ref => Ref,
        view_id => ViewId,
        event_name => EventName,
        payload => Payload,
        view => View,
        sender => self()
    }),
    Event = event_tuple(~"join", Ref, ViewId, [~"ok", Payload]),
    {[Event], View};
norm_handle_join_result({error, Reason, View}, Ref, ViewId, _EventName) ->
    Event = event_tuple(~"join", Ref, ViewId, [~"error", Reason]),
    {[Event], View}.

event_tuple(EventName, Ref, ViewId, Payload) ->
    {EventName, [Ref, ViewId, Payload]}.

commands([]) ->
    [];
commands(Events) ->
    [{text, encode(lists:reverse(Events))}].

encode(Term) ->
    json:encode(Term, fun
        ({diff, Diff}, Encode) ->
            json:encode(proplists:to_map(Diff), Encode);
        ([{_, _} | _] = Proplist, Encode) ->
            json:encode(proplist_to_list(Proplist), Encode);
        (Other, Encode) ->
            json:encode_value(Other, Encode)
    end).

proplist_to_list([]) ->
    [];
proplist_to_list([{K, V} | T]) ->
    [[K, V] | proplist_to_list(T)].
