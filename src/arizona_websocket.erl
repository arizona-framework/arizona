-module(arizona_websocket).
-behaviour(cowboy_websocket).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([state/0]).
-export_type([call_result/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-record(state, {
    live_pid :: pid()
}).

-opaque state() :: #state{}.
-type call_result() :: {Commands :: cowboy_websocket:commands(), State :: state()}.

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-spec init(Req, State) -> {cowboy_websocket, Req, {LiveModule, ArizonaReq}} when
    Req :: cowboy_req:req(),
    State :: map(),
    LiveModule :: module(),
    ArizonaReq :: arizona_request:request().
init(Req, _State) ->
    % Extract path from query parameter ?path=/users
    PathParams = cowboy_req:parse_qs(Req),
    LivePath = proplists:get_value(~"path", PathParams, ~"/"),

    % Create fake request with the Live path to resolve correct handler
    FakeReq = Req#{path => LivePath},
    RouteMetadata = arizona_server:get_route_metadata(FakeReq),
    LiveModule = arizona_server:get_route_handler(RouteMetadata),

    % Create Arizona request with the original Live path
    ArizonaReq = arizona_request:from_cowboy(FakeReq),
    {cowboy_websocket, Req, {LiveModule, ArizonaReq}}.

-spec websocket_init(InitData) -> Result when
    InitData :: {LiveModule, ArizonaReq},
    LiveModule :: module(),
    ArizonaReq :: arizona_request:request(),
    Result :: call_result().
websocket_init({ViewModule, ArizonaReq}) ->
    {ok, LivePid} = arizona_live:start_link(),
    ViewState = arizona_live:mount_view(LivePid, ViewModule, ArizonaReq),
    View = arizona_view:new(ViewModule, ViewState, hierarchical, LivePid),
    ok = arizona_live:set_view(LivePid, View),
    HierarchicalStructure = arizona_live:initial_render(LivePid, ViewState),

    % Send initial hierarchical structure to client
    InitialPayload = json_encode(#{
        type => ~"initial_render",
        structure => HierarchicalStructure
    }),

    State = #state{live_pid = LivePid},

    {[{text, InitialPayload}], State}.

-spec websocket_handle(Message, State) -> Result when
    Message :: {text, binary()},
    State :: state(),
    Result :: call_result().
websocket_handle({text, JsonBinary}, State) ->
    try
        Message = json:decode(JsonBinary),
        MessageType = maps:get(~"type", Message, undefined),
        handle_message_type(MessageType, Message, State)
    catch
        Error:Reason:Stacktrace ->
            handle_websocket_error(Error, Reason, Stacktrace, State)
    end.

-spec websocket_info(Info, State) -> Result when
    Info :: term(),
    State :: state(),
    Result :: call_result().
websocket_info(_Info, State) ->
    % TODO: Handle real-time updates (push notifications, live data feeds, etc.)
    {[], State}.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% Handle different message types
-spec handle_message_type(MessageType, Message, State) -> Result when
    MessageType :: binary() | undefined,
    Message :: map(),
    State :: state(),
    Result :: call_result().
handle_message_type(~"event", Message, State) ->
    handle_event_message(Message, State);
handle_message_type(~"ping", _Message, State) ->
    handle_ping_message(State);
handle_message_type(_UnknownType, _Message, State) ->
    handle_unknown_message(State).

%% Handle DOM event message
-spec handle_event_message(Message, State) -> Result when
    Message :: map(),
    State :: state(),
    Result :: call_result().
handle_event_message(Message, #state{} = State) ->
    LivePid = State#state.live_pid,
    StatefulIdOrUndefined = maps:get(~"stateful_id", Message, undefined),
    Event = maps:get(~"event", Message),
    Params = maps:get(~"params", Message, #{}),
    case arizona_live:handle_event(LivePid, StatefulIdOrUndefined, Event, Params) of
        {reply, StatefulId, Reply} ->
            handle_reply_response(StatefulId, Reply, State);
        {noreply, StatefulId} ->
            handle_noreply_response(StatefulId, State)
    end.

%% Handle reply response from Live
-spec handle_reply_response(StatefulId, Reply, State) -> Result when
    StatefulId :: arizona_stateful:id(),
    Reply :: term(),
    State :: state(),
    Result :: call_result().
handle_reply_response(StatefulId, Reply, #state{} = State) ->
    ReplyPayload = json_encode(#{
        type => ~"reply",
        data => Reply
    }),
    Cmds = [{text, ReplyPayload}],
    handle_diff_response(StatefulId, Cmds, State).

%% Handle noreply response from Live
-spec handle_noreply_response(StatefulId, State) -> Result when
    StatefulId :: arizona_stateful:id(),
    State :: state(),
    Result :: call_result().
handle_noreply_response(StatefulId, #state{} = State) ->
    handle_diff_response(StatefulId, [], State).

%% Handle noreply response from Live
-spec handle_diff_response(StatefulId, Cmds, State) -> Result when
    StatefulId :: arizona_stateful:id(),
    Cmds :: cowboy_websocket:commands(),
    State :: state(),
    Result :: call_result().
handle_diff_response(StatefulId, Cmds, #state{} = State) ->
    LivePid = State#state.live_pid,
    case arizona_live:diff_stateful(LivePid, StatefulId) of
        [] ->
            {Cmds, State};
        DiffChanges ->
            DiffPayload = json_encode(#{
                type => ~"diff",
                changes => DiffChanges
            }),
            {[{text, DiffPayload} | Cmds], State}
    end.

%% Handle ping message
-spec handle_ping_message(State) -> Result when
    State :: state(),
    Result :: call_result().
handle_ping_message(State) ->
    PongPayload = json_encode(#{type => ~"pong"}),
    {[{text, PongPayload}], State}.

%% Handle unknown message type
-spec handle_unknown_message(State) -> Result when
    State :: state(),
    Result :: call_result().
handle_unknown_message(State) ->
    ErrorPayload = json_encode(#{
        type => ~"error",
        message => ~"Unknown message type"
    }),
    {[{text, ErrorPayload}], State}.

%% Handle WebSocket errors
-spec handle_websocket_error(Error, Reason, Stacktrace, State) -> Result when
    Error :: term(),
    Reason :: term(),
    Stacktrace :: list(),
    State :: state(),
    Result :: call_result().
handle_websocket_error(Error, Reason, Stacktrace, State) ->
    logger:error("WebSocket message handling error: ~p:~p~nStacktrace: ~p", [
        Error, Reason, Stacktrace
    ]),
    ErrorPayload = json_encode(#{
        type => ~"error",
        message => ~"Internal server error"
    }),
    {[{text, ErrorPayload}], State}.

-spec json_encode(Term) -> JsonData when
    Term :: term(),
    JsonData :: iodata().
json_encode(Term) ->
    % Convert tuples to arrays for JavaScript compatibility
    json:encode(Term, fun json_encoder/2).

%% Custom JSON encoder that converts tuples to arrays for JavaScript compatibility
-spec json_encoder(Term, Encoder) -> JsonData when
    Term :: term(),
    Encoder :: json:encoder(),
    JsonData :: iodata().
json_encoder(Tuple, Encoder) when is_tuple(Tuple) ->
    json:encode_list(tuple_to_list(Tuple), Encoder);
json_encoder(Other, Encoder) ->
    json:encode_value(Other, Encoder).
