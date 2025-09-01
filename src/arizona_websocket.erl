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
-nominal call_result() :: {Commands :: cowboy_websocket:commands(), State :: state()}.

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-spec init(Req, State) -> {cowboy_websocket, Req, {ViewModule, MountArg, ArizonaReq}} when
    Req :: cowboy_req:req(),
    State :: undefined,
    ViewModule :: module(),
    MountArg :: dynamic(),
    ArizonaReq :: arizona_request:request().
init(CowboyRequest, undefined) ->
    % Extract path from query parameter ?path=/users
    PathParams = cowboy_req:parse_qs(CowboyRequest),
    {~"path", LivePath} = proplists:lookup(~"path", PathParams),
    {~"qs", Qs} = proplists:lookup(~"qs", PathParams),

    % Create request with the Live path to resolve correct handler
    LiveRequest = CowboyRequest#{path => LivePath, qs => Qs},
    {ViewModule, MountArg} = arizona_server:get_handler_opts(LiveRequest),

    % Create Arizona request with the original Live path
    ArizonaRequest = arizona_cowboy_request:new(LiveRequest),
    {cowboy_websocket, CowboyRequest, {ViewModule, MountArg, ArizonaRequest}}.

-spec websocket_init(InitData) -> Result when
    InitData :: {ViewModule, MountArg, ArizonaRequest},
    ViewModule :: module(),
    MountArg :: dynamic(),
    ArizonaRequest :: arizona_request:request(),
    Result :: call_result().
websocket_init({ViewModule, MountArg, ArizonaRequest}) ->
    {ok, LivePid} = arizona_live:start_link(ViewModule, MountArg, ArizonaRequest, self()),

    % Subscribe to live reload if arizona_reloader process is alive
    ok = maybe_join_live_reload(),

    HierarchicalStructure = arizona_live:initial_render(LivePid),
    View = arizona_live:get_view(LivePid),
    ViewState = arizona_view:get_state(View),
    ViewId = arizona_stateful:get_binding(id, ViewState),

    % Send initial hierarchical structure to client
    InitialPayload = json_encode(#{
        type => ~"initial_render",
        stateful_id => ViewId,
        structure => HierarchicalStructure
    }),

    State = #state{live_pid = LivePid},

    {[{text, InitialPayload}], State}.

-spec websocket_handle(Message, State) -> Result when
    Message :: {text, binary()},
    State :: state(),
    Result :: call_result().
websocket_handle({text, JSONBinary}, State) ->
    try
        Message = json:decode(JSONBinary),
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
websocket_info({reply_response, StatefulId, Diff, Reply}, State) ->
    handle_reply_response(StatefulId, Diff, Reply, State);
websocket_info({noreply_response, StatefulId, Diff}, State) ->
    handle_noreply_response(StatefulId, Diff, State);
% Handle live reload messages
websocket_info({pubsub_message, ~"live_reload", {file_changed, reload}}, State) ->
    Message = #{type => ~"reload"},
    ReloadPayload = json_encode(Message),
    {[{text, ReloadPayload}], State}.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

maybe_join_live_reload() ->
    case whereis(arizona_reloader) of
        undefined -> ok;
        _Pid -> arizona_pubsub:join(~"live_reload", self())
    end.

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
    ok = arizona_live:handle_event(LivePid, StatefulIdOrUndefined, Event, Params),
    {[], State}.

%% Handle reply response from Live
-spec handle_reply_response(StatefulId, Diff, Reply, State) -> Result when
    StatefulId :: arizona_stateful:id(),
    Diff :: arizona_differ:diff(),
    Reply :: term(),
    State :: state(),
    Result :: call_result().
handle_reply_response(StatefulId, Diff, Reply, #state{} = State) ->
    ReplyPayload = json_encode(#{
        type => ~"reply",
        data => Reply
    }),
    Cmds = [{text, ReplyPayload}],
    handle_diff_response(StatefulId, Diff, Cmds, State).

%% Handle noreply response from Live
-spec handle_noreply_response(StatefulId, Diff, State) -> Result when
    StatefulId :: arizona_stateful:id(),
    Diff :: arizona_differ:diff(),
    State :: state(),
    Result :: call_result().
handle_noreply_response(StatefulId, Diff, #state{} = State) ->
    handle_diff_response(StatefulId, Diff, [], State).

%% Handle noreply response from Live
-spec handle_diff_response(StatefulId, Diff, Cmds, State) -> Result when
    StatefulId :: arizona_stateful:id(),
    Diff :: arizona_differ:diff(),
    Cmds :: cowboy_websocket:commands(),
    State :: state(),
    Result :: call_result().
handle_diff_response(StatefulId, Diff, Cmds, #state{} = State) ->
    case Diff of
        [] ->
            {Cmds, State};
        _ ->
            DiffPayload = json_encode(#{
                type => ~"diff",
                stateful_id => StatefulId,
                changes => Diff
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

-spec json_encode(Term) -> JSONData when
    Term :: term(),
    JSONData :: iodata().
json_encode(Term) ->
    % Convert tuples to arrays for JavaScript compatibility
    json:encode(Term, fun json_encoder/2).

%% Custom JSON encoder that converts tuples to arrays for JavaScript compatibility
-spec json_encoder(Term, Encoder) -> JSONData when
    Term :: term(),
    Encoder :: json:encoder(),
    JSONData :: iodata().
json_encoder(Tuple, Encoder) when is_tuple(Tuple) ->
    json:encode_list(tuple_to_list(Tuple), Encoder);
json_encoder(Other, Encoder) ->
    json:encode_value(Other, Encoder).
