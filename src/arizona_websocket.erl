-module(arizona_websocket).
-behaviour(cowboy_websocket).

%% API
-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2]).

%% Testing helpers
-export([new_state/1, get_live_pid/1]).

%% Types
-record(state, {
    live_pid :: pid()
}).

-opaque state() :: #state{}.
-export_type([state/0]).

-type call_result() :: {Commands :: cowboy_websocket:commands(), State :: state()}.

%% Cowboy WebSocket callbacks

%% @doc Initialize WebSocket connection
-spec init(cowboy_req:req(), map()) -> {cowboy_websocket, cowboy_req:req(), term()}.
init(Req, _State) ->
    % Extract path from query parameter ?path=/users
    PathParams = cowboy_req:parse_qs(Req),
    LivePath = proplists:get_value(~"path", PathParams, ~"/"),

    % Create fake request with the LiveView path to resolve correct handler
    FakeReq = Req#{path => LivePath},
    RouteMetadata = arizona_server:get_route_metadata(FakeReq),
    LiveModule = arizona_server:get_route_handler(RouteMetadata),

    % Create Arizona request with the original LiveView path
    ArizonaReq = arizona_request:from_cowboy(FakeReq),
    {cowboy_websocket, Req, {LiveModule, ArizonaReq}}.

%% @doc WebSocket connection established - start LiveView process
-spec websocket_init({LiveModule, Req}) -> Result when
    LiveModule :: module(),
    Req :: arizona_request:request(),
    Result :: call_result().
websocket_init({LiveModule, Req}) ->
    % Create initial Arizona socket
    Socket = arizona_socket:new(#{}),

    % Start the LiveView process
    {ok, LivePid} = arizona_live:start_link(LiveModule, Socket),

    % Mount the LiveView
    _MountedSocket = arizona_live:mount(LivePid, Req),

    % Initial render
    RenderedSocket = arizona_live:render(LivePid),
    Html = arizona_socket:get_html(RenderedSocket),

    % Send initial HTML to client
    InitialPayload = json:encode(#{
        type => ~"initial_render",
        html => Html
    }),

    State = #state{live_pid = LivePid},

    {[{text, InitialPayload}], State}.

%% @doc Handle incoming WebSocket messages from client
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

%% Private functions

%% @doc Handle different message types
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

%% @doc Handle DOM event message
-spec handle_event_message(Message, State) -> Result when
    Message :: map(),
    State :: state(),
    Result :: call_result().
handle_event_message(Message, #state{live_pid = LivePid} = State) ->
    Event = maps:get(~"event", Message),
    Params = maps:get(~"params", Message, #{}),

    case arizona_live:handle_event(LivePid, Event, Params) of
        {noreply, UpdatedSocket} ->
            handle_noreply_response(UpdatedSocket, State);
        {reply, Reply, UpdatedSocket} ->
            handle_reply_response(Reply, UpdatedSocket, State)
    end.

%% @doc Handle noreply response from LiveView
-spec handle_noreply_response(Socket, State) -> Result when
    Socket :: arizona_socket:socket(),
    State :: state(),
    Result :: call_result().
handle_noreply_response(UpdatedSocket, State) ->
    Changes = arizona_socket:get_changes(UpdatedSocket),
    case Changes of
        [] ->
            {[], State};
        _ ->
            DiffPayload = json:encode(#{
                type => ~"diff",
                changes => Changes
            }),
            arizona_socket:clear_changes(UpdatedSocket),
            {[{text, DiffPayload}], State}
    end.

%% @doc Handle reply response from LiveView
-spec handle_reply_response(Reply, Socket, State) -> Result when
    Reply :: term(),
    Socket :: arizona_socket:socket(),
    State :: state(),
    Result :: call_result().
handle_reply_response(Reply, UpdatedSocket, State) ->
    ReplyPayload = json:encode(#{
        type => ~"reply",
        data => Reply
    }),

    Changes = arizona_socket:get_changes(UpdatedSocket),
    case Changes of
        [] ->
            {[{text, ReplyPayload}], State};
        _ ->
            DiffPayload = json:encode(#{
                type => ~"diff",
                changes => Changes
            }),
            arizona_socket:clear_changes(UpdatedSocket),
            {[{text, ReplyPayload}, {text, DiffPayload}], State}
    end.

%% @doc Handle ping message
-spec handle_ping_message(State) -> Result when
    State :: state(),
    Result :: call_result().
handle_ping_message(State) ->
    PongPayload = json:encode(#{type => ~"pong"}),
    {[{text, PongPayload}], State}.

%% @doc Handle unknown message type
-spec handle_unknown_message(State) -> Result when
    State :: state(),
    Result :: call_result().
handle_unknown_message(State) ->
    ErrorPayload = json:encode(#{
        type => ~"error",
        message => ~"Unknown message type"
    }),
    {[{text, ErrorPayload}], State}.

%% @doc Handle WebSocket errors
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
    ErrorPayload = json:encode(#{
        type => ~"error",
        message => ~"Internal server error"
    }),
    {[{text, ErrorPayload}], State}.

%% Testing helpers

%% @doc Create a new state for testing
-spec new_state(pid()) -> state().
new_state(LivePid) ->
    #state{live_pid = LivePid}.

%% @doc Get live_pid from state for testing
-spec get_live_pid(state()) -> pid().
get_live_pid(#state{live_pid = LivePid}) ->
    LivePid.

%% @doc Handle Erlang messages (from LiveView process or other sources)
-spec websocket_info(Info, State) -> Result when
    Info :: term(),
    State :: state(),
    Result :: call_result().
websocket_info(_Info, State) ->
    % TODO: Handle real-time updates (push notifications, live data feeds, etc.)
    {[], State}.
