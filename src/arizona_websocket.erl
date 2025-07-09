-module(arizona_websocket).
-moduledoc ~"""
Provides WebSocket handler functionality for Arizona LiveView connections.

## Overview

The WebSocket handler manages real-time communication between Arizona LiveView
processes and client browsers. It handles connection establishment, message routing,
and state synchronization for interactive web applications.

## Features

- **Connection Management**: Establishes WebSocket connections and initializes LiveView processes
- **Message Routing**: Routes client events to appropriate LiveView handlers
- **Diff Updates**: Sends efficient hierarchical diffs for DOM updates
- **Error Handling**: Comprehensive error handling with client notifications
- **Real-time Communication**: Supports bidirectional communication with clients
- **JSON Encoding**: Custom JSON encoding with tuple-to-array conversion for JavaScript

## Key Functions

- `init/2`: Initialize WebSocket connection and resolve LiveView module
- `websocket_init/1`: Start LiveView process and send initial render
- `websocket_handle/2`: Handle incoming client messages and events
- `websocket_info/2`: Handle Erlang messages from LiveView processes
- `json_encode/1`: Encode terms to JSON with JavaScript compatibility

## Message Types

- **event**: DOM events from client (clicks, form submissions, etc.)
- **ping**: Client heartbeat messages
- **initial_render**: Initial hierarchical structure sent to client
- **diff**: Hierarchical diff updates for efficient DOM patching
- **reply**: Direct responses to client events
- **error**: Error notifications to client
""".

-behaviour(cowboy_websocket).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

%% --------------------------------------------------------------------
%% Testing helper exports
%% --------------------------------------------------------------------

-export([new_state/1]).
-export([get_live_pid/1]).
-export([json_encode/1]).

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

-doc ~"""
Opaque WebSocket state containing LiveView process information.

Maintains the connection between the WebSocket handler and the associated
LiveView process for message routing and state management.
""".
-opaque state() :: #state{}.

-doc ~"""
Result type for WebSocket callback functions.

Contains commands to send to the client and updated handler state.
""".
-type call_result() :: {Commands :: cowboy_websocket:commands(), State :: state()}.

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-doc ~"""
Initialize WebSocket connection and resolve LiveView module from path parameter.

Extracts the LiveView path from the query parameter (?path=/users), resolves
the appropriate LiveView module using the routing system, and prepares for
WebSocket upgrade.

## Examples

```erlang
1> Req = #{path => "/live", qs => "path=%2Fusers", ...}.
#{...}
2> arizona_websocket:init(Req, #{}).
{cowboy_websocket, Req, {user_live, ArizonaReq}}
```
""".
-spec init(Req, State) -> {cowboy_websocket, Req, {LiveModule, ArizonaReq}} when
    Req :: cowboy_req:req(),
    State :: map(),
    LiveModule :: module(),
    ArizonaReq :: arizona_request:request().
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

-doc ~"""
WebSocket connection established - start LiveView process and send initial render.

Creates a new Arizona socket in hierarchical mode, starts the LiveView process,
performs initial mount and render, then sends the hierarchical structure to
the client for initial page rendering.

## Examples

```erlang
1> arizona_websocket:websocket_init({user_live, ArizonaReq}).
{[{text, InitialPayload}], State}
```
""".
-spec websocket_init(InitData) -> Result when
    InitData :: {LiveModule, Req},
    LiveModule :: module(),
    Req :: arizona_request:request(),
    Result :: call_result().
websocket_init({LiveModule, Req}) ->
    % Create initial Arizona socket in hierarchical mode
    Socket = arizona_socket:new(#{mode => hierarchical}),
    StatefulState = arizona_stateful:new(root, LiveModule, #{}),
    Socket1 = arizona_socket:put_stateful_state(StatefulState, Socket),

    % Start the LiveView process
    {ok, LivePid} = arizona_live:start_link(LiveModule, Socket1),

    % Mount the LiveView
    _MountedSocket = arizona_live:mount(LivePid, Req),

    % Initial render in hierarchical mode
    RenderedSocket = arizona_live:render(LivePid),
    HierarchicalStructure = arizona_socket:get_hierarchical_acc(RenderedSocket),

    % Send initial hierarchical structure to client
    InitialPayload = json_encode(#{
        type => ~"initial_render",
        structure => HierarchicalStructure
    }),

    ok = arizona_live:set_mode(LivePid, diff),

    State = #state{live_pid = LivePid},

    {[{text, InitialPayload}], State}.

-doc ~"""
Handle incoming WebSocket messages from client.

Parses JSON messages from the client and routes them to appropriate handlers
based on message type. Supports event messages, ping messages, and provides
comprehensive error handling for invalid messages.

## Examples

```erlang
1> Message = {text, <<"{\"type\":\"event\",\"event\":\"click\"}">>}.
{text, ...}
2> arizona_websocket:websocket_handle(Message, State).
{[{text, DiffPayload}], State}
```
""".
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

-doc ~"""
Handle Erlang messages from LiveView processes or other sources.

Processes internal Erlang messages that may be sent from LiveView processes
or other parts of the system. Currently returns empty commands but provides
a foundation for real-time push notifications and live data feeds.

## Examples

```erlang
1> arizona_websocket:websocket_info(some_message, State).
{[], State}
```
""".
-spec websocket_info(Info, State) -> Result when
    Info :: term(),
    State :: state(),
    Result :: call_result().
websocket_info(_Info, State) ->
    % TODO: Handle real-time updates (push notifications, live data feeds, etc.)
    {[], State}.

%% --------------------------------------------------------------------
%% Testing helper functions
%% --------------------------------------------------------------------

-doc ~"""
Create a new WebSocket state for testing purposes.

Utility function for test suites to create WebSocket handler state
with a specified LiveView process PID.

## Examples

```erlang
1> arizona_websocket:new_state(LivePid).
#state{live_pid = LivePid}
```
""".
-spec new_state(LivePid) -> State when
    LivePid :: pid(),
    State :: state().
new_state(LivePid) ->
    #state{live_pid = LivePid}.

-doc ~"""
Get LiveView process PID from WebSocket state for testing.

Utility function for test suites to extract the LiveView process PID
from the WebSocket handler state.

## Examples

```erlang
1> arizona_websocket:get_live_pid(State).
<0.123.0>
```
""".
-spec get_live_pid(State) -> LivePid when
    State :: state(),
    LivePid :: pid().
get_live_pid(#state{live_pid = LivePid}) ->
    LivePid.

-doc ~"""
Encode terms to JSON with custom tuple-to-array conversion for JavaScript compatibility.

Converts Erlang terms to JSON format with special handling for tuples,
which are converted to arrays for better JavaScript interoperability.
Used for encoding WebSocket messages sent to clients.

## Examples

```erlang
1> arizona_websocket:json_encode(#{type => diff, changes => [{root, [{0, "new"}]}]}).
<<"{\"type\":\"diff\",\"changes\":[[\"root\",[[0,\"new\"]]]]}">>
```
""".
-spec json_encode(Term) -> JsonData when
    Term :: term(),
    JsonData :: iodata().
json_encode(Term) ->
    % Convert tuples to arrays for JavaScript compatibility
    json:encode(Term, fun json_encoder/2).

%% --------------------------------------------------------------------
%% Private functions
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
handle_event_message(Message, #state{live_pid = LivePid} = State) ->
    Event = maps:get(~"event", Message),
    Params = maps:get(~"params", Message, #{}),

    case arizona_live:handle_event(LivePid, Event, Params) of
        {noreply, _UpdatedSocket} ->
            handle_noreply_response(State);
        {reply, Reply, _UpdatedSocket} ->
            handle_reply_response(Reply, State)
    end.

%% Handle noreply response from LiveView
-spec handle_noreply_response(State) -> Result when
    State :: state(),
    Result :: call_result().
handle_noreply_response(#state{live_pid = LivePid} = State) ->
    DiffSocket = arizona_live:render(LivePid),
    case arizona_socket:get_changes(DiffSocket) of
        [] ->
            {[], State};
        DiffChanges ->
            DiffPayload = json_encode(#{
                type => ~"diff",
                changes => DiffChanges
            }),
            % TODO: Clear live_pid socket changes
            _ClearedSocket = arizona_socket:clear_changes(DiffSocket),
            {[{text, DiffPayload}], State}
    end.

%% Handle reply response from LiveView
-spec handle_reply_response(Reply, State) -> Result when
    Reply :: term(),
    State :: state(),
    Result :: call_result().
handle_reply_response(Reply, #state{live_pid = LivePid} = State) ->
    ReplyPayload = json_encode(#{
        type => ~"reply",
        data => Reply
    }),

    DiffSocket = arizona_live:render(LivePid),
    case arizona_socket:get_changes(DiffSocket) of
        [] ->
            {[{text, ReplyPayload}], State};
        DiffChanges ->
            DiffPayload = json_encode(#{
                type => ~"diff",
                changes => DiffChanges
            }),
            % TODO: Clear live_pid socket changes
            _ClearedSocket = arizona_socket:clear_changes(DiffSocket),
            {[{text, ReplyPayload}, {text, DiffPayload}], State}
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

%% Custom JSON encoder that converts tuples to arrays for JavaScript compatibility
-spec json_encoder(Term, Encoder) -> JsonData when
    Term :: term(),
    Encoder :: json:encoder(),
    JsonData :: iodata().
json_encoder(Tuple, Encode) when is_tuple(Tuple) ->
    % Convert tuple to array
    json:encode_list(tuple_to_list(Tuple), Encode);
json_encoder(Other, Encode) ->
    % For all other types, use the default JSON encoder
    json:encode_value(Other, Encode).
