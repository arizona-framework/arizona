-module(arizona_websocket).
-moduledoc ~"""
Cowboy WebSocket handler providing WebSocket transport for Arizona framework.

Implements the `cowboy_websocket` behavior to provide real-time bidirectional
communication between clients and Arizona live processes. Handles WebSocket
upgrade, message routing, JSON serialization, and error management.

## Message Protocol

JSON messages with `type` field for routing:

### Client to Server
- `{"type": "event", "stateful_id": "comp1", "event": "click", "params": {}}`
- `{"type": "ping"}`

### Server to Client
- `{"type": "initial_render", "stateful_id": "view1", "structure": {...}}`
- `{"type": "diff", "stateful_id": "comp1", "changes": [...]}`
- `{"type": "reply", "data": {...}}`
- `{"type": "pong"}`
- `{"type": "reload"}` (development)
- `{"type": "error", "message": "..."}`

## Connection Lifecycle

1. HTTP request upgraded to WebSocket via `init/2`
2. WebSocket initialized with `websocket_init/1`
3. Live process started and initial render sent
4. Bidirectional message handling begins
5. Live reload subscription for development

## Error Handling

All message processing errors are caught and converted to error messages
sent back to the client, preventing WebSocket disconnection.

## Example Usage

```javascript
// Client-side WebSocket connection
const ws = new WebSocket('ws://localhost:4000/live?path=/users&qs=');
ws.send(JSON.stringify({
  type: 'event',
  stateful_id: 'button1',
  event: 'click',
  params: {}
}));
```
""".
-behaviour(cowboy_websocket).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([state/0]).
-export_type([call_result/0]).
-export_type([terminate_reason/0]).
-export_type([websocket_close_code/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-record(state, {
    live_pid :: pid(),
    live_shutdown_timeout :: timeout()
}).

-opaque state() :: #state{}.

-nominal call_result() :: {Commands :: cowboy_websocket:commands(), State :: state()}.

-nominal terminate_reason() ::
    normal
    | shutdown
    | {shutdown, term()}
    | {remote, websocket_close_code(), Reason :: binary()}
    | term().

-nominal websocket_close_code() ::
    % Normal Closure
    1000
    % Going Away
    | 1001
    % Protocol Error
    | 1002
    % Unsupported Data
    | 1003
    % No Status Rcvd
    | 1005
    % Abnormal Closure
    | 1006
    % Invalid frame payload data
    | 1007
    % Policy Violation
    | 1008
    % Message Too Big
    | 1009
    % Mandatory Extension
    | 1010
    % Internal Server Error
    | 1011
    % TLS handshake
    | 1015
    % Other codes
    | pos_integer().

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-doc ~"""
Initializes WebSocket upgrade from HTTP request.

Extracts path and query string parameters, resolves the view module
and mount arguments, and prepares data for WebSocket initialization.
""".
-spec init(Req, State) -> Result when
    Req :: cowboy_req:req(),
    State :: undefined,
    Result :: {cowboy_websocket, Req, {ViewModule, MountArg, ArizonaRequest, LiveShutdownTimeout}},
    ViewModule :: module(),
    MountArg :: arizona_view:mount_arg(),
    ArizonaRequest :: arizona_request:request(),
    LiveShutdownTimeout :: timeout().
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
    LiveShutdownTimeout = 5_000,
    {cowboy_websocket, CowboyRequest, {ViewModule, MountArg, ArizonaRequest, LiveShutdownTimeout}}.

-doc ~"""
Initializes WebSocket connection and starts live process.

Starts `arizona_live` process, subscribes to live reload (development),
performs initial render, and sends initial hierarchical structure to client.
""".
-spec websocket_init(InitData) -> Result when
    InitData :: {ViewModule, MountArg, ArizonaRequest, LiveShutdownTimeout},
    ViewModule :: module(),
    MountArg :: arizona_view:mount_arg(),
    ArizonaRequest :: arizona_request:request(),
    LiveShutdownTimeout :: timeout(),
    Result :: call_result().
websocket_init({ViewModule, MountArg, ArizonaRequest, LiveShutdownTimeout}) ->
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

    State = #state{
        live_pid = LivePid,
        live_shutdown_timeout = LiveShutdownTimeout
    },

    {[{text, InitialPayload}], State}.

-doc ~"""
Handles WebSocket messages from client.

Parses JSON messages and routes them based on type. Handles DOM events,
ping messages, and unknown message types with proper error handling.
""".
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

-doc ~"""
Handles Erlang messages from live process and other sources.

Processes differential updates, event replies, and live reload messages
from the associated `arizona_live` process, converting them to WebSocket
messages for the client.
""".
-spec websocket_info(Info, State) -> Result when
    Info :: term(),
    State :: state(),
    Result :: call_result().
websocket_info({actions_response, StatefulId, Diff, Actions}, State) ->
    handle_actions_response(StatefulId, Diff, Actions, State);
% Handle live reload messages
websocket_info({pubsub_message, ~"live_reload", {file_changed, reload}}, State) ->
    Message = #{type => ~"reload"},
    ReloadPayload = json_encode(Message),
    {[{text, ReloadPayload}], State}.

-doc ~"""
Handles WebSocket connection termination.

Forwards the termination reason to the live process for graceful shutdown,
allowing views to perform cleanup operations via their terminate/2 callback.
""".
-spec terminate(Reason, Req, State) -> ok when
    Reason :: terminate_reason(),
    Req :: cowboy_req:req(),
    State :: state().
terminate(Reason, _Req, State) ->
    gen_server:stop(State#state.live_pid, {shutdown, Reason}, State#state.live_shutdown_timeout).

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

%% Handle actions response from Live
-spec handle_actions_response(StatefulId, Diff, Actions, State) -> Result when
    StatefulId :: arizona_stateful:id(),
    Diff :: arizona_differ:diff(),
    Actions :: arizona_action:actions(),
    State :: state(),
    Result :: call_result().
handle_actions_response(StatefulId, Diff, Actions, #state{} = State) ->
    ActionCmds = [action_to_command(Action) || Action <- Actions],
    handle_diff_response(StatefulId, Diff, ActionCmds, State).

%% Convert action to WebSocket command
-spec action_to_command(Action) -> Command when
    Action :: arizona_action:action(),
    Command :: {text, JSON},
    JSON :: json:encode_value().
action_to_command({reply, Data}) ->
    Payload = json_encode(#{
        type => ~"reply",
        data => Data
    }),
    {text, Payload};
action_to_command({redirect, Url, Options}) ->
    Payload = json_encode(#{
        type => ~"redirect",
        url => Url,
        options => Options
    }),
    {text, Payload};
action_to_command(reload) ->
    Payload = json_encode(#{
        type => ~"reload"
    }),
    {text, Payload}.

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
