-module(arizona_live).
-moduledoc ~"""
GenServer-based live process for managing connected views with real-time updates.

Manages a single transport connection and its associated view, handling
real-time updates through differential patching. Each live process
represents one connected client and maintains view state, nested
component states, and dependency tracking for efficient updates.

## Process Lifecycle

1. Started via `start_link/4` with view module and mount arguments
2. Joins process group for connection tracking
3. Mounts view and initializes tracker
4. Handles initial rendering with hierarchical structure
5. Processes events and generates differential updates
6. Sends patches to transport process

## Event Handling

- View events: Handled by the main view module
- Component events: Routed to specific stateful components by ID
- Process messages: Handled by view's `handle_info/2` callback
- PubSub messages: Treated as view events with topic/data

## Differential Updates

All state changes trigger differential update generation:
- Compares old vs new state using `arizona_differ`
- Generates minimal patches for transport process
- Supports both `reply` (with data) and `noreply` responses

## Example Usage

```erlang
%% Start live process for transport connection
{ok, LivePid} = arizona_live:start_link(
    my_view,
    #{user_id => 123},
    Request,
    TransportPid
).

%% Handle transport event
ok = arizona_live:handle_event(LivePid, ~"button1", ~"click", #{}).

%% Check connection status
true = arizona_live:is_connected(LivePid).
```
""".
-behaviour(gen_server).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([start_link/4]).
-export([is_connected/1]).
-export([get_view/1]).
-export([initial_render/1]).
-export([handle_event/4]).

%% --------------------------------------------------------------------
%% Gen_server callback exports
%% --------------------------------------------------------------------

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

-ignore_xref([is_connected/1]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([state/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

%% Internal state record for live processes
-record(state, {
    view :: arizona_view:view(),
    tracker :: arizona_tracker:tracker(),
    transport_pid :: pid()
}).

-opaque state() :: #state{}.

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-doc ~"""
Starts a live process for a transport connection.

Creates a new GenServer process to manage the view and transport
connection. The process mounts the view and joins the connection
process group for tracking.
""".
-spec start_link(ViewModule, MountArg, ArizonaRequest, TransportPid) -> Return when
    ViewModule :: module(),
    MountArg :: arizona_view:mount_arg(),
    ArizonaRequest :: arizona_request:request(),
    TransportPid :: pid(),
    Return :: gen_server:start_ret().
start_link(ViewModule, MountArg, ArizonaRequest, TransportPid) ->
    gen_server:start_link(?MODULE, {ViewModule, MountArg, ArizonaRequest, TransportPid}, []).

-doc ~"""
Checks if a live process is still connected.

Returns `true` if the process is in the connected process group,
`false` otherwise. Used to verify connection status.
""".
-spec is_connected(Pid) -> IsConnected when
    Pid :: pid(),
    IsConnected :: boolean().
is_connected(Pid) ->
    Members = pg:get_local_members(?MODULE, connected),
    lists:member(Pid, Members).

-doc ~"""
Retrieves the current view state from a live process.

Synchronous call to get the complete view state including
nested component states and fingerprints.
""".
-spec get_view(Pid) -> View when
    Pid :: pid(),
    View :: arizona_view:view().
get_view(Pid) ->
    gen_server:call(Pid, get_view, infinity).

-doc ~"""
Generates the initial hierarchical structure for rendering.

Performs complete view rendering and returns hierarchical structure
suitable for initial HTML generation. Sets up dependency tracking
and component fingerprints.
""".
-spec initial_render(Pid) -> HierarchicalStructure when
    Pid :: pid(),
    HierarchicalStructure :: arizona_hierarchical_dict:hierarchical_structure().
initial_render(Pid) ->
    gen_server:call(Pid, initial_render, infinity).

-doc ~"""
Handles a transport event asynchronously.

Routes the event to either the main view (if StatefulId is `undefined`)
or to a specific stateful component. Generates differential updates
and sends them to the transport process.
""".
-spec handle_event(Pid, StatefulIdOrUndefined, Event, Params) -> ok when
    Pid :: pid(),
    StatefulIdOrUndefined :: arizona_stateful:id() | undefined,
    Event :: arizona_stateful:event_name(),
    Params :: arizona_stateful:event_params().
handle_event(Pid, StatefulIdOrUndefined, Event, Params) ->
    gen_server:cast(Pid, {handle_event, StatefulIdOrUndefined, Event, Params}).

%% --------------------------------------------------------------------
%% Gen_server callback implementations
%% --------------------------------------------------------------------

-spec init(InitArgs) -> {ok, State} when
    InitArgs :: {ViewModule, MountArg, ArizonaRequest, TransportPid},
    ViewModule :: module(),
    MountArg :: arizona_view:mount_arg(),
    ArizonaRequest :: arizona_request:request(),
    TransportPid :: pid(),
    State :: state().
init({ViewModule, MountArg, ArizonaRequest, TransportPid}) ->
    ok = pg:join(?MODULE, connected, self()),
    View = arizona_view:call_mount_callback(ViewModule, MountArg, ArizonaRequest),
    {ok, #state{
        view = View,
        tracker = arizona_tracker:new(),
        transport_pid = TransportPid
    }}.

-spec handle_call(Message, From, State) -> Result when
    Message ::
        get_view
        | initial_render
        | {handle_event, StatefulId | undefined, Event, Params},
    StatefulId :: arizona_stateful:id(),
    Event :: arizona_stateful:event_name(),
    Params :: arizona_stateful:event_params(),
    From :: gen_server:from(),
    State :: state(),
    Result :: {reply, Reply, State1},
    Reply :: dynamic(),
    State1 :: state().
handle_call(get_view, _From, #state{} = State) ->
    {reply, State#state.view, State};
handle_call(initial_render, _From, #state{} = State) ->
    undefined = arizona_tracker_dict:set_tracker(arizona_tracker:new()),
    undefined = arizona_hierarchical_dict:set_structure(#{}),
    {_Struct, HierarchicalView} = arizona_hierarchical:hierarchical_view(State#state.view),
    HierarchicalStructure = arizona_hierarchical_dict:clear(),
    {reply, HierarchicalStructure, State#state{view = HierarchicalView}}.

-spec handle_cast(Message, State) -> Result when
    Message :: term(),
    State :: state(),
    Result :: {noreply, State1},
    State1 :: state().
handle_cast({handle_event, StatefulIdOrUndefined, Event, Params}, #state{} = State) ->
    case StatefulIdOrUndefined of
        undefined ->
            handle_view_event(Event, Params, State);
        StatefulId ->
            handle_stateful_event(StatefulId, Event, Params, State)
    end.

-spec handle_info(Info, State) -> Result when
    Info :: term(),
    State :: state(),
    Result :: {noreply, State1},
    State1 :: state().
handle_info({pubsub_message, Topic, Data}, #state{} = State) ->
    handle_view_event(Topic, Data, State);
handle_info(Info, #state{} = State) ->
    handle_view_info(Info, State).

-doc ~"""
Handles live process termination.

Called when the live process is shutting down, typically due to WebSocket
connection closure. Forwards the termination reason to the view's terminate
callback for cleanup operations like removing user presence, saving state, etc.

The reason parameter provides context about why the process is terminating:
- `normal`: Clean shutdown
- `{shutdown, Reason}`: Shutdown with specific reason
- `{remote, Code, Reason}`: WebSocket closed by client (browser)
- Other reasons: Internal errors, timeouts, etc.
""".
-spec terminate(Reason, State) -> Result when
    Reason :: arizona_websocket:terminate_reason(),
    State :: state(),
    Result :: term().
terminate(Reason, State) ->
    arizona_view:call_terminate_callback(Reason, State#state.view).

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

handle_view_event(Event, Params, State) ->
    {Actions, UpdatedView} = arizona_view:call_handle_event_callback(
        Event, Params, State#state.view
    ),
    {Diff, DiffView} = arizona_differ:diff_view(UpdatedView),
    ViewState = arizona_view:get_state(DiffView),
    ViewId = arizona_stateful:get_binding(id, ViewState),
    ok = handle_actions_response(ViewId, Diff, Actions, State),
    {noreply, State#state{view = DiffView}}.

handle_stateful_event(StatefulId, Event, Params, State) ->
    View = State#state.view,
    StatefulState = arizona_view:get_stateful_state(StatefulId, View),
    {Actions, UpdatedStatefulState} = arizona_stateful:call_handle_event_callback(
        Event, Params, StatefulState
    ),
    Module = arizona_stateful:get_module(UpdatedStatefulState),
    Bindings = arizona_stateful:get_bindings(UpdatedStatefulState),
    DiffStatefulId = arizona_binder:get(id, Bindings),
    UpdatedView = arizona_view:put_stateful_state(StatefulId, UpdatedStatefulState, View),
    DiffBindings = arizona_binder:to_map(Bindings),
    {Diff, DiffView} = arizona_differ:diff_root_stateful(Module, DiffBindings, UpdatedView),
    ok = handle_actions_response(DiffStatefulId, Diff, Actions, State),
    {noreply, State#state{view = DiffView}}.

handle_actions_response(StatefulId, Diff, Actions, State) ->
    State#state.transport_pid ! {actions_response, StatefulId, Diff, Actions},
    ok.

handle_view_info(Info, State) ->
    {Actions, UpdatedView} = arizona_view:call_handle_info_callback(Info, State#state.view),
    {Diff, DiffView} = arizona_differ:diff_view(UpdatedView),
    ViewState = arizona_view:get_state(DiffView),
    ViewId = arizona_stateful:get_binding(id, ViewState),
    ok = handle_actions_response(ViewId, Diff, Actions, State),
    {noreply, State#state{view = DiffView}}.
