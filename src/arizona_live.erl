-module(arizona_live).
-behaviour(gen_server).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([start_link/3]).
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

-spec start_link(ViewModule, ArizonaRequest, TransportPid) -> Return when
    ViewModule :: module(),
    ArizonaRequest :: arizona_request:request(),
    TransportPid :: pid(),
    Return :: gen_server:start_ret().
start_link(ViewModule, ArizonaRequest, TransportPid) ->
    gen_server:start_link(?MODULE, {ViewModule, ArizonaRequest, TransportPid}, []).

-spec is_connected(Pid) -> IsConnected when
    Pid :: pid(),
    IsConnected :: boolean().
is_connected(Pid) ->
    Members = pg:get_local_members(?MODULE, connected),
    lists:member(Pid, Members).

-spec get_view(Pid) -> View when
    Pid :: pid(),
    View :: arizona_view:view().
get_view(Pid) ->
    gen_server:call(Pid, get_view, infinity).

-spec initial_render(Pid) -> HierarchicalStructure when
    Pid :: pid(),
    HierarchicalStructure :: arizona_hierarchical_dict:hierarchical_structure().
initial_render(Pid) ->
    gen_server:call(Pid, initial_render, infinity).

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
    InitArgs :: {ViewModule, ArizonaRequest, TransportPid},
    ViewModule :: module(),
    ArizonaRequest :: arizona_request:request(),
    TransportPid :: pid(),
    State :: state().
init({ViewModule, ArizonaRequest, TransportPid}) ->
    ok = pg:join(?MODULE, connected, self()),
    View = arizona_view:call_mount_callback(ViewModule, ArizonaRequest),
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

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

handle_view_event(Event, Params, State) ->
    case arizona_view:call_handle_event_callback(Event, Params, State#state.view) of
        {reply, Reply, UpdatedView} ->
            {Diff, DiffView} = arizona_differ:diff_view(UpdatedView),
            ViewState = arizona_view:get_state(DiffView),
            ViewId = arizona_stateful:get_binding(id, ViewState),
            ok = handle_reply_response(ViewId, Diff, Reply, State),
            {noreply, State#state{view = DiffView}};
        {noreply, UpdatedView} ->
            {Diff, DiffView} = arizona_differ:diff_view(UpdatedView),
            ViewState = arizona_view:get_state(DiffView),
            ViewId = arizona_stateful:get_binding(id, ViewState),
            ok = handle_noreply_response(ViewId, Diff, State),
            {noreply, State#state{view = DiffView}}
    end.

handle_stateful_event(StatefulId, Event, Params, State) ->
    View = State#state.view,
    StatefulState = arizona_view:get_stateful_state(StatefulId, View),
    case arizona_stateful:call_handle_event_callback(Event, Params, StatefulState) of
        {reply, Reply, UpdatedStatefulState} ->
            Module = arizona_stateful:get_module(UpdatedStatefulState),
            Bindings = arizona_stateful:get_bindings(UpdatedStatefulState),
            DiffStatefulId = arizona_binder:get(id, Bindings),
            UpdatedView = arizona_view:put_stateful_state(StatefulId, UpdatedStatefulState, View),
            DiffBindings = arizona_binder:to_map(Bindings),
            {Diff, DiffView} = arizona_differ:diff_root_stateful(Module, DiffBindings, UpdatedView),
            ok = handle_reply_response(DiffStatefulId, Diff, Reply, State),
            {noreply, State#state{view = DiffView}};
        {noreply, UpdatedStatefulState} ->
            Module = arizona_stateful:get_module(UpdatedStatefulState),
            Bindings = arizona_stateful:get_bindings(UpdatedStatefulState),
            DiffStatefulId = arizona_binder:get(id, Bindings),
            UpdatedView = arizona_view:put_stateful_state(StatefulId, UpdatedStatefulState, View),
            DiffBindings = arizona_binder:to_map(Bindings),
            {Diff, DiffView} = arizona_differ:diff_root_stateful(Module, DiffBindings, UpdatedView),
            ok = handle_noreply_response(DiffStatefulId, Diff, State),
            {noreply, State#state{view = DiffView}}
    end.

handle_reply_response(StatefulId, Diff, Reply, State) ->
    State#state.transport_pid ! {reply_response, StatefulId, Diff, Reply},
    ok.

handle_noreply_response(StatefulId, Diff, State) ->
    State#state.transport_pid ! {noreply_response, StatefulId, Diff},
    ok.

handle_view_info(Info, State) ->
    {noreply, UpdatedView} = arizona_view:call_handle_info_callback(Info, State#state.view),
    {Diff, DiffView} = arizona_differ:diff_view(UpdatedView),
    ViewState = arizona_view:get_state(DiffView),
    ViewId = arizona_stateful:get_binding(id, ViewState),
    ok = handle_noreply_response(ViewId, Diff, State),
    {noreply, State#state{view = DiffView}}.
