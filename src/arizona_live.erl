-module(arizona_live).
-behaviour(gen_server).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([start_link/2]).
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
    tracker :: arizona_tracker:tracker()
}).

-opaque state() :: #state{}.

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-spec start_link(ViewModule, ArizonaRequest) -> Return when
    ViewModule :: module(),
    ArizonaRequest :: arizona_request:request(),
    Return :: gen_server:start_ret().
start_link(ViewModule, ArizonaRequest) ->
    gen_server:start_link(?MODULE, {ViewModule, ArizonaRequest}, []).

-spec get_view(Pid) -> View when
    Pid :: pid(),
    View :: arizona_view:view().
get_view(Pid) ->
    gen_server:call(Pid, get_view).

-spec initial_render(Pid) -> HierarchicalStructure when
    Pid :: pid(),
    HierarchicalStructure :: arizona_hierarchical_dict:hierarchical_structure().
initial_render(Pid) ->
    gen_server:call(Pid, initial_render).

-spec handle_event(Pid, StatefulIdOrUndefined, Event, Params) -> Result when
    Pid :: pid(),
    StatefulIdOrUndefined :: arizona_stateful:id() | undefined,
    Event :: arizona_stateful:event_name(),
    Params :: arizona_stateful:event_params(),
    Result :: {reply, StatefulId, Reply} | {noreply, StatefulId},
    StatefulId :: arizona_stateful:id(),
    Reply :: arizona_stateful:event_reply().
handle_event(Pid, StatefulIdOrUndefined, Event, Params) ->
    gen_server:call(Pid, {handle_event, StatefulIdOrUndefined, Event, Params}).

%% --------------------------------------------------------------------
%% Gen_server callback implementations
%% --------------------------------------------------------------------

-spec init(InitArgs) -> {ok, State} when
    InitArgs :: {ViewModule, ArizonaRequest},
    ViewModule :: module(),
    ArizonaRequest :: arizona_request:request(),
    State :: state().
init({ViewModule, ArizonaRequest}) ->
    View = arizona_view:call_mount_callback(ViewModule, ArizonaRequest),
    {ok, #state{
        view = View,
        tracker = arizona_tracker:new()
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
    View = State#state.view,
    ViewState = arizona_view:get_state(View),
    Module = arizona_stateful:get_module(ViewState),
    Bindings = arizona_stateful:get_bindings(ViewState),
    {_Struct, HierarchicalView} = arizona_hierarchical:hierarchical_stateful(
        Module, Bindings, View
    ),
    HierarchicalStructure = arizona_hierarchical_dict:clear(),
    {reply, HierarchicalStructure, State#state{view = HierarchicalView}};
handle_call({handle_event, StatefulIdOrUndefined, Event, Params}, _From, State) ->
    case StatefulIdOrUndefined of
        undefined ->
            handle_view_event(Event, Params, State);
        StatefulId ->
            handle_stateful_event(StatefulId, Event, Params, State)
    end.

-spec handle_cast(Message, State) -> Result when
    Message :: term(),
    State :: state(),
    Result :: {noreply, State1},
    State1 :: state().
handle_cast(_Message, #state{} = State) ->
    {noreply, State}.

-spec handle_info(Info, State) -> Result when
    Info :: term(),
    State :: state(),
    Result :: {noreply, State1},
    State1 :: state().
handle_info(Info, #state{} = State) ->
    {noreply, UpdatedView} = arizona_view:call_handle_info_callback(Info, State#state.view),
    {noreply, State#state{view = UpdatedView}}.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

handle_view_event(Event, Params, State) ->
    case arizona_view:call_handle_event_callback(Event, Params, State#state.view) of
        {reply, Reply, UpdatedView} ->
            {Diff, DiffView} = arizona_differ:diff_view(UpdatedView),
            ViewState = arizona_view:get_state(DiffView),
            ViewId = arizona_stateful:get_binding(id, ViewState),
            {reply, {reply, ViewId, Diff, Reply}, State#state{view = DiffView}};
        {noreply, UpdatedView} ->
            {Diff, DiffView} = arizona_differ:diff_view(UpdatedView),
            ViewState = arizona_view:get_state(DiffView),
            ViewId = arizona_stateful:get_binding(id, ViewState),
            {reply, {noreply, ViewId, Diff}, State#state{view = DiffView}}
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
            {Diff, DiffView} = arizona_differ:diff_stateful(Module, Bindings, UpdatedView),
            {reply, {reply, DiffStatefulId, Diff, Reply}, State#state{view = DiffView}};
        {noreply, UpdatedStatefulState} ->
            Module = arizona_stateful:get_module(UpdatedStatefulState),
            Bindings = arizona_stateful:get_bindings(UpdatedStatefulState),
            DiffStatefulId = arizona_binder:get(id, Bindings),
            UpdatedView = arizona_view:put_stateful_state(StatefulId, UpdatedStatefulState, View),
            {Diff, DiffView} = arizona_differ:diff_stateful(Module, Bindings, UpdatedView),
            {reply, {noreply, DiffStatefulId, Diff}, State#state{view = DiffView}}
    end.
