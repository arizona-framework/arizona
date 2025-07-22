-module(arizona_live).
-behaviour(gen_server).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([start_link/0]).
-export([mount_view/3]).
-export([initial_render/2]).
-export([diff_stateful/2]).
-export([get_view/1]).
-export([set_view/2]).
-export([get_dependency_tracker/1]).
-export([set_dependency_tracker/2]).
-export([get_stateful_hierarchical/1]).
-export([set_stateful_hierarchical/2]).
-export([handle_event/4]).

%% --------------------------------------------------------------------
%% Gen_server callback exports
%% --------------------------------------------------------------------

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([state/0]).
-export_type([stateful_hierarchical/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

%% Internal state record for live processes
-record(state, {
    view :: arizona_view:view() | undefined,
    dependency_tracker :: arizona_tracker:tracker(),
    stateful_hierarchical :: stateful_hierarchical()
}).

-opaque state() :: #state{}.
-nominal stateful_hierarchical() :: #{
    arizona_stateful:id() => arizona_hierarchical:hierarchical_data()
}.

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-spec start_link() -> gen_server:start_ret().
start_link() ->
    gen_server:start_link(?MODULE, [], []).

-spec mount_view(Pid, ViewModule, ArizonaReq) -> ViewState when
    Pid :: pid(),
    ViewModule :: module(),
    ArizonaReq :: arizona_request:request(),
    ViewState :: arizona_stateful:state().
mount_view(Pid, ViewModule, ArizonaReq) ->
    gen_server:call(Pid, {mount_view, ViewModule, ArizonaReq}).

-spec initial_render(Pid, ViewState) -> HierarchicalStructure when
    Pid :: pid(),
    ViewState :: arizona_stateful:state(),
    HierarchicalStructure :: arizona_hierarchical:stateful_struct().
initial_render(Pid, ViewState) ->
    gen_server:call(Pid, {initial_render, ViewState}).

-spec diff_stateful(Pid, StatefulId) -> Diff when
    Pid :: pid(),
    StatefulId :: arizona_stateful:id(),
    Diff :: [term()].
diff_stateful(Pid, StatefulId) ->
    gen_server:call(Pid, {diff_stateful, StatefulId}).

-spec get_view(Pid) -> View when
    Pid :: pid(),
    View :: arizona_view:view().
get_view(Pid) ->
    gen_server:call(Pid, get_view).

-spec set_view(Pid, View) -> ok when
    Pid :: pid(),
    View :: arizona_view:view().
set_view(Pid, View) ->
    gen_server:cast(Pid, {set_view, View}).

-spec get_dependency_tracker(Pid) -> Tracker when
    Pid :: pid(),
    Tracker :: arizona_tracker:tracker().
get_dependency_tracker(Pid) ->
    gen_server:call(Pid, get_dependency_tracker).

-spec set_dependency_tracker(Pid, Tracker) -> ok when
    Pid :: pid(),
    Tracker :: arizona_tracker:tracker().
set_dependency_tracker(Pid, Tracker) ->
    gen_server:cast(Pid, {set_dependency_tracker, Tracker}).

-spec get_stateful_hierarchical(Pid) -> Hierarchical when
    Pid :: pid(),
    Hierarchical :: stateful_hierarchical().
get_stateful_hierarchical(Pid) ->
    gen_server:call(Pid, get_stateful_hierarchical).

-spec set_stateful_hierarchical(Pid, Hierarchical) -> ok when
    Pid :: pid(),
    Hierarchical :: stateful_hierarchical().
set_stateful_hierarchical(Pid, Hierarchical) ->
    gen_server:cast(Pid, {set_stateful_hierarchical, Hierarchical}).

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
    InitArgs :: [],
    State :: state().
init([]) ->
    {ok, #state{
        view = undefined,
        dependency_tracker = arizona_tracker:new(),
        stateful_hierarchical = #{}
    }}.

-spec handle_call(Message, From, State) -> Result when
    Message ::
        {mount_view, Module, ArizonaReq}
        | {initial_render, StatefulState}
        | {diff_stateful, StatefulId}
        | get_view
        | get_dependency_tracker
        | get_stateful_hierarchical
        | {handle_event, StatefulId | undefined, Event, Params},
    Module :: module(),
    ArizonaReq :: arizona_request:request(),
    StatefulState :: arizona_stateful:state(),
    StatefulId :: arizona_stateful:id(),
    Event :: arizona_stateful:event_name(),
    Params :: arizona_stateful:event_params(),
    From :: gen_server:from(),
    State :: state(),
    Result :: {reply, Reply, State1},
    Reply :: dynamic(),
    State1 :: state().
handle_call({mount_view, ViewModule, ArizonaReq}, _From, #state{} = State) ->
    ViewState = arizona_view:call_mount_callback(ViewModule, ArizonaReq),
    {reply, ViewState, State};
handle_call({initial_render, ViewState}, _From, #state{view = View} = State) when
    View =/= undefined
->
    Module = arizona_stateful:get_module(ViewState),
    Bindings = arizona_stateful:get_bindings(ViewState),
    {HierarchicalStructure, RenderView} = arizona_hierarchical:hierarchical_stateful(
        Module, Bindings, View
    ),
    DiffModeView = arizona_view:set_render_mode(diff, RenderView),
    {reply, HierarchicalStructure, State#state{
        view = DiffModeView,
        stateful_hierarchical = #{}
    }};
handle_call({diff_stateful, StatefulId}, _From, #state{view = View} = State) when
    View =/= undefined
->
    StatefulState = arizona_view:get_stateful_state(StatefulId, View),
    Module = arizona_stateful:get_module(StatefulState),
    Bindings = arizona_stateful:get_bindings(StatefulState),
    {Diff, DiffView} = arizona_differ:diff_stateful(Module, Bindings, View),
    {reply, Diff, State#state{view = DiffView}};
handle_call(get_view, _From, #state{view = View} = State) when View =/= undefined ->
    {reply, View, State};
handle_call(get_dependency_tracker, _From, #state{} = State) ->
    {reply, State#state.dependency_tracker, State};
handle_call(get_stateful_hierarchical, _From, #state{} = State) ->
    {reply, State#state.stateful_hierarchical, State};
handle_call(
    {handle_event, StatefulIdOrUndefined, Event, Params}, _From, #state{view = View} = State
) when View =/= undefined ->
    StatefulId =
        case StatefulIdOrUndefined of
            undefined -> arizona_view:get_id(View);
            _ -> StatefulIdOrUndefined
        end,
    StatefulState = arizona_view:get_stateful_state(StatefulId, View),
    StatefulModule = arizona_stateful:get_module(StatefulState),
    case
        arizona_stateful:call_handle_event_callback(StatefulModule, Event, Params, StatefulState)
    of
        {reply, Reply, UpdatedStatefulState} ->
            UpdatedView = arizona_view:put_stateful_state(StatefulId, UpdatedStatefulState, View),
            {reply, {reply, StatefulId, Reply}, State#state{view = UpdatedView}};
        {noreply, UpdatedStatefulState} ->
            UpdatedView = arizona_view:put_stateful_state(StatefulId, UpdatedStatefulState, View),
            {reply, {noreply, StatefulId}, State#state{view = UpdatedView}}
    end.

-spec handle_cast(Message, State) -> Result when
    Message ::
        {set_view, View}
        | {set_dependency_tracker, Tracker}
        | {set_stateful_hierarchical, Hierarchical},
    View :: arizona_view:view(),
    Tracker :: arizona_tracker:tracker(),
    Hierarchical :: stateful_hierarchical(),
    State :: state(),
    Result :: {noreply, State1},
    State1 :: state().
handle_cast({set_view, View}, #state{} = State) ->
    {noreply, State#state{view = View}};
handle_cast({set_dependency_tracker, Tracker}, #state{} = State) ->
    {noreply, State#state{dependency_tracker = Tracker}};
handle_cast({set_stateful_hierarchical, Hierarchical}, #state{} = State) ->
    {noreply, State#state{stateful_hierarchical = Hierarchical}}.
