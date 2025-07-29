-module(arizona_live).
-behaviour(gen_server).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([start_link/0]).
-export([get_view/1]).
-export([set_view/2]).
-export([record_variable_dependency/2]).
-export([get_stateful_hierarchical/1]).
-export([set_stateful_hierarchical/2]).
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
-export_type([stateful_hierarchical/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

%% Internal state record for live processes
-record(state, {
    view :: arizona_view:view() | undefined,
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

-spec record_variable_dependency(Pid, VarName) -> ok when
    Pid :: pid(),
    VarName :: arizona_tracker:var_name().
record_variable_dependency(Pid, VarName) ->
    gen_server:cast(Pid, {record_variable_dependency, VarName}).

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
        stateful_hierarchical = #{}
    }}.

-spec handle_call(Message, From, State) -> Result when
    Message ::
        get_view
        | get_stateful_hierarchical
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
        | {record_variable_dependency, VarName}
        | {set_stateful_hierarchical, Hierarchical},
    View :: arizona_view:view(),
    VarName :: arizona_tracker:var_name(),
    Hierarchical :: stateful_hierarchical(),
    State :: state(),
    Result :: {noreply, State1},
    State1 :: state().
handle_cast({set_view, View}, #state{} = State) ->
    {noreply, State#state{view = View}};
handle_cast({set_stateful_hierarchical, Hierarchical}, #state{} = State) ->
    {noreply, State#state{stateful_hierarchical = Hierarchical}}.

-spec handle_info(Info, State) -> Result when
    Info :: term(),
    State :: state(),
    Result :: {noreply, State1},
    State1 :: state().
handle_info(Info, #state{view = View} = State) when View =/= undefined ->
    ViewState = arizona_view:get_state(View),
    Module = arizona_stateful:get_module(ViewState),
    {noreply, UpdatedViewState} = arizona_view:call_handle_info_callback(Module, Info, ViewState),
    UpdatedView = arizona_view:update_state(UpdatedViewState, View),
    {noreply, State#state{view = UpdatedView}}.
