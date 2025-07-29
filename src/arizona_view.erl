-module(arizona_view).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([call_mount_callback/2]).
-export([call_handle_info_callback/3]).
-export([new/4]).
-export([get_id/1]).
-export([get_state/1]).
-export([update_state/2]).
-export([get_render_mode/1]).
-export([set_render_mode/2]).
-export([get_stateful_state/2]).
-export([find_stateful_state/2]).
-export([put_stateful_state/3]).
-export([get_live_pid/1]).
-export([live_put_stateful_hierarchical/3]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([view/0]).
-export_type([render_mode/0]).
-export_type([layout/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-record(view, {
    id :: arizona_stateful:id(),
    layout :: layout() | undefined,
    stateful_states :: #{arizona_stateful:id() => arizona_stateful:state()},
    render_mode :: render_mode(),
    live_pid :: pid() | undefined
}).

-opaque view() :: #view{}.
-nominal render_mode() :: render | diff | hierarchical.
-nominal layout() :: {LayoutModule :: module(), LayoutRenderFun :: atom(), SlotName :: atom()}.

%% --------------------------------------------------------------------
%% Behavior callback definitions
%% --------------------------------------------------------------------

-callback mount(Req) -> State when
    Req :: arizona_request:request(),
    State :: arizona_stateful:state().

% Same as arizona_stateful:render/1
-callback render(Bindings) -> Template when
    Bindings :: arizona_binder:bindings(),
    Template :: arizona_template:template().

% Same as arizona_stateful:handle_event/3
-callback handle_event(Event, Params, State) -> Result when
    Event :: arizona_stateful:event_name(),
    Params :: arizona_stateful:event_params(),
    State :: arizona_stateful:state(),
    Result :: {reply, Reply, State1} | {noreply, State1},
    Reply :: arizona_stateful:event_reply(),
    State1 :: arizona_stateful:state().

-callback handle_info(Info, State) -> Result when
    Info :: term(),
    State :: arizona_stateful:state(),
    Result :: {noreply, State1},
    State1 :: arizona_stateful:state().

-optional_callbacks([handle_event/3, handle_info/2]).

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-spec call_mount_callback(Module, Req) -> State when
    Module :: module(),
    Req :: arizona_request:request(),
    State :: arizona_stateful:state().
call_mount_callback(Module, Req) ->
    apply(Module, mount, [Req]).

-spec call_handle_info_callback(Module, Info, State) -> Result when
    Module :: module(),
    Info :: term(),
    State :: arizona_stateful:state(),
    Result :: {noreply, State1},
    State1 :: arizona_stateful:state().
call_handle_info_callback(Module, Info, State) ->
    apply(Module, handle_info, [Info, State]).

-spec new(Module, State, RenderMode, LivePid) -> View when
    Module :: module(),
    State :: arizona_stateful:state(),
    RenderMode :: render | hierarchical,
    LivePid :: pid() | undefined,
    View :: view().
new(Module, State, RenderMode, LivePid) when
    is_atom(Module),
    (RenderMode =:= render orelse RenderMode =:= hierarchical),
    (is_pid(LivePid) orelse LivePid =:= undefined)
->
    Bindings = arizona_stateful:get_bindings(State),
    Id = arizona_stateful:get_id(Bindings),
    #view{
        id = Id,
        layout = undefined,
        stateful_states = #{Id => State},
        render_mode = RenderMode,
        live_pid = LivePid
    }.

-spec get_id(View) -> Id when
    View :: view(),
    Id :: arizona_stateful:id().
get_id(#view{} = View) ->
    View#view.id.

-spec get_state(View) -> State when
    View :: view(),
    State :: arizona_stateful:state().
get_state(#view{} = View) ->
    get_stateful_state(View#view.id, View).

-spec update_state(State, View) -> View1 when
    State :: arizona_stateful:state(),
    View :: view(),
    View1 :: view().
update_state(State, #view{} = View) ->
    Id = View#view.id,
    case View#view.stateful_states of
        #{Id := _} = States ->
            View#view{stateful_states = States#{Id => State}}
    end.

-spec get_render_mode(View) -> RenderMode when
    View :: view(),
    RenderMode :: render_mode().
get_render_mode(#view{} = View) ->
    View#view.render_mode.

-spec set_render_mode(RenderMode, View) -> View1 when
    RenderMode :: render_mode(),
    View :: view(),
    View1 :: view().
set_render_mode(RenderMode, #view{} = View) ->
    View#view{render_mode = RenderMode}.

-spec get_stateful_state(Id, View) -> StatefulState when
    Id :: arizona_stateful:id(),
    View :: view(),
    StatefulState :: arizona_stateful:state().
get_stateful_state(Id, #view{} = View) when is_binary(Id) ->
    maps:get(Id, View#view.stateful_states).

-spec find_stateful_state(Id, View) -> {ok, StatefulState} | error when
    Id :: arizona_stateful:id(),
    View :: view(),
    StatefulState :: arizona_stateful:state().
find_stateful_state(Id, #view{} = View) when is_binary(Id) ->
    maps:find(Id, View#view.stateful_states).

-spec put_stateful_state(Id, State, View) -> View1 when
    Id :: arizona_stateful:id(),
    State :: arizona_stateful:state(),
    View :: view(),
    View1 :: view().
put_stateful_state(Id, State, #view{} = View) ->
    States = View#view.stateful_states,
    View#view{stateful_states = States#{Id => State}}.

-spec get_live_pid(View) -> LivePid when
    View :: view(),
    LivePid :: pid() | undefined.
get_live_pid(#view{} = View) ->
    View#view.live_pid.

-spec live_put_stateful_hierarchical(StatefulId, HierarchicalData, View) -> ok when
    StatefulId :: arizona_stateful:id(),
    HierarchicalData :: arizona_hierarchical:hierarchical_data(),
    View :: view().
live_put_stateful_hierarchical(StatefulId, HierarchicalData, #view{} = View) ->
    case View#view.live_pid of
        undefined ->
            ok;
        LivePid ->
            Hierarchical = arizona_live:get_stateful_hierarchical(LivePid),
            UpdatedHierarchical = Hierarchical#{StatefulId => HierarchicalData},
            arizona_live:set_stateful_hierarchical(LivePid, UpdatedHierarchical)
    end.
