-module(arizona_view).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([call_mount_callback/2]).
-export([call_render_callback/1]).
-export([call_handle_event_callback/3]).
-export([call_handle_info_callback/2]).
-export([new/3]).
-export([get_layout/1]).
-export([get_state/1]).
-export([update_state/2]).
-export([get_stateful_state/2]).
-export([fingerprint_matches/4]).
-export([put_fingerprint/4]).
-export([remove_fingerprint/3]).
-export([find_stateful_state/2]).
-export([put_stateful_state/3]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

-ignore_xref([new/3]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([view/0]).
-export_type([layout/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-record(view, {
    layout :: layout() | none,
    state :: arizona_stateful:state(),
    stateful_states :: #{arizona_stateful:id() => arizona_stateful:state()},
    fingerprints :: #{
        arizona_stateful:id() => #{
            arizona_tracker:element_index() => arizona_template:fingerprint()
        }
    }
}).

-opaque view() :: #view{}.
-nominal layout() :: {
    Module :: module(),
    RenderFun :: atom(),
    SlotName :: atom(),
    Bindings :: arizona_binder:map()
}.

%% --------------------------------------------------------------------
%% Behavior callback definitions
%% --------------------------------------------------------------------

-callback mount(ArizonaRequest) -> View when
    ArizonaRequest :: arizona_request:request(),
    View :: view().

-callback render(Bindings) -> Template when
    Bindings :: arizona_binder:bindings(),
    Template :: arizona_template:template().

-callback handle_event(Event, Params, View) -> Result when
    Event :: arizona_stateful:event_name(),
    Params :: arizona_stateful:event_params(),
    View :: view(),
    Result :: {reply, Reply, View1} | {noreply, View1},
    Reply :: arizona_stateful:event_reply(),
    View1 :: view().

-callback handle_info(Info, View) -> Result when
    Info :: term(),
    View :: view(),
    Result :: {noreply, View1},
    View1 :: view().

-optional_callbacks([handle_event/3, handle_info/2]).

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-spec call_mount_callback(Module, ArizonaRequest) -> View when
    Module :: module(),
    ArizonaRequest :: arizona_request:request(),
    View :: view().
call_mount_callback(Module, ArizonaRequest) when is_atom(Module) ->
    apply(Module, mount, [ArizonaRequest]).

-spec call_render_callback(View) -> Template when
    View :: view(),
    Template :: arizona_template:template().
call_render_callback(#view{state = State}) ->
    Module = arizona_stateful:get_module(State),
    Bindings = arizona_stateful:get_bindings(State),
    apply(Module, render, [Bindings]).

-spec call_handle_event_callback(Event, Params, View) -> Result when
    Event :: binary(),
    Params :: map(),
    View :: view(),
    Result :: {reply, Reply, View1} | {noreply, View1},
    Reply :: term(),
    View1 :: view().
call_handle_event_callback(Event, Params, #view{state = State} = View) ->
    Module = arizona_stateful:get_module(State),
    apply(Module, handle_event, [Event, Params, View]).

-spec call_handle_info_callback(Info, View) -> Result when
    Info :: term(),
    View :: view(),
    Result :: {noreply, View1},
    View1 :: view().
call_handle_info_callback(Info, #view{state = State} = View) ->
    Module = arizona_stateful:get_module(State),
    apply(Module, handle_info, [Info, View]).

-spec new(Module, Bindings, Layout) -> View when
    Module :: module(),
    Bindings :: arizona_binder:map(),
    Layout :: {LayoutModule, LayoutRenderFun, LayoutSlotName, LayoutBindings} | none,
    LayoutModule :: module(),
    LayoutRenderFun :: atom(),
    LayoutSlotName :: atom(),
    LayoutBindings :: arizona_binder:map(),
    View :: view().
new(Module, Bindings, Layout) when is_atom(Module) ->
    State = arizona_stateful:new(Module, Bindings),
    #view{
        layout = Layout,
        state = State,
        stateful_states = #{},
        fingerprints = #{}
    }.

-spec get_layout(View) -> Layout when
    View :: view(),
    Layout :: layout() | none.
get_layout(#view{} = View) ->
    View#view.layout.

-spec get_state(View) -> State when
    View :: view(),
    State :: arizona_stateful:state().
get_state(#view{} = View) ->
    View#view.state.

-spec update_state(State, View) -> View1 when
    State :: arizona_stateful:state(),
    View :: view(),
    View1 :: view().
update_state(State, #view{} = View) ->
    View#view{state = State}.

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

-spec fingerprint_matches(Id, ElementIndex, Fingerprint, View) -> boolean() when
    Id :: arizona_stateful:id(),
    ElementIndex :: arizona_tracker:element_index(),
    Fingerprint :: arizona_template:fingerprint(),
    View :: view().
fingerprint_matches(Id, ElementIndex, Fingerprint, #view{} = View) ->
    case View#view.fingerprints of
        #{Id := #{ElementIndex := Fingerprint}} ->
            true;
        #{} ->
            false
    end.

-spec put_fingerprint(Id, ElementIndex, Fingerprint, View) -> View1 when
    Id :: arizona_stateful:id(),
    ElementIndex :: arizona_tracker:element_index(),
    Fingerprint :: arizona_template:fingerprint(),
    View :: view(),
    View1 :: view().
put_fingerprint(Id, ElementIndex, Fingerprint, #view{fingerprints = Fingerprints} = View) ->
    StatefulFingerprints = maps:get(Id, Fingerprints, #{}),
    UpdatedStatefulFingerprints = StatefulFingerprints#{ElementIndex => Fingerprint},
    View#view{fingerprints = Fingerprints#{Id => UpdatedStatefulFingerprints}}.

-spec remove_fingerprint(Id, ElementIndex, View) -> View1 when
    Id :: arizona_stateful:id(),
    ElementIndex :: arizona_tracker:element_index(),
    View :: view(),
    View1 :: view().
remove_fingerprint(Id, ElementIndex, #view{} = View) ->
    case View#view.fingerprints of
        #{Id := StatefulFingerprints} = Fingerprints ->
            UpdatedStatefulFingerprints = maps:remove(ElementIndex, StatefulFingerprints),
            View#view{fingerprints = Fingerprints#{Id => UpdatedStatefulFingerprints}};
        #{} ->
            View
    end.
