-module(arizona_stateful).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([call_mount_callback/2]).
-export([call_render_callback/2]).
-export([call_handle_event_callback/4]).
-export([call_handle_info_callback/3]).
-export([call_dynamic_function/2]).
-export([new/2]).
-export([get_id/1]).
-export([get_module/1]).
-export([get_binding/2]).
-export([get_binding/3]).
-export([get_bindings/1]).
-export([put_binding/3]).
-export([merge_bindings/2]).
-export([get_changed_bindings/1]).
-export([set_changed_bindings/2]).
-export([prepare_render/3]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([state/0]).
-export_type([id/0]).
-export_type([event_name/0]).
-export_type([event_params/0]).
-export_type([event_reply/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

%% Internal state record for stateful components
-record(state, {
    module :: module(),
    bindings :: arizona_binder:bindings(),
    changed_bindings :: arizona_binder:bindings()
}).

-opaque state() :: #state{}.
-nominal id() :: binary().
-nominal event_name() :: binary().
-nominal event_params() :: #{binary() => json:decode_value()}.
-nominal event_reply() :: term().

%% --------------------------------------------------------------------
%% Behavior callback definitions
%% --------------------------------------------------------------------

-callback mount(View) -> State when
    View :: arizona_view:view(),
    State :: state().

-callback render(Bindings) -> Template when
    Bindings :: arizona_binder:bindings(),
    Template :: arizona_template:template().

-callback handle_event(Event, Params, State) -> Result when
    Event :: event_name(),
    Params :: event_params(),
    State :: arizona_stateful:state(),
    Result :: {noreply, State1} | {reply, Reply, State1},
    Reply :: event_reply(),
    State1 :: arizona_stateful:state().

-callback handle_info(Info, State) -> {noreply, State1} when
    Info :: term(),
    State :: arizona_stateful:state(),
    State1 :: arizona_stateful:state().

-optional_callbacks([handle_event/3, handle_info/2]).

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-spec call_mount_callback(Mod, View) -> State when
    Mod :: module(),
    View :: arizona_view:view(),
    State :: state().
call_mount_callback(Mod, View) when is_atom(Mod) ->
    apply(Mod, mount, [View]).

-spec call_render_callback(Mod, Bindings) -> Template when
    Mod :: module(),
    Bindings :: arizona_binder:bindings(),
    Template :: arizona_template:template().
call_render_callback(Mod, Bindings) when is_atom(Mod) ->
    apply(Mod, render, [Bindings]).

-spec call_handle_event_callback(Module, Event, Params, State) -> Result when
    Module :: module(),
    Event :: binary(),
    Params :: map(),
    State :: arizona_stateful:state(),
    Result :: {noreply, State1} | {reply, Reply, State1},
    Reply :: term(),
    State1 :: arizona_stateful:state().
call_handle_event_callback(Module, Event, Params, State) ->
    case erlang:function_exported(Module, handle_event, 3) of
        true ->
            apply(Module, handle_event, [Event, Params, State]);
        false ->
            {noreply, State}
    end.

-spec call_handle_info_callback(Module, Info, State) -> Result when
    Module :: module(),
    Info :: term(),
    State :: arizona_stateful:state(),
    Result :: {noreply, State1},
    State1 :: arizona_stateful:state().
call_handle_info_callback(Module, Info, State) ->
    case erlang:function_exported(Module, handle_info, 2) of
        true ->
            apply(Module, handle_info, [Info, State]);
        false ->
            {noreply, State}
    end.

-spec call_dynamic_function(Fun, View) -> Result when
    Fun :: fun((View) -> Result),
    View :: arizona_view:view(),
    Result :: term().
call_dynamic_function(Fun, View) when is_function(Fun, 1) ->
    apply(Fun, [View]).

-spec new(Module, Bindings) -> State when
    Module :: module(),
    Bindings :: arizona_binder:bindings(),
    State :: state().
new(Module, Bindings) when is_atom(Module) ->
    #state{
        module = Module,
        bindings = Bindings,
        changed_bindings = arizona_binder:new()
    }.

-spec get_id(Bindings) -> Id when
    Bindings :: arizona_binder:bindings(),
    Id :: id().
get_id(Bindings) ->
    case arizona_binder:get(id, Bindings) of
        Id when is_binary(Id) ->
            Id
    end.

-spec get_module(State) -> Mod when
    State :: state(),
    Mod :: module().
get_module(#state{} = State) ->
    State#state.module.

-spec get_binding(Key, State) -> Value when
    Key :: arizona_binder:key(),
    State :: state(),
    Value :: arizona_binder:value().
get_binding(Key, #state{} = State) ->
    arizona_binder:get(Key, State#state.bindings).

-spec get_binding(Key, State, Default) -> Value when
    Key :: arizona_binder:key(),
    State :: state(),
    Default :: arizona_binder:default_fun(),
    Value :: arizona_binder:value().
get_binding(Key, #state{} = State, Default) ->
    arizona_binder:get(Key, State#state.bindings, Default).

-spec get_bindings(State) -> Bindings when
    State :: state(),
    Bindings :: arizona_binder:bindings().
get_bindings(#state{} = State) ->
    State#state.bindings.

-spec put_binding(Key, Value, State) -> State1 when
    Key :: arizona_binder:key(),
    Value :: arizona_binder:value(),
    State :: state(),
    State1 :: state().
put_binding(Key, Value, #state{} = State) ->
    case arizona_binder:find(Key, State#state.bindings) of
        {ok, Value} ->
            State;
        _ ->
            NewBindings = arizona_binder:put(Key, Value, State#state.bindings),
            NewChangedBindings = arizona_binder:put(Key, Value, State#state.changed_bindings),
            State#state{
                bindings = NewBindings,
                changed_bindings = NewChangedBindings
            }
    end.

-spec merge_bindings(Bindings, State) -> State1 when
    Bindings :: arizona_binder:bindings(),
    State :: state(),
    State1 :: state().
merge_bindings(Bindings, #state{} = State) when is_map(Bindings) ->
    maps:fold(fun put_binding/3, State, Bindings).

-spec get_changed_bindings(State) -> ChangedBindings when
    State :: state(),
    ChangedBindings :: arizona_binder:bindings().
get_changed_bindings(#state{} = State) ->
    State#state.changed_bindings.

-spec set_changed_bindings(ChangedBindings, State) -> State1 when
    ChangedBindings :: arizona_binder:bindings(),
    State :: state(),
    State1 :: state().
set_changed_bindings(ChangedBindings, #state{} = State) ->
    State#state{changed_bindings = ChangedBindings}.

-spec prepare_render(Module, Bindings, View1) -> {Id, Template, View2} when
    Module :: module(),
    Bindings :: arizona_binder:bindings(),
    View1 :: arizona_view:view(),
    Id :: id(),
    Template :: arizona_template:template(),
    View2 :: arizona_view:view().
prepare_render(Module, Bindings, View) ->
    Id = get_id(Bindings),
    case arizona_view:find_stateful_state(Id, View) of
        {ok, State} ->
            BindingsState = merge_bindings(Bindings, State),
            RenderBindings = get_bindings(BindingsState),
            Template = call_render_callback(Module, RenderBindings),
            StatefulView = arizona_view:put_stateful_state(Id, BindingsState, View),
            {Id, Template, StatefulView};
        error ->
            NewState = new(Module, Bindings),
            StatefulView = arizona_view:put_stateful_state(Id, NewState, View),
            MountState = call_mount_callback(Module, StatefulView),
            MountBindings = get_bindings(MountState),
            Template = call_render_callback(Module, MountBindings),
            MountView = arizona_view:put_stateful_state(Id, MountState, View),
            {Id, Template, MountView}
    end.
