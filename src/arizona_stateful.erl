-module(arizona_stateful).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([call_mount_callback/2]).
-export([call_render_callback/1]).
-export([call_handle_event_callback/3]).
-export([new/2]).
-export([get_module/1]).
-export([get_binding/2]).
-export([get_binding/3]).
-export([get_bindings/1]).
-export([put_binding/3]).
-export([merge_bindings/2]).
-export([get_changed_bindings/1]).
-export([set_changed_bindings/2]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

-ignore_xref([new/2]).
-ignore_xref([get_binding/2]).
-ignore_xref([get_binding/3]).
-ignore_xref([put_binding/3]).

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
-nominal event_reply() :: dynamic().

%% --------------------------------------------------------------------
%% Behavior callback definitions
%% --------------------------------------------------------------------

-callback mount(Bindings) -> State when
    Bindings :: arizona_binder:bindings(),
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

-optional_callbacks([handle_event/3]).

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-spec call_mount_callback(Module, Bindings) -> State when
    Module :: module(),
    Bindings :: arizona_binder:bindings(),
    State :: state().
call_mount_callback(Module, Bindings) ->
    apply(Module, mount, [Bindings]).

-spec call_render_callback(State) -> Template when
    State :: state(),
    Template :: arizona_template:template().
call_render_callback(#state{} = State) ->
    apply(State#state.module, render, [State#state.bindings]).

-spec call_handle_event_callback(Event, Params, State) -> Result when
    Event :: binary(),
    Params :: map(),
    State :: state(),
    Result :: {reply, Reply, State1} | {noreply, State1},
    Reply :: term(),
    State1 :: state().
call_handle_event_callback(Event, Params, #state{} = State) ->
    apply(State#state.module, handle_event, [Event, Params, State]).

-spec new(Module, Bindings) -> State when
    Module :: module(),
    Bindings :: arizona_binder:bindings(),
    State :: state().
new(Module, Bindings) when is_atom(Module) ->
    #state{
        module = Module,
        bindings = Bindings,
        changed_bindings = arizona_binder:new(#{})
    }.

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
