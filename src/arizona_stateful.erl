-module(arizona_stateful).
-moduledoc ~""""
Stateful component behavior definition and state management.

Defines the behavior for stateful components that maintain internal state
and can handle WebSocket events. Components implement callbacks for
mounting, rendering, and event handling while the module manages state
and change tracking for efficient differential updates.

## Behavior Callbacks

- `mount/1` - Initialize component state from initial bindings
- `render/1` - Generate template from current bindings
- `handle_event/3` - Handle WebSocket events (optional, raises if events triggered but not defined)

## State Management

Component state includes:
- Module reference for callback execution
- Current bindings for template rendering
- Changed bindings for differential update tracking

## Example Implementation

```erlang
-module(counter_component).
-compile({parse_transform, arizona_parse_transform}).
-behaviour(arizona_stateful).
-export([mount/1, render/1, handle_event/3]).

mount(InitialBindings) ->
    arizona_stateful:new(?MODULE, InitialBindings#{count => 0}).

render(Bindings) ->
    arizona_template:from_html(~"""
    <div id="{arizona_template:get_binding(id, Bindings)}">
        <p>Count: {arizona_template:get_binding(count, Bindings)}</p>
        <button
            onclick="arizona.sendEventTo(
                '{arizona_template:get_binding(id, Bindings)}',
                'increment'
            )"
        >
            +
        </button>
    </div>
    """).

handle_event(~"increment", _Params, State) ->
    Count = arizona_stateful:get_binding(count, State),
    NewState = arizona_stateful:put_binding(count, Count + 1, State),
    {noreply, NewState}.
```
"""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([call_mount_callback/2]).
-export([call_unmount_callback/1]).
-export([call_render_callback/1]).
-export([call_handle_event_callback/3]).
-export([new/2]).
-export([get_module/1]).
-export([get_binding/2]).
-export([get_binding/3]).
-export([get_binding_lazy/3]).
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
-ignore_xref([get_binding_lazy/3]).
-ignore_xref([put_binding/3]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([state/0]).
-export_type([id/0]).
-export_type([event_name/0]).
-export_type([event_params/0]).
-export_type([event_reply/0]).
-export_type([handle_event_result/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-record(state, {
    module :: module(),
    bindings :: arizona_binder:bindings(),
    changed_bindings :: arizona_binder:bindings()
}).

-opaque state() :: #state{}.
-nominal id() :: binary().
-nominal event_name() :: binary().
-nominal event_params() :: dynamic().
-nominal event_reply() :: json:encode_value().
-nominal handle_event_result() :: {Actions :: arizona_action:actions(), State :: state()}.

%% --------------------------------------------------------------------
%% Behavior callback definitions
%% --------------------------------------------------------------------

-callback mount(Bindings) -> State when
    Bindings :: arizona_binder:map(),
    State :: state().

-doc ~"""
Optional callback for component cleanup when unmounted.

This callback is invoked when a stateful component is removed from the component
tree (e.g., when the parent template changes and the component is no longer
rendered). Use this callback to clean up resources such as:

- Cancel active timers
- Close network connections
- Unsubscribe from PubSub topics
- Release GenServer references
- Clean up ETS tables or other shared resources

Example implementations:

```erlang
% Timer cleanup
unmount(State) ->
    case arizona_stateful:get_binding(timer_ref, State) of
        undefined -> ok;
        TimerRef -> timer:cancel(TimerRef)
    end.

% PubSub cleanup
unmount(State) ->
    TopicName = arizona_stateful:get_binding(topic, State),
    arizona_pubsub:leave(TopicName, self()).
```

The callback should always return `ok`. If the unmount callback crashes,
the error will propagate following Erlang's "let it crash" philosophy.
""".
-callback unmount(State) -> Result when
    State :: state(),
    Result :: ok.

-callback render(Bindings) -> Template when
    Bindings :: arizona_binder:bindings(),
    Template :: arizona_template:template().

-callback handle_event(Event, Params, State) -> Result when
    Event :: event_name(),
    Params :: event_params(),
    State :: arizona_stateful:state(),
    Result :: handle_event_result().

-optional_callbacks([unmount/1, handle_event/3]).

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-doc ~"""
Executes a component's mount callback.

Calls the module's `mount/1` function with initial bindings to
initialize component state. Used during component lifecycle startup.
""".
-spec call_mount_callback(Module, Bindings) -> State when
    Module :: module(),
    Bindings :: arizona_binder:map(),
    State :: state().
call_mount_callback(Module, Bindings) ->
    apply(Module, mount, [Bindings]).

-doc ~"""
Safely calls a component's unmount callback if it exists.

Checks if the component module exports an `unmount/1` function and calls it
if available. This function handles the optional nature of the unmount callback.

The unmount callback is called when:
- A stateful component's template fingerprint doesn't match the previous render
- This happens when the component is replaced by a different component at the same location
- The previous component at that location was a stateful component that needs cleanup
""".
-spec call_unmount_callback(State) -> Result when
    State :: state(),
    Result :: ok.
call_unmount_callback(#state{} = State) ->
    Module = State#state.module,
    case erlang:function_exported(Module, unmount, 1) of
        true ->
            apply(Module, unmount, [State]);
        false ->
            ok
    end.

-doc ~"""
Executes a component's render callback.

Calls the module's `render/1` function with current bindings to
generate the component template. Used during rendering pipeline.
""".
-spec call_render_callback(State) -> Template when
    State :: state(),
    Template :: arizona_template:template().
call_render_callback(#state{} = State) ->
    apply(State#state.module, render, [State#state.bindings]).

-doc ~"""
Executes a component's event handler callback.

Calls the module's `handle_event/3` function to process WebSocket events.
Returns a list of actions to execute and the updated state.
""".
-spec call_handle_event_callback(Event, Params, State) -> Result when
    Event :: event_name(),
    Params :: event_params(),
    State :: state(),
    Result :: handle_event_result().
call_handle_event_callback(Event, Params, #state{} = State) ->
    apply(State#state.module, handle_event, [Event, Params, State]).

-doc ~"""
Creates new stateful component state.

Initializes state with module reference and bindings. Changed bindings
start empty and are populated as bindings are modified.
""".
-spec new(Module, Bindings) -> State when
    Module :: module(),
    Bindings :: arizona_binder:map(),
    State :: state().
new(Module, Bindings) when is_atom(Module), is_map(Bindings) ->
    #state{
        module = Module,
        bindings = arizona_binder:new(Bindings),
        changed_bindings = arizona_binder:new(#{})
    }.

-doc ~"""
Returns the component's module atom.

Used to identify which module implements the component callbacks.
""".
-spec get_module(State) -> Mod when
    State :: state(),
    Mod :: module().
get_module(#state{} = State) ->
    State#state.module.

-doc ~"""
Gets a binding value by key, throwing if not found.

Returns the value associated with the key from component bindings.
Raises `{badkey, Key}` exception if key is missing.
""".
-spec get_binding(Key, State) -> Value when
    Key :: arizona_binder:key(),
    State :: state(),
    Value :: arizona_binder:value().
get_binding(Key, #state{} = State) ->
    arizona_binder:get(Key, State#state.bindings).

-doc ~"""
Gets a binding value by key with default fallback.

Returns the value if key exists, otherwise returns the provided
default value. This is a safe lookup that never raises an exception.
""".
-spec get_binding(Key, State, Default) -> Value when
    Key :: arizona_binder:key(),
    State :: state(),
    Default :: arizona_binder:value(),
    Value :: arizona_binder:value().
get_binding(Key, #state{} = State, Default) ->
    arizona_binder:get(Key, State#state.bindings, Default).

-doc ~"""
Gets a binding value by key with lazy default function fallback.

Returns the value if key exists, otherwise calls the default function
to generate a fallback value. Useful for expensive computations that
should only be performed when needed.
""".
-spec get_binding_lazy(Key, State, DefaultFun) -> Value when
    Key :: arizona_binder:key(),
    State :: state(),
    DefaultFun :: arizona_binder:default_fun(),
    Value :: arizona_binder:value().
get_binding_lazy(Key, #state{} = State, DefaultFun) ->
    arizona_binder:get_lazy(Key, State#state.bindings, DefaultFun).

-doc ~"""
Returns all component bindings.

Provides access to the complete bindings map for template rendering
and other operations that need all component data.
""".
-spec get_bindings(State) -> Bindings when
    State :: state(),
    Bindings :: arizona_binder:bindings().
get_bindings(#state{} = State) ->
    State#state.bindings.

-doc ~"""
Updates a binding and tracks the change.

Sets the key-value pair in bindings and adds it to changed bindings
for differential update tracking. No-op if value is unchanged.
""".
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

-doc ~"""
Merges multiple bindings into component state.

Applies multiple key-value pairs using `put_binding/3`, tracking
all changes for differential updates.
""".
-spec merge_bindings(Bindings, State) -> State1 when
    Bindings :: arizona_binder:map(),
    State :: state(),
    State1 :: state().
merge_bindings(Bindings, #state{} = State) when is_map(Bindings) ->
    maps:fold(fun put_binding/3, State, Bindings).

-doc ~"""
Returns bindings that have changed since last diff.

Used by differential update system to identify which template
variables need re-rendering for WebSocket updates.
""".
-spec get_changed_bindings(State) -> ChangedBindings when
    State :: state(),
    ChangedBindings :: arizona_binder:bindings().
get_changed_bindings(#state{} = State) ->
    State#state.changed_bindings.

-doc ~"""
Sets the changed bindings for the component.

Usually called with empty bindings after differential updates are
processed to reset change tracking.
""".
-spec set_changed_bindings(ChangedBindings, State) -> State1 when
    ChangedBindings :: arizona_binder:bindings(),
    State :: state(),
    State1 :: state().
set_changed_bindings(ChangedBindings, #state{} = State) ->
    State#state{changed_bindings = ChangedBindings}.
