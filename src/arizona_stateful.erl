-module(arizona_stateful).
-moduledoc ~"""
Provides stateful component functionality for Arizona LiveView applications.

## Overview

The stateful module defines the behavior and state management for stateful
components in Arizona LiveView applications. It manages component lifecycle,
binding tracking, change detection, and fingerprinting for efficient updates.

## Features

- **Component Behavior**: Defines callbacks for mount, render, and unmount lifecycle
- **State Management**: Tracks component state with ID, module, and bindings
- **Change Detection**: Monitors binding changes for efficient re-rendering
- **Fingerprinting**: Uses fingerprints to determine when remounting is needed
- **Binding System**: Provides get/put operations for component variable bindings
- **Lifecycle Callbacks**: Safe callback invocation with optional unmount support

## Key Functions

- `new/3`: Create new stateful component state
- `get_binding/2`, `put_binding/3`: Manage component bindings
- `should_remount/1`: Determine if component needs remounting
- `call_mount_callback/2`, `call_render_callback/2`: Invoke lifecycle callbacks
- `get_module/1`, `get_id/1`: Access component metadata

## Component Lifecycle

1. **Mount**: Initialize component with initial bindings
2. **Render**: Generate template output based on current state
3. **Update**: Track binding changes and detect need for re-rendering
4. **Unmount**: Clean up component resources (optional callback)

The stateful behavior ensures efficient updates by tracking only changed bindings
and using fingerprints to determine when full remounting is necessary.
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([call_mount_callback/2]).
-export([call_unmount_callback/2]).
-export([call_render_callback/2]).
-export([call_dynamic_function/2]).
-export([prepare_render/3]).
-export([new/3]).
-export([get_module/1]).
-export([get_id/1]).
-export([get_binding/2]).
-export([get_binding/3]).
-export([get_bindings/1]).
-export([put_binding/3]).
-export([put_bindings/2]).
-export([get_changed_bindings/1]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([state/0]).
-export_type([id/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-doc ~"""
Component identifier type for stateful components.

Can be either the special `root` atom for the root component or a binary
string for nested components.
""".
-type id() :: root | binary().

%% Internal state record for stateful components
-record(state, {
    id :: id(),
    module :: module(),
    bindings :: arizona_binder:bindings(),
    % Track which bindings changed
    changed_bindings :: arizona_binder:bindings()
}).

-doc ~"""
Stateful component state containing all component information.

Tracks the component ID, module, current bindings, changed bindings for
efficient updates, and fingerprint for remount detection.
""".
-opaque state() :: #state{}.

%% --------------------------------------------------------------------
%% Behavior callback definitions
%% --------------------------------------------------------------------

-doc ~"""
Mount callback for initializing stateful components.

Called when a stateful component is first created or needs to be remounted.
Should initialize the component's state and return the updated socket.
""".
-callback mount(Socket) -> Socket1 when
    Socket :: arizona_socket:socket(),
    Socket1 :: arizona_socket:socket().

-doc ~"""
Render callback for generating component template output.

Called to render the component's template with the current socket state.
Should return the socket with accumulated HTML or hierarchical structure.
""".
-callback render(Socket) -> Socket1 when
    Socket :: arizona_socket:socket(),
    Socket1 :: arizona_socket:socket().

-doc ~"""
Unmount callback for cleaning up component resources.

Optional callback invoked when a component is being destroyed. Can be used
to clean up resources, cancel timers, or perform other cleanup operations.
""".
-callback unmount(Socket) -> Socket1 when
    Socket :: arizona_socket:socket(),
    Socket1 :: arizona_socket:socket().

%

-optional_callbacks([unmount/1]).

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-doc ~"""
Invoke the mount callback for a stateful component module.

Calls the module's mount/1 callback function with the provided socket,
handling component initialization and setup.

## Examples

```erlang
1> Socket = arizona_socket:new(#{}).
#socket{...}
2> arizona_stateful:call_mount_callback(my_component, Socket).
#socket{...}
```
""".
-spec call_mount_callback(Mod, Socket) -> Socket1 when
    Mod :: module(),
    Socket :: arizona_socket:socket(),
    Socket1 :: arizona_socket:socket().
call_mount_callback(Mod, Socket) when is_atom(Mod) ->
    apply(Mod, mount, [Socket]).

-doc ~"""
Invoke the unmount callback for a stateful component module if it exists.

Safely calls the module's unmount/1 callback if exported, otherwise returns
the socket unchanged. Provides safe cleanup for components.

## Examples

```erlang
1> Socket = arizona_socket:new(#{}).
#socket{...}
2> arizona_stateful:call_unmount_callback(my_component, Socket).
#socket{...}
```
""".
-spec call_unmount_callback(Mod, Socket) -> Socket1 when
    Mod :: module(),
    Socket :: arizona_socket:socket(),
    Socket1 :: arizona_socket:socket().
call_unmount_callback(Mod, Socket) when is_atom(Mod) ->
    case erlang:function_exported(Mod, unmount, 1) of
        true ->
            apply(Mod, unmount, [Socket]);
        false ->
            Socket
    end.

-doc ~"""
Invoke the render callback for a stateful component module.

Calls the module's render/1 callback function with the provided socket,
generating the component's template output.

## Examples

```erlang
1> Socket = arizona_socket:new(#{}).
#socket{...}
2> arizona_stateful:call_render_callback(my_component, Socket).
#socket{...}
```
""".
-spec call_render_callback(Mod, Bindings) -> Template when
    Mod :: module(),
    Bindings :: arizona_socket:bindings(),
    Template :: arizona_template:template().
call_render_callback(Mod, Bindings) when is_atom(Mod) ->
    apply(Mod, render, [Bindings]).

-doc ~"""
Invoke a dynamic function with the provided socket.

Calls template functions generated by the parse transform, providing
safe function invocation for dynamic template elements.

## Examples

```erlang
1> Fun = fun(Socket) -> arizona_socket:get_binding(name, Socket) end.
#Fun<...>
2> arizona_stateful:call_dynamic_function(Fun, Socket).
~"John"
```
""".
-spec call_dynamic_function(Fun, Socket) -> Result when
    Fun :: fun((arizona_socket:socket()) -> term()),
    Socket :: arizona_socket:socket(),
    Result :: term().
call_dynamic_function(Fun, Socket) when is_function(Fun, 1) ->
    apply(Fun, [Socket]).

-doc ~"""
Create a new stateful component state.

Initializes a stateful component with the specified ID, module, and initial
bindings. Generates a fingerprint for change detection and remount decisions.

## Examples

```erlang
1> arizona_stateful:new(root, my_component, #{name => ~"John"}).
#state{id = root, module = my_component, bindings = #{name => ~"John"}, ...}
2> arizona_stateful:new(~"child_1", child_component, #{}).
#state{id = ~"child_1", module = child_component, bindings = #{}, ...}
```
""".
-spec new(Id, Mod, Bindings) -> State when
    Id :: id(),
    Mod :: module(),
    Bindings :: arizona_socket:bindings(),
    State :: state().
new(Id, Mod, Bindings) when (Id =:= root orelse is_binary(Id)), is_atom(Mod), is_map(Bindings) ->
    #state{
        id = Id,
        module = Mod,
        bindings = Bindings,
        changed_bindings = #{}
    }.

-doc ~"""
Get the module name from stateful component state.

Returns the module associated with the stateful component for callback
invocation and identification purposes.

## Examples

```erlang
1> State = arizona_stateful:new(root, my_component, #{}).
#state{...}
2> arizona_stateful:get_module(State).
my_component
```
""".
-spec get_module(State) -> Mod when
    State :: state(),
    Mod :: module().
get_module(#state{} = State) ->
    State#state.module.

-doc ~"""
Get the component ID from stateful component state.

Returns the unique identifier for the stateful component within its
parent context.

## Examples

```erlang
1> State = arizona_stateful:new(root, my_component, #{}).
#state{...}
2> arizona_stateful:get_id(State).
root
```
""".
-spec get_id(State) -> Id when
    State :: state(),
    Id :: id().
get_id(#state{} = State) ->
    State#state.id.

-doc ~"""
Get a binding value from stateful component state.

Retrieves a binding value by key, throwing an error if the binding
is not found. Use get_binding/3 for safe access with defaults.

## Examples

```erlang
1> State = arizona_stateful:new(root, my_component, #{name => ~"John"}).
#state{...}
2> arizona_stateful:get_binding(name, State).
~"John"
3> arizona_stateful:get_binding(missing, State).
** exception throw: {binding_not_found,missing}
```
""".
-spec get_binding(Key, State) -> Value when
    Key :: arizona_binder:key(),
    State :: state(),
    Value :: arizona_binder:value().
get_binding(Key, #state{} = State) ->
    arizona_binder:get(Key, State#state.bindings).

-doc ~"""
Get a binding value from stateful component state with a default.

Safely retrieves a binding value by key, returning the default value
if the binding is not found.

## Examples

```erlang
1> State = arizona_stateful:new(root, my_component, #{name => ~"John"}).
#state{...}
2> arizona_stateful:get_binding(name, State, ~"Unknown").
~"John"
3> arizona_stateful:get_binding(missing, State, ~"Unknown").
~"Unknown"
```
""".
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

-doc ~"""
Put a binding value into stateful component state.

Updates a binding value in the component state, tracking the change for
efficient re-rendering. Returns the same state if the value hasn't changed.

## Examples

```erlang
1> State = arizona_stateful:new(root, my_component, #{}).
#state{...}
2> State2 = arizona_stateful:put_binding(name, ~"John", State).
#state{bindings = #{name => ~"John"}, changed_bindings = #{name => ~"John"}, ...}
```
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
Put multiple binding values into stateful component state.

Updates multiple binding values in the component state, tracking all changes
for efficient re-rendering. Uses put_binding/3 internally for each binding.

## Examples

```erlang
1> State = arizona_stateful:new(root, my_component, #{}).
#state{...}
2> Bindings = #{name => ~"John", age => 30}.
#{name => ~"John", age => 30}
3> arizona_stateful:put_bindings(Bindings, State).
#state{bindings = #{name => ~"John", age => 30}, ...}
```
""".
-spec put_bindings(Bindings, State) -> State1 when
    Bindings :: arizona_socket:bindings(),
    State :: state(),
    State1 :: state().
put_bindings(Bindings, #state{} = State) when is_map(Bindings) ->
    maps:fold(fun put_binding/3, State, Bindings).

-doc ~"""
Get the changed bindings from stateful component state.

Returns a map of bindings that have changed since the last rendering,
used for efficient diff generation and change detection.

## Examples

```erlang
1> State = arizona_stateful:new(root, my_component, #{name => ~"John"}).
#state{...}
2> State2 = arizona_stateful:put_binding(age, 30, State).
#state{...}
3> arizona_stateful:get_changed_bindings(State2).
#{age => 30}
```
""".
-spec get_changed_bindings(State) -> ChangedBindings when
    State :: state(),
    ChangedBindings :: arizona_binder:bindings().
get_changed_bindings(#state{} = State) ->
    State#state.changed_bindings.

-doc ~"""
Prepare stateful component for rendering.

Handles the initialization and setup of stateful components, including
state management, binding updates, and template generation. This function
manages the complete lifecycle of preparing a stateful component for rendering.

## Examples

```erlang
1> Socket = arizona_socket:new(#{}).
#socket{...}
2> Bindings = #{id => root, name => ~"John"}.
#{id => root, name => ~"John"}
3> {Id, Template, Socket1} = arizona_stateful:prepare_render(my_component, Bindings, Socket).
{root, #template{...}, #socket{...}}
```
""".
-spec prepare_render(Module, Bindings, Socket) -> {Id, Template, Socket1} when
    Module :: module(),
    Bindings :: arizona_binder:bindings(),
    Socket :: arizona_socket:socket(),
    Id :: term(),
    Template :: arizona_template:template(),
    Socket1 :: arizona_socket:socket().
prepare_render(Mod, Bindings, Socket) ->
    Id = arizona_binder:get(id, Bindings),
    case arizona_socket:find_stateful_state(Id, Socket) of
        {ok, State} ->
            %% Apply new bindings to existing state before checking remount
            UpdatedState = put_bindings(Bindings, State),
            %% Update socket with new state and call render callback (which handles diffing)
            Socket1 = arizona_socket:put_stateful_state(UpdatedState, Socket),
            UpdatedBindings = get_bindings(UpdatedState),
            Template = call_render_callback(Mod, UpdatedBindings),
            {Id, Template, Socket1};
        error ->
            State = new(Id, Mod, Bindings),
            Socket1 = arizona_socket:put_stateful_state(State, Socket),
            %% Call mount callback for new components
            Socket2 = call_mount_callback(Mod, Socket1),
            %% Call the component's render callback which handles
            %% rendering and returns updated socket
            MountedState = arizona_socket:get_stateful_state(Id, Socket2),
            MountedBindings = get_bindings(MountedState),
            Template = call_render_callback(Mod, MountedBindings),
            {Id, Template, Socket2}
    end.
