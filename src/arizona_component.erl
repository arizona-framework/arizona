-module(arizona_component).
-moduledoc ~"""
Provides component invocation functionality for Arizona LiveView applications.

## Overview

The component module handles the invocation of both stateful and stateless
components within Arizona LiveView applications. It manages component lifecycles,
state transitions, and binding management to ensure proper component behavior
and efficient rendering.

## Features

- **Stateful Components**: Full lifecycle management with mount, remount, and render callbacks
- **Stateless Components**: Lightweight function-based components with temporary bindings
- **State Management**: Intelligent state tracking and remount detection
- **Binding System**: Efficient binding management for component parameters
- **Lifecycle Callbacks**: Proper unmount/remount handling for stateful components
- **Performance Optimization**: Minimal re-rendering through change detection

## Key Functions

- `call_stateful/3`: Invoke stateful components with lifecycle management
- `call_stateless/4`: Invoke stateless components with temporary bindings

## Component Types

### Stateful Components

Stateful components maintain state across renders and support the full component
lifecycle including mount, render, and unmount callbacks. They automatically
handle remounting when significant changes occur.

### Stateless Components

Stateless components are lightweight functions that render based on provided
bindings without maintaining persistent state. They're ideal for simple
presentational components.

## Lifecycle Management

The module handles component lifecycle events including:
1. **Mount**: Initialize new components
2. **Remount**: Reinitialize when fingerprint changes
3. **Render**: Generate component output
4. **Unmount**: Clean up resources during remount

This ensures proper resource management and optimal rendering performance.
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([call_stateful/3]).
-export([call_stateless/4]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

-ignore_xref([call_stateful/3]).
-ignore_xref([call_stateless/4]).

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-doc ~"""
Invoke a stateful component with lifecycle management.

Handles the complete lifecycle of stateful components including state lookup,
binding updates, remount detection, and callback invocation. Manages existing
component state and performs remounting when necessary.

## Component Lifecycle

1. **State Lookup**: Find existing component state by ID
2. **Binding Update**: Apply new bindings to component state
3. **Remount Check**: Determine if component needs remounting
4. **Callback Invocation**: Call appropriate lifecycle callbacks
5. **Render**: Generate component output

## Examples

```erlang
1> Bindings = #{name => ~"John", age => 30}.
#{name => ~"John", age => 30}
2> arizona_component:call_stateful(user_component, Bindings, Socket).
#socket{stateful_states = #{root => #state{...}}, ...}
```

## Remount Behavior

Components are remounted when their fingerprint changes, ensuring consistency
when component structure or critical bindings change.
""".
-spec call_stateful(Mod, Bindings, Socket) -> Socket1 when
    Mod :: module(),
    Bindings :: arizona_socket:bindings(),
    Socket :: arizona_socket:socket(),
    Socket1 :: arizona_socket:socket().
call_stateful(Mod, Bindings, Socket) ->
    Id = arizona_socket:get_current_stateful_id(Socket),
    case arizona_socket:find_stateful_state(Id, Socket) of
        {ok, State} ->
            %% Apply new bindings to existing state before checking remount
            UpdatedState = arizona_stateful:put_bindings(Bindings, State),
            case arizona_stateful:should_remount(UpdatedState) of
                true ->
                    Socket1 = arizona_stateful:call_unmount_callback(Mod, Socket),
                    %% Call mount callback after unmount for remount
                    Socket2 = arizona_stateful:call_mount_callback(Mod, Socket1),
                    %% Call the component's render callback which handles
                    %% rendering and returns updated socket
                    arizona_stateful:call_render_callback(Mod, Socket2);
                false ->
                    %% Update socket with new state and call render callback (which handles diffing)
                    Socket1 = arizona_socket:put_stateful_state(UpdatedState, Socket),
                    arizona_stateful:call_render_callback(Mod, Socket1)
            end;
        error ->
            State = arizona_stateful:new(Id, Mod, Bindings),
            Socket1 = arizona_socket:put_stateful_state(State, Socket),
            %% Call mount callback for new components
            Socket2 = arizona_stateful:call_mount_callback(Mod, Socket1),
            %% Call the component's render callback which handles
            %% rendering and returns updated socket
            arizona_stateful:call_render_callback(Mod, Socket2)
    end.

-doc ~"""
Invoke a stateless component with temporary bindings.

Calls a stateless component function with the provided bindings in a temporary
socket context. Stateless components don't maintain state and are ideal for
simple presentational components.

## Component Execution

1. **Temporary Bindings**: Create socket with temporary binding context
2. **Function Call**: Invoke the specified component function
3. **Result**: Return socket with rendered output

## Examples

```erlang
1> Bindings = #{title => ~"Welcome", content => ~"Hello World"}.
#{title => ~"Welcome", content => ~"Hello World"}
2> arizona_component:call_stateless(card_component, render, Bindings, Socket).
#socket{html_acc = [~"<div class='card'>..."], ...}
```

## Performance

Stateless components are lightweight and don't maintain persistent state,
making them ideal for frequently rendered presentational components.
""".
-spec call_stateless(Mod, Fun, Bindings, Socket) -> Socket1 when
    Mod :: module(),
    Fun :: atom(),
    Bindings :: arizona_socket:bindings(),
    Socket :: arizona_socket:socket(),
    Socket1 :: arizona_socket:socket().
call_stateless(Mod, Fun, Bindings, Socket) ->
    %% Create temporary socket with bindings for stateless component
    TempSocket = arizona_socket:with_temp_bindings(Bindings, Socket),
    %% Call the stateless component function to get result
    arizona_stateless:call_render_callback(Mod, Fun, TempSocket).
