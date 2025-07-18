-module(arizona_socket).
-moduledoc ~"""
Provides socket management functionality for Arizona rendering operations.

## Overview

The socket module manages the state and context during template rendering operations.
It maintains rendering modes, HTML accumulation, hierarchical structures, stateful component
states, and temporary bindings throughout the rendering process.

## Features

- **Rendering Modes**: Support for render, diff, and hierarchical modes
- **State Management**: Tracks stateful component states and their relationships
- **Hierarchical Rendering**: Manages structured data for real-time WebSocket updates
- **Binding System**: Handles both stateful and temporary bindings for components
- **Layout Support**: Manages layout components and their slots
- **Change Tracking**: Accumulates changes for efficient diff operations

## Key Functions

- `new/1`: Create a new socket with specified options
- `get_mode/1`, `set_mode/2`: Manage rendering modes
- `get_binding/2`, `put_binding/3`: Handle component bindings
- `get_html/1`, `set_html_acc/2`: Manage HTML accumulation
- `get_hierarchical_acc/1`: Access hierarchical structures
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([new/1]).
-export([get_mode/1]).
-export([set_mode/2]).
-export([get_stateful_states/1]).
-export([get_stateful_state/2]).
-export([put_stateful_state/2]).
-export([find_stateful_state/2]).
-export([get_layout/1]).
-export([set_layout/2]).
-export([get_live_pid/1]).
-export([set_live_pid/2]).

%% --------------------------------------------------------------------
%% Live PID and Dependency Tracking Functions
%% --------------------------------------------------------------------

-export([notify_current_stateful_id/2]).
-export([notify_current_element_index/2]).
-export([notify_variable_dependency/2]).
-export([clear_component_dependencies/2]).
-export([put_hierarchical/3]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

-ignore_xref([get_stateful_state/2]).
-ignore_xref([get_stateful_states/1]).
-ignore_xref([set_layout/2]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([socket/0]).
-export_type([bindings/0]).
-export_type([mode/0]).
-export_type([layout/0]).
-export_type([socket_opts/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-record(socket, {
    mode :: mode(),
    stateful_states :: #{arizona_stateful:id() => arizona_stateful:state()},
    layout :: layout() | undefined,
    live_pid :: pid() | undefined
}).

-doc ~"""
Opaque socket type representing the rendering context and state.

Contains all necessary information for template rendering including mode,
accumulators, stateful states, and temporary bindings.
""".
-opaque socket() :: #socket{}.

-doc ~"""
Map of bindings for component variables.

Used to store key-value pairs where keys are atoms representing variable names
and values are any Erlang term.
""".
-type bindings() :: #{atom() => term()}.

-doc ~"""
Rendering mode for socket operations.

- `render`: Standard HTML rendering mode
- `diff`: Differential rendering for change detection
- `hierarchical`: Structured rendering for WebSocket updates
""".
-type mode() :: render | diff | hierarchical.

-doc ~"""
Layout component specification.

Defines a layout with its module, render function, and slot name for content placement.
""".
-type layout() :: {LayoutModule :: atom(), LayoutRenderFun :: atom(), SlotName :: atom()}.

-doc ~"""
Options for socket creation.

Allows configuration of rendering mode and stateful component context.
""".
-type socket_opts() :: #{
    mode => mode(),
    current_stateful_parent_id => arizona_stateful:id() | undefined,
    current_stateful_id => arizona_stateful:id()
}.

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-doc ~"""
Create a new socket with the specified options.

Initializes a socket with default values and applies any provided options.
The socket maintains rendering state and context throughout template operations.

## Examples

```erlang
1> arizona_socket:new(#{}).
#socket{mode = render, current_stateful_id = root, ...}
2> arizona_socket:new(#{mode => hierarchical, current_stateful_id => my_component}).
#socket{mode = hierarchical, current_stateful_id = my_component, ...}
```
""".
-spec new(Opts) -> Socket when
    Opts :: socket_opts(),
    Socket :: socket().
new(Opts) when is_map(Opts) ->
    #socket{
        mode = maps:get(mode, Opts, render),
        stateful_states = #{},
        layout = undefined,
        live_pid = maps:get(live_pid, Opts, undefined)
    }.

-doc ~"""
Get the current rendering mode from the socket.

Returns the current rendering mode which determines how templates are processed.

## Examples

```erlang
1> Socket = arizona_socket:new(#{mode => hierarchical}).
#socket{...}
2> arizona_socket:get_mode(Socket).
hierarchical
```
""".
-spec get_mode(Socket) -> Mode when
    Socket :: socket(),
    Mode :: mode().
get_mode(#socket{} = Socket) ->
    Socket#socket.mode.

-doc ~"""
Set the rendering mode in the socket.

Updates the socket with a new rendering mode. The mode affects how templates
are processed and what data structures are generated.

## Examples

```erlang
1> Socket = arizona_socket:new(#{}).
#socket{mode = render, ...}
2> Socket2 = arizona_socket:set_mode(hierarchical, Socket).
#socket{mode = hierarchical, ...}
```
""".
-spec set_mode(Mode, Socket) -> Socket1 when
    Mode :: mode(),
    Socket :: socket(),
    Socket1 :: socket().
set_mode(Mode, #socket{} = Socket) when Mode =:= render; Mode =:= diff; Mode =:= hierarchical ->
    Socket#socket{mode = Mode}.

-doc ~"""
Get a specific stateful component state by ID.

Returns the state of a stateful component with the given ID.

## Examples

```erlang
1> Socket = arizona_socket:new(#{}).
#socket{...}
2> arizona_socket:get_stateful_state(root, Socket).
#{id => root, bindings => #{}, ...}
```
""".
-spec get_stateful_state(Id, Socket) -> StatefulState when
    Id :: arizona_stateful:id(),
    Socket :: socket(),
    StatefulState :: arizona_stateful:state().
get_stateful_state(Id, #socket{} = Socket) when Id =:= root; is_binary(Id) ->
    maps:get(Id, Socket#socket.stateful_states).

-doc ~"""
Get all stateful component states from the socket.

Returns a map of all stateful component states indexed by their IDs.

## Examples

```erlang
1> Socket = arizona_socket:new(#{}).
#socket{...}
2> arizona_socket:get_stateful_states(Socket).
#{root => #{id => root, bindings => #{}, ...}}
```
""".
-spec get_stateful_states(Socket) -> States when
    Socket :: socket(),
    States :: #{arizona_stateful:id() => arizona_stateful:state()}.
get_stateful_states(#socket{} = Socket) ->
    Socket#socket.stateful_states.

-doc ~"""
Find a stateful component state by ID.

Safely looks up a stateful component state, returning `{ok, State}` if found
or `error` if not found.

## Examples

```erlang
1> Socket = arizona_socket:new(#{}).
#socket{...}
2> arizona_socket:find_stateful_state(root, Socket).
{ok, #{id => root, bindings => #{}, ...}}
3> arizona_socket:find_stateful_state(nonexistent, Socket).
error
```
""".
-spec find_stateful_state(Id, Socket) -> {ok, StatefulState} | error when
    Id :: arizona_stateful:id(),
    Socket :: socket(),
    StatefulState :: arizona_stateful:state().
find_stateful_state(Id, #socket{} = Socket) when Id =:= root; is_binary(Id) ->
    maps:find(Id, Socket#socket.stateful_states).

-doc ~"""
Store a stateful component state in the socket.

Adds or updates a stateful component state in the socket's state map.

## Examples

```erlang
1> Socket = arizona_socket:new(#{}).
#socket{...}
2> State = arizona_stateful:new(my_component, #{}).
#{id => my_component, ...}
3> Socket2 = arizona_socket:put_stateful_state(State, Socket).
#socket{stateful_states = #{my_component => #{id => my_component, ...}}, ...}
```
""".
-spec put_stateful_state(StatefulState, Socket) -> Socket1 when
    StatefulState :: arizona_stateful:state(),
    Socket :: socket(),
    Socket1 :: socket().
put_stateful_state(State, #socket{} = Socket) ->
    Id = arizona_stateful:get_id(State),
    States = Socket#socket.stateful_states,
    Socket#socket{stateful_states = States#{Id => State}}.

-doc ~"""
Set the layout component in the socket.

Configures a layout component with its module, render function, and slot name.

## Examples

```erlang
1> Socket = arizona_socket:new(#{}).
#socket{...}
2> Layout = {my_layout, render, content}.
{my_layout, render, content}
3> Socket2 = arizona_socket:set_layout(Layout, Socket).
#socket{layout = {my_layout, render, content}, ...}
```
""".
-spec set_layout(Layout, Socket) -> Socket1 when
    Layout :: layout(),
    Socket :: socket(),
    Socket1 :: socket().
set_layout({LayoutModule, LayoutRenderFun, SlotName} = Layout, #socket{} = Socket) when
    is_atom(LayoutModule), is_atom(LayoutRenderFun), is_atom(SlotName)
->
    Socket#socket{layout = Layout}.

-doc ~"""
Get the layout component from the socket.

Returns the configured layout component, or `undefined` if no layout is set.

## Examples

```erlang
1> Socket = arizona_socket:new(#{}).
#socket{...}
2> arizona_socket:get_layout(Socket).
undefined
```
""".
-spec get_layout(Socket) -> Layout when
    Socket :: socket(),
    Layout :: layout() | undefined.
get_layout(#socket{} = Socket) ->
    Socket#socket.layout.

-doc ~"""
Set the live process PID for dependency tracking.

Associates the socket with a live process that will receive dependency
tracking notifications.
""".
-spec set_live_pid(pid(), socket()) -> socket().
set_live_pid(LivePid, Socket) ->
    Socket#socket{live_pid = LivePid}.

%% --------------------------------------------------------------------
%% Live PID and Dependency Tracking Functions
%% --------------------------------------------------------------------

-doc ~"""
Get the live process PID from the socket.

Returns the PID of the live process associated with this socket, or undefined
if no live process is set.
""".
-spec get_live_pid(socket()) -> pid() | undefined.
get_live_pid(#socket{live_pid = LivePid}) ->
    LivePid.

-doc ~"""
Notify the live process about the current stateful component.

Sends a notification to the live process (if present) about which stateful
component is currently being rendered for dependency tracking purposes.
""".
-spec notify_current_stateful_id(arizona_stateful:id(), socket()) -> ok.
notify_current_stateful_id(StatefulId, #socket{live_pid = LivePid}) ->
    case LivePid of
        undefined ->
            ok;
        _ ->
            arizona_live:set_current_stateful_id(LivePid, StatefulId)
    end.

-doc ~"""
Notify the live process about the current element index.

Sends a notification to the live process (if present) about which element
index is currently being rendered for dependency tracking purposes.
""".
-spec notify_current_element_index(non_neg_integer(), socket()) -> ok.
notify_current_element_index(ElementIndex, #socket{live_pid = LivePid}) ->
    case LivePid of
        undefined ->
            ok;
        _ ->
            arizona_live:set_current_element_index(LivePid, ElementIndex)
    end.

-doc ~"""
Notify the live process about a variable dependency.

Sends a notification to the live process (if present) that a variable
has been accessed for dependency tracking purposes.
""".
-spec notify_variable_dependency(atom(), socket()) -> ok.
notify_variable_dependency(VarName, #socket{live_pid = LivePid}) ->
    case LivePid of
        undefined ->
            ok;
        _ ->
            arizona_live:record_variable_dependency(LivePid, VarName)
    end.

-doc ~"""
Clear all dependencies for a specific component.

Sends a notification to the live process (if present) to clear all
recorded dependencies for the specified stateful component.
""".
-spec clear_component_dependencies(arizona_stateful:id(), socket()) -> ok.
clear_component_dependencies(StatefulId, #socket{live_pid = LivePid}) ->
    case LivePid of
        undefined ->
            ok;
        _ ->
            arizona_live:clear_component_dependencies(LivePid, StatefulId)
    end.

-doc ~"""
Store hierarchical data for a component.

Sends hierarchical rendering data to the live process (if present) for
accumulation. Used in hierarchical rendering mode to build structured
component representations.
""".
-spec put_hierarchical(arizona_stateful:id(), HierarchicalData, socket()) -> ok when
    HierarchicalData :: arizona_template_hierarchical:hierarchical_data().
put_hierarchical(StatefulId, HierarchicalData, #socket{live_pid = LivePid}) ->
    case LivePid of
        undefined ->
            ok;
        _ ->
            arizona_live:put_hierarchical(LivePid, StatefulId, HierarchicalData)
    end.
