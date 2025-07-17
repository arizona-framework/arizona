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
-export([is_socket/1]).
-export([get_mode/1]).
-export([set_mode/2]).
-export([get_current_stateful_id/1]).
-export([get_current_stateful_state/1]).
-export([get_stateful_states/1]).
-export([get_stateful_state/2]).
-export([find_stateful_state/2]).
-export([set_current_stateful_id/2]).
-export([put_stateful_state/2]).
-export([set_html_acc/2]).
-export([get_html/1]).
-export([append_changes/2]).
-export([get_changes/1]).
-export([clear_changes/1]).
-export([set_hierarchical_acc/2]).
-export([get_hierarchical_acc/1]).
-export([put_hierarchical_acc/3]).
-export([set_hierarchical_pending_element/2]).
-export([get_hierarchical_pending_element/1]).
-export([clear_hierarchical_pending_element/1]).
-export([get_binding/2]).
-export([get_binding/3]).
-export([put_binding/3]).
-export([put_bindings/2]).
-export([with_temp_bindings/2]).
-export([get_temp_binding/2]).
-export([set_layout/2]).
-export([get_layout/1]).
-export([set_live_pid/2]).
-export([get_live_pid/1]).
-export([notify_current_stateful_id/2]).
-export([notify_current_element_index/2]).
-export([notify_variable_dependency/2]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

-ignore_xref([get_stateful_state/2]).
-ignore_xref([get_stateful_states/1]).
-ignore_xref([get_temp_binding/2]).
-ignore_xref([put_binding/3]).
-ignore_xref([set_current_stateful_id/2]).
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
    html_acc :: iolist(),
    changes_acc :: arizona_differ:diff_changes(),
    hierarchical_acc :: arizona_hierarchical:hierarchical_structure(),
    hierarchical_pending_element :: arizona_hierarchical:element_content() | undefined,
    current_stateful_parent_id :: arizona_stateful:id() | undefined,
    current_stateful_id :: arizona_stateful:id(),
    stateful_states :: #{arizona_stateful:id() => arizona_stateful:state()},
    % For stateless component bindings, always a map
    temp_bindings :: map(),
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
        html_acc = [],
        changes_acc = [],
        hierarchical_acc = #{},
        hierarchical_pending_element = undefined,
        current_stateful_parent_id = maps:get(current_stateful_parent_id, Opts, undefined),
        current_stateful_id = maps:get(current_stateful_id, Opts, root),
        stateful_states = #{},
        temp_bindings = #{},
        layout = undefined,
        live_pid = maps:get(live_pid, Opts, undefined)
    }.

-doc ~"""
Check if a term is a socket.

Type guard function to determine if a given term is a valid socket record.

## Examples

```erlang
1> Socket = arizona_socket:new(#{}).
#socket{...}
2> arizona_socket:is_socket(Socket).
true
3> arizona_socket:is_socket(not_a_socket).
false
```
""".
-spec is_socket(Term) -> boolean() when
    Term :: term().
is_socket(#socket{}) -> true;
is_socket(_) -> false.

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
Get the current stateful component ID from the socket.

Returns the ID of the stateful component currently being processed.

## Examples

```erlang
1> Socket = arizona_socket:new(#{current_stateful_id => my_component}).
#socket{...}
2> arizona_socket:get_current_stateful_id(Socket).
my_component
```
""".
-spec get_current_stateful_id(Socket) -> Id when
    Socket :: socket(),
    Id :: arizona_stateful:id().
get_current_stateful_id(#socket{} = Socket) ->
    Socket#socket.current_stateful_id.

-doc ~"""
Get the current stateful component state from the socket.

Returns the state of the current stateful component being processed.

## Examples

```erlang
1> Socket = arizona_socket:new(#{current_stateful_id => root}).
#socket{...}
2> arizona_socket:get_current_stateful_state(Socket).
#{id => root, bindings => #{}, ...}
```
""".
-spec get_current_stateful_state(Socket) -> StatefulState when
    Socket :: socket(),
    StatefulState :: arizona_stateful:state().
get_current_stateful_state(#socket{} = Socket) ->
    Id = get_current_stateful_id(Socket),
    get_stateful_state(Id, Socket).

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
Set the current stateful component ID in the socket.

Updates the socket to track a different stateful component as the current one.

## Examples

```erlang
1> Socket = arizona_socket:new(#{current_stateful_id => root}).
#socket{...}
2> Socket2 = arizona_socket:set_current_stateful_id(my_component, Socket).
#socket{current_stateful_id = my_component, ...}
```
""".
-spec set_current_stateful_id(Id, Socket) -> Socket1 when
    Id :: arizona_stateful:id(),
    Socket :: socket(),
    Socket1 :: socket().
set_current_stateful_id(Id, #socket{} = Socket) when Id =:= root; is_binary(Id) ->
    Socket#socket{current_stateful_id = Id}.

-doc ~"""
Set the HTML accumulator in the socket.

Updates the socket with HTML content that has been accumulated during rendering.

## Examples

```erlang
1> Socket = arizona_socket:new(#{}).
#socket{...}
2> Socket2 = arizona_socket:set_html_acc([~"Hello", ~"World"], Socket).
#socket{html_acc = [~"Hello", ~"World"], ...}
```
""".
-spec set_html_acc(Html, Socket) -> Socket1 when
    Html :: arizona_html:html(),
    Socket :: socket(),
    Socket1 :: socket().
set_html_acc(Html, #socket{} = Socket) when is_list(Html) ->
    Socket#socket{html_acc = Html}.

-doc ~"""
Get the HTML accumulator from the socket.

Returns the HTML content that has been accumulated during rendering.

## Examples

```erlang
1> Socket = arizona_socket:new(#{}).
#socket{...}
2> arizona_socket:get_html(Socket).
[]
```
""".
-spec get_html(Socket) -> Html when
    Socket :: socket(),
    Html :: arizona_html:html().
get_html(#socket{} = Socket) ->
    Socket#socket.html_acc.

-doc ~"""
Set the hierarchical accumulator in the socket.

Updates the socket with a hierarchical structure for hierarchical rendering mode.

## Examples

```erlang
1> Socket = arizona_socket:new(#{}).
#socket{...}
2> Structure = #{root => #{0 => ~"content"}}.
#{root => #{0 => ~"content"}}
3> Socket2 = arizona_socket:set_hierarchical_acc(Structure, Socket).
#socket{hierarchical_acc = #{root => #{0 => ~"content"}}, ...}
```
""".
-spec set_hierarchical_acc(HierarchicalStructure, Socket) -> Socket1 when
    HierarchicalStructure :: arizona_hierarchical:hierarchical_structure(),
    Socket :: socket(),
    Socket1 :: socket().
set_hierarchical_acc(HierarchicalStructure, #socket{} = Socket) when
    is_map(HierarchicalStructure)
->
    Socket#socket{hierarchical_acc = HierarchicalStructure}.

-doc ~"""
Get the hierarchical accumulator from the socket.

Returns the hierarchical structure that has been accumulated during rendering.

## Examples

```erlang
1> Socket = arizona_socket:new(#{}).
#socket{...}
2> arizona_socket:get_hierarchical_acc(Socket).
#{}
```
""".
-spec get_hierarchical_acc(Socket) -> HierarchicalStructure when
    Socket :: socket(),
    HierarchicalStructure :: arizona_hierarchical:hierarchical_structure().
get_hierarchical_acc(#socket{} = Socket) ->
    Socket#socket.hierarchical_acc.

put_hierarchical_acc(StatefulId, Struct, #socket{} = Socket) ->
    Acc = Socket#socket.hierarchical_acc,
    Socket#socket{hierarchical_acc = Acc#{StatefulId => Struct}}.

-doc ~"""
Set a pending hierarchical element in the socket.

Stores an element that is pending inclusion in the hierarchical structure.

## Examples

```erlang
1> Socket = arizona_socket:new(#{}).
#socket{...}
2> Element = #{type => stateless, structure => #{0 => ~"content"}}.
#{type => stateless, structure => #{0 => ~"content"}}
3> Socket2 = arizona_socket:set_hierarchical_pending_element(Element, Socket).
#socket{hierarchical_pending_element = #{type => stateless, ...}, ...}
```
""".
-spec set_hierarchical_pending_element(Element, Socket) -> Socket1 when
    Element :: arizona_hierarchical:element_content(),
    Socket :: socket(),
    Socket1 :: socket().
set_hierarchical_pending_element(Element, #socket{} = Socket) ->
    Socket#socket{hierarchical_pending_element = Element}.

-doc ~"""
Get the pending hierarchical element from the socket.

Returns the element that is pending inclusion in the hierarchical structure,
or `undefined` if no element is pending.

## Examples

```erlang
1> Socket = arizona_socket:new(#{}).
#socket{...}
2> arizona_socket:get_hierarchical_pending_element(Socket).
undefined
```
""".
-spec get_hierarchical_pending_element(Socket) -> Element when
    Socket :: socket(),
    Element :: arizona_hierarchical:element_content() | undefined.
get_hierarchical_pending_element(#socket{} = Socket) ->
    Socket#socket.hierarchical_pending_element.

-doc ~"""
Clear the pending hierarchical element from the socket.

Resets the pending element to `undefined`.

## Examples

```erlang
1> Socket = arizona_socket:set_hierarchical_pending_element(Element, Socket).
#socket{hierarchical_pending_element = #{...}, ...}
2> Socket2 = arizona_socket:clear_hierarchical_pending_element(Socket).
#socket{hierarchical_pending_element = undefined, ...}
```
""".
-spec clear_hierarchical_pending_element(Socket) -> Socket1 when
    Socket :: socket(),
    Socket1 :: socket().
clear_hierarchical_pending_element(#socket{} = Socket) ->
    Socket#socket{hierarchical_pending_element = undefined}.

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
Get a binding value from the socket.

Retrieves a binding value, first checking temporary bindings (for stateless components)
and falling back to stateful component bindings.

## Examples

```erlang
1> Socket = arizona_socket:new(#{}).
#socket{...}
2> Socket2 = arizona_socket:put_binding(name, ~"John", Socket).
#socket{...}
3> arizona_socket:get_binding(name, Socket2).
~"John"
```
""".
-spec get_binding(Key, Socket) -> Value when
    Key :: atom(),
    Socket :: socket(),
    Value :: term().
get_binding(Key, #socket{} = Socket) when is_atom(Key) ->
    %% First try temporary bindings (for stateless components)
    case Socket#socket.temp_bindings of
        #{Key := Value} ->
            Value;
        #{} ->
            %% Fall back to stateful bindings
            CurrentState = get_current_stateful_state(Socket),
            arizona_stateful:get_binding(Key, CurrentState)
    end.

-doc ~"""
Get a binding value from the socket with a default value.

Retrieves a binding value, returning the default if the binding is not found.

## Examples

```erlang
1> Socket = arizona_socket:new(#{}).
#socket{...}
2> arizona_socket:get_binding(nonexistent, Socket, ~"default").
~"default"
```
""".
-spec get_binding(Key, Socket, Default) -> Value when
    Key :: atom(),
    Socket :: socket(),
    Default :: term(),
    Value :: term() | Default.
get_binding(Key, #socket{} = Socket, Default) when is_atom(Key) ->
    %% First try temporary bindings (for stateless components)
    case Socket#socket.temp_bindings of
        #{Key := Value} ->
            Value;
        #{} ->
            %% Fall back to stateful bindings
            CurrentState = get_current_stateful_state(Socket),
            arizona_stateful:get_binding(Key, CurrentState, Default)
    end.

-doc ~"""
Set temporary bindings for stateless components.

Replaces the temporary bindings map with the provided bindings.
Used for stateless component rendering.

## Examples

```erlang
1> Socket = arizona_socket:new(#{}).
#socket{...}
2> Bindings = #{item => ~"value"}.
#{item => ~"value"}
3> Socket2 = arizona_socket:with_temp_bindings(Bindings, Socket).
#socket{temp_bindings = #{item => ~"value"}, ...}
```
""".
-spec with_temp_bindings(Bindings, Socket) -> Socket1 when
    Bindings :: bindings(),
    Socket :: socket(),
    Socket1 :: socket().
with_temp_bindings(Bindings, #socket{} = Socket) when is_map(Bindings) ->
    Socket#socket{temp_bindings = Bindings}.

-doc ~"""
Get a temporary binding value from the socket.

Retrieves a temporary binding value, throwing an error if not found.
Used for stateless component rendering.

## Examples

```erlang
1> Socket = arizona_socket:with_temp_bindings(#{item => ~"value"}, Socket).
#socket{...}
2> arizona_socket:get_temp_binding(item, Socket).
~"value"
3> arizona_socket:get_temp_binding(missing, Socket).
** exception throw: {binding_not_found,missing}
```
""".
-spec get_temp_binding(Key, Socket) -> Value when
    Key :: atom(),
    Socket :: socket(),
    Value :: term().
get_temp_binding(Key, #socket{} = Socket) when is_atom(Key) ->
    case Socket#socket.temp_bindings of
        #{Key := Value} -> Value;
        #{} -> throw({binding_not_found, Key})
    end.

-doc ~"""
Put a binding value into the socket.

Stores a binding value in the current stateful component's state.

## Examples

```erlang
1> Socket = arizona_socket:new(#{}).
#socket{...}
2> Socket2 = arizona_socket:put_binding(name, ~"John", Socket).
#socket{...}
3> arizona_socket:get_binding(name, Socket2).
~"John"
```
""".
-spec put_binding(Key, Value, Socket) -> Socket1 when
    Key :: atom(),
    Value :: term(),
    Socket :: socket(),
    Socket1 :: socket().
put_binding(Key, Value, #socket{} = Socket) when is_atom(Key) ->
    CurrentState = get_current_stateful_state(Socket),
    UpdatedState = arizona_stateful:put_binding(Key, Value, CurrentState),
    put_stateful_state(UpdatedState, Socket).

-doc ~"""
Put multiple binding values into the socket.

Stores multiple binding values in the current stateful component's state.

## Examples

```erlang
1> Socket = arizona_socket:new(#{}).
#socket{...}
2> Bindings = #{name => ~"John", age => 30}.
#{name => ~"John", age => 30}
3> Socket2 = arizona_socket:put_bindings(Bindings, Socket).
#socket{...}
```
""".
-spec put_bindings(Bindings, Socket) -> Socket1 when
    Bindings :: bindings(),
    Socket :: socket(),
    Socket1 :: socket().
put_bindings(Bindings, #socket{} = Socket) when is_map(Bindings) ->
    CurrentState = get_current_stateful_state(Socket),
    UpdatedState = arizona_stateful:put_bindings(Bindings, CurrentState),
    put_stateful_state(UpdatedState, Socket).

%% Changes accumulator functions (for diff mode)

-doc ~"""
Append changes to the socket's change accumulator.

Merges new changes with existing changes in the socket for diff mode operations.

## Examples

```erlang
1> Socket = arizona_socket:new(#{}).
#socket{...}
2> Changes = [{root, [{0, ~"new_content"}]}].
[{root, [{0, ~"new_content"}]}]
3> Socket2 = arizona_socket:append_changes(Changes, Socket).
#socket{changes_acc = [{root, [{0, ~"new_content"}]}], ...}
```
""".
-spec append_changes(Changes, Socket) -> Socket1 when
    Changes :: arizona_differ:diff_changes(),
    Socket :: socket(),
    Socket1 :: socket().
append_changes(Changes, #socket{} = Socket) when is_list(Changes) ->
    CurrentChanges = Socket#socket.changes_acc,
    % Merge changes at the correct hierarchical path
    MergedChanges = merge_changes(Changes, CurrentChanges),
    Socket#socket{changes_acc = MergedChanges}.

-doc ~"""
Get the accumulated changes from the socket.

Returns the changes that have been accumulated during diff mode operations,
ordered chronologically.

## Examples

```erlang
1> Socket = arizona_socket:new(#{}).
#socket{...}
2> arizona_socket:get_changes(Socket).
[]
```
""".
-spec get_changes(Socket) -> Changes when
    Socket :: socket(),
    Changes :: arizona_differ:diff_changes().
get_changes(#socket{} = Socket) ->
    % Reverse to get changes in chronological order
    lists:reverse(Socket#socket.changes_acc).

-doc ~"""
Clear the changes accumulator in the socket.

Resets the changes accumulator to an empty state.

## Examples

```erlang
1> Socket = arizona_socket:append_changes([{root, [{0, ~"change"}]}], Socket).
#socket{changes_acc = [...], ...}
2> Socket2 = arizona_socket:clear_changes(Socket).
#socket{changes_acc = [], ...}
```
""".
-spec clear_changes(Socket) -> Socket1 when
    Socket :: socket(),
    Socket1 :: socket().
clear_changes(#socket{} = Socket) ->
    Socket#socket{changes_acc = []}.

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

%% --------------------------------------------------------------------
%% Private functions
%% --------------------------------------------------------------------

%% Merge new changes with existing changes, maintaining hierarchical structure
%% Format: [{StatefulId, [{ElementIndex, Changes}]}]
-spec merge_changes(NewChanges, ExistingChanges) -> MergedChanges when
    NewChanges :: arizona_differ:diff_changes(),
    ExistingChanges :: arizona_differ:diff_changes(),
    MergedChanges :: arizona_differ:diff_changes().
merge_changes([], ExistingChanges) ->
    ExistingChanges;
merge_changes(NewChanges, []) ->
    NewChanges;
merge_changes([{ComponentId, NewElementChanges} | RestNew], ExistingChanges) ->
    % Find if this component already has changes
    case lists:keyfind(ComponentId, 1, ExistingChanges) of
        false ->
            % Component not found, add it
            UpdatedExisting = [{ComponentId, NewElementChanges} | ExistingChanges],
            merge_changes(RestNew, UpdatedExisting);
        {ComponentId, ExistingElementChanges} ->
            % Component found, merge element changes
            MergedElementChanges = merge_element_changes(NewElementChanges, ExistingElementChanges),
            UpdatedExisting = lists:keyreplace(
                ComponentId,
                1,
                ExistingChanges,
                {ComponentId, MergedElementChanges}
            ),
            merge_changes(RestNew, UpdatedExisting)
    end.

%% Merge element changes within the same component
-spec merge_element_changes(NewElementChanges, ExistingElementChanges) ->
    MergedElementChanges
when
    NewElementChanges :: [arizona_differ:element_change_entry()],
    ExistingElementChanges :: [arizona_differ:element_change_entry()],
    MergedElementChanges :: [arizona_differ:element_change_entry()].
merge_element_changes([], ExistingElements) ->
    ExistingElements;
merge_element_changes([{ElementIndex, NewChange} | RestNew], ExistingElements) ->
    case lists:keyfind(ElementIndex, 1, ExistingElements) of
        false ->
            % Element not found, add it
            UpdatedExisting = [{ElementIndex, NewChange} | ExistingElements],
            merge_element_changes(RestNew, UpdatedExisting);
        {ElementIndex, ExistingChange} ->
            % Element found, merge the changes using arizona_differ
            MergedChange = arizona_differ:merge_element_changes(NewChange, ExistingChange),
            UpdatedExisting = lists:keyreplace(
                ElementIndex,
                1,
                ExistingElements,
                {ElementIndex, MergedChange}
            ),
            merge_element_changes(RestNew, UpdatedExisting)
    end.

%% --------------------------------------------------------------------
%% Live PID and Dependency Tracking Functions
%% --------------------------------------------------------------------

-doc ~"""
Set the live process PID for dependency tracking.

Associates the socket with a live process that will receive dependency
tracking notifications.
""".
-spec set_live_pid(pid(), socket()) -> socket().
set_live_pid(LivePid, Socket) ->
    Socket#socket{live_pid = LivePid}.

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
