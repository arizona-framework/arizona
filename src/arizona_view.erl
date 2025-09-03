-module(arizona_view).
-moduledoc ~""""
View behavior definition and state management for top-level page components.

Defines the behavior for views that represent complete pages or routes
in the Arizona framework. Views manage their own state plus the states
of nested stateful components, handle WebSocket events and Erlang process
messages, and support optional layout wrapping.

## Behavior Callbacks

- `mount/2` - Initialize view from mount arg and HTTP request
- `render/1` - Generate template from current bindings
- `handle_event/3` - Handle WebSocket events (optional, raises if events triggered but not defined)
- `handle_info/2` - Handle Erlang process messages (optional)

## View State Management

View state includes:
- Optional `layout/0` configuration
- Main `arizona_stateful:state/0`
- Nested stateful component states by ID
- Template fingerprints for differential updates

## Example Implementation

```erlang
-module(home_view).
-compile({parse_transform, arizona_parse_transform}).
-behaviour(arizona_view).
-export([mount/2, render/1]).

mount(_MountArg, _Request) ->
    arizona_view:new(?MODULE, #{user => ~"Anonymous"}, none).

render(Bindings) ->
    arizona_template:from_string(~"""
    <h1>Welcome {arizona_template:get_binding(user, Bindings)}!</h1>
    """).
```
"""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([call_mount_callback/3]).
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

-callback mount(MountArg, ArizonaRequest) -> View when
    MountArg :: dynamic(),
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

-doc ~"""
Executes a view's mount callback.

Calls the module's `mount/2` function with mount argument and HTTP request
to initialize the view. Used during page navigation and initial load.
""".
-spec call_mount_callback(Module, MountArg, ArizonaRequest) -> View when
    Module :: module(),
    MountArg :: dynamic(),
    ArizonaRequest :: arizona_request:request(),
    View :: view().
call_mount_callback(Module, MountArg, ArizonaRequest) when is_atom(Module) ->
    apply(Module, mount, [MountArg, ArizonaRequest]).

-doc ~"""
Executes a view's render callback.

Calls the module's `render/1` function with current bindings to
generate the view template. Used during rendering pipeline.
""".
-spec call_render_callback(View) -> Template when
    View :: view(),
    Template :: arizona_template:template().
call_render_callback(#view{state = State}) ->
    Module = arizona_stateful:get_module(State),
    Bindings = arizona_stateful:get_bindings(State),
    apply(Module, render, [Bindings]).

-doc ~"""
Executes a view's event handler callback.

Calls the module's `handle_event/3` function to process WebSocket events.
Returns either a reply to send back or no reply with updated view.
""".
-spec call_handle_event_callback(Event, Params, View) -> Result when
    Event :: arizona_stateful:event_name(),
    Params :: arizona_stateful:event_params(),
    View :: view(),
    Result :: {reply, Reply, View1} | {noreply, View1},
    Reply :: arizona_stateful:event_reply(),
    View1 :: view().
call_handle_event_callback(Event, Params, #view{state = State} = View) ->
    Module = arizona_stateful:get_module(State),
    apply(Module, handle_event, [Event, Params, View]).

-doc ~"""
Executes a view's info handler callback.

Calls the module's `handle_info/2` function to process Erlang process
messages. Returns updated view without replies.
""".
-spec call_handle_info_callback(Info, View) -> Result when
    Info :: term(),
    View :: view(),
    Result :: {noreply, View1},
    View1 :: view().
call_handle_info_callback(Info, #view{state = State} = View) ->
    Module = arizona_stateful:get_module(State),
    apply(Module, handle_info, [Info, View]).

-doc ~"""
Creates a new view with optional layout configuration.

Initializes view with module reference, bindings, and optional layout.
Layout tuple specifies wrapper module, function, slot name, and bindings.
""".
-spec new(Module, Bindings, Layout) -> View when
    Module :: module(),
    Bindings :: arizona_binder:map(),
    Layout :: layout() | none,
    View :: view().
new(Module, Bindings, Layout) when is_atom(Module), (is_tuple(Layout) orelse Layout =:= none) ->
    State = arizona_stateful:new(Module, Bindings),
    #view{
        layout = Layout,
        state = State,
        stateful_states = #{},
        fingerprints = #{}
    }.

-doc ~"""
Returns the view's layout configuration.

Returns `layout/0` or `none` if no layout is configured.
""".
-spec get_layout(View) -> Layout when
    View :: view(),
    Layout :: layout() | none.
get_layout(#view{} = View) ->
    View#view.layout.

-doc ~"""
Returns the view's main stateful state.

Provides access to the primary view state for binding operations.
""".
-spec get_state(View) -> State when
    View :: view(),
    State :: arizona_stateful:state().
get_state(#view{} = View) ->
    View#view.state.

-doc ~"""
Updates the view's main stateful state.

Replaces the current main state with the provided state.
""".
-spec update_state(State, View) -> View1 when
    State :: arizona_stateful:state(),
    View :: view(),
    View1 :: view().
update_state(State, #view{} = View) ->
    View#view{state = State}.

-doc ~"""
Gets a nested stateful component state by ID, throwing if not found.

Returns the state for the specified component ID. Raises `{badkey, Id}`
exception if the component is not found.
""".
-spec get_stateful_state(Id, View) -> StatefulState when
    Id :: arizona_stateful:id(),
    View :: view(),
    StatefulState :: arizona_stateful:state().
get_stateful_state(Id, #view{} = View) when is_binary(Id) ->
    maps:get(Id, View#view.stateful_states).

-doc ~"""
Safely looks up a nested stateful component state by ID.

Returns `{ok, State}` if the component exists, or `error` if not found.
Provides exception-free lookup for optional components.
""".
-spec find_stateful_state(Id, View) -> {ok, StatefulState} | error when
    Id :: arizona_stateful:id(),
    View :: view(),
    StatefulState :: arizona_stateful:state().
find_stateful_state(Id, #view{} = View) when is_binary(Id) ->
    maps:find(Id, View#view.stateful_states).

-doc ~"""
Stores a nested stateful component state by ID.

Adds or updates the component state in the view's stateful states map.
""".
-spec put_stateful_state(Id, State, View) -> View1 when
    Id :: arizona_stateful:id(),
    State :: arizona_stateful:state(),
    View :: view(),
    View1 :: view().
put_stateful_state(Id, State, #view{} = View) ->
    States = View#view.stateful_states,
    View#view{stateful_states = States#{Id => State}}.

-doc ~"""
Checks if a stored template fingerprint matches the given fingerprint.

Returns `true` if the component and element have a matching fingerprint,
`false` otherwise. Used to determine if differential updates are needed.
""".
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

-doc ~"""
Stores a template fingerprint for a component element.

Saves the fingerprint for future differential update comparisons.
Creates nested maps as needed for new components.
""".
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

-doc ~"""
Removes a template fingerprint for a component element.

Clears the stored fingerprint, typically when elements are removed
or templates change significantly. No-op if fingerprint doesn't exist.
""".
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
