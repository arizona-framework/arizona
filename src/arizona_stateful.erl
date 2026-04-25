-module(arizona_stateful).
-moduledoc """
Behaviour for stateful Arizona handlers.

A stateful handler owns a piece of UI with its own bindings, lifecycle,
and event handling. The framework instantiates one set of bindings per
mounted instance and re-renders / diffs against that instance whenever
its bindings change.

Handlers include `arizona_stateful.hrl`, which sets the behaviour and
enables the parse transform that compiles `?html(...)` calls in
`render/1` into optimized template maps.

## Required callbacks

- `mount/1` -- runs once on instance creation; returns initial bindings
  plus any reset values
- `render/1` -- returns the template for the current bindings; called
  on first render and after any change

## Optional callbacks

- `handle_event/3` -- responds to client events
- `handle_info/2` -- responds to mailbox messages
- `handle_update/2` -- intercepts parent prop updates before they
  reach the bindings map
- `unmount/1` -- cleanup hook on instance removal

## Resets and effects

`Resets` is a map of binding values that should be re-applied on top
of any modifications made by the handler -- typically used to clear
form fields or transient flags after they've been processed.

`Effects` is a list of `t:arizona_js:cmd/0` to run client-side after
the diff is applied (e.g. focus a field, dispatch a custom event).
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([call_mount/2]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([bindings/0]).
-export_type([resets/0]).
-export_type([effect/0]).
-export_type([effects/0]).
-export_type([event_name/0]).
-export_type([event_payload/0]).
-export_type([mount_ret/0]).
-export_type([handle_event_ret/0]).
-export_type([handle_info_ret/0]).
-export_type([handle_update_ret/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-type bindings() :: arizona_template:bindings().
-type resets() :: map().
-nominal effect() :: arizona_js:cmd().
-nominal effects() :: [effect()].

-type event_name() :: binary().
-type event_payload() :: map().

-type mount_ret() :: {bindings(), resets()}.
-type handle_event_ret() :: {bindings(), resets(), effects()}.
-type handle_info_ret() :: {bindings(), resets(), effects()}.
-type handle_update_ret() :: {bindings(), resets()}.

%% --------------------------------------------------------------------
%% Behaviour callbacks
%% --------------------------------------------------------------------

-doc """
Initializes a stateful instance. Called once when the framework
mounts the handler.

Returns `{Bindings, Resets}` where `Resets` is reapplied after every
subsequent `handle_event/3` / `handle_info/2` to clear transient
fields. Use the empty map `#{}` if you have nothing to reset.
""".
-callback mount(Bindings :: bindings()) -> mount_ret().

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc """
Invokes the required `mount/1` callback on a stateful handler module.
""".
-spec call_mount(Handler, Bindings) -> {bindings(), resets()} when
    Handler :: module(),
    Bindings :: bindings().
call_mount(H, Bindings) ->
    H:mount(Bindings).
