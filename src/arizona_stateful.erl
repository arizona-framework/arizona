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

-export([call_unmount/2]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([bindings/0]).
-export_type([resets/0]).
-export_type([effect/0]).
-export_type([effects/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-nominal bindings() :: map().
-nominal resets() :: map().
-nominal effect() :: arizona_js:cmd().
-nominal effects() :: [effect()].

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
-callback mount(Bindings :: bindings()) -> {bindings(), resets()}.

-doc """
Returns the template for the current bindings. Called on the first
render and after every state change.

Must use `?html(...)` so the parse transform can compile it into a
template map. The root element must carry an `id` attribute that
resolves via `?get(id)`.
""".
-callback render(Bindings :: bindings()) -> arizona_template:template().

-doc """
Handles a client UI event. Optional.

`Event` is the event name dispatched from `az-click`, `az-submit`,
etc. `Payload` carries any auto-collected form data plus explicit
extra fields. Returns updated bindings, resets, and a list of
client-side effects to run after the diff applies.
""".
-callback handle_event(Event :: binary(), Payload :: map(), Bindings :: bindings()) ->
    {bindings(), resets(), effects()}.

-doc """
Handles a mailbox message. Optional.

Triggered by anything that lands in the live process's mailbox --
`?send/1,2`, `?send_after/2,3`, pubsub broadcasts, or arbitrary
`Pid ! Msg` calls. Returns updated bindings, resets, and effects.
""".
-callback handle_info(Info :: term(), Bindings :: bindings()) ->
    {bindings(), resets(), effects()}.

-doc """
Intercepts a parent prop update before it reaches the bindings. Optional.

When a parent re-renders and its child stateful instance receives
new props, this callback gets the chance to merge / transform them.
If not exported, the framework merges `Props` into `Bindings` directly.
""".
-callback handle_update(Props :: map(), Bindings :: bindings()) -> {bindings(), resets()}.

-doc """
Cleanup hook called when the instance is removed (parent stopped
rendering it, or the page is being navigated away). Optional.
""".
-callback unmount(Bindings :: bindings()) -> ok.

-optional_callbacks([handle_event/3, handle_info/2, handle_update/2, unmount/1]).

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc """
Invokes the optional `unmount/1` callback on a stateful handler module.

No-op if the callback is not exported. Used by the live process when a
child view is removed from the parent template and by the evaluator when
a stateful slot swaps handlers at the same id.
""".
-spec call_unmount(Handler, Bindings) -> ok when
    Handler :: module(),
    Bindings :: bindings().
call_unmount(H, Bindings) ->
    case erlang:function_exported(H, unmount, 1) of
        true -> H:unmount(Bindings);
        false -> ok
    end.
