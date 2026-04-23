-module(arizona_handler).
-moduledoc """
Shared behaviour for callbacks common to `arizona_stateful` and
`arizona_view` handlers.

Declares the lifecycle callbacks that both a stateful component and
a route-level view implement identically: `render/1` (required) plus
the optional `handle_event/3`, `handle_info/2`, and `unmount/1`.
Mount-specific callbacks live in the specialized behaviour modules
because their signatures differ (`mount/1` for stateful,
`mount/2` for view). Parent-update interception
(`handle_update/2`) lives in `arizona_stateful` because views don't
receive it.

User modules declare both behaviours via the headers:

```erlang
%% A stateful component:
-include_lib("arizona/include/arizona_stateful.hrl").
%% expands to -behaviour(arizona_handler) + -behaviour(arizona_stateful)

%% A route-level view:
-include_lib("arizona/include/arizona_view.hrl").
%% expands to -behaviour(arizona_handler) + -behaviour(arizona_view)
```
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([call_render/2]).
-export([call_handle_event/4]).
-export([call_handle_info/3]).
-export([call_handle_update/3]).
-export([call_unmount/2]).

%% --------------------------------------------------------------------
%% Behaviour callbacks
%% --------------------------------------------------------------------

-doc """
Returns the template for the current bindings. Called on the first
render and after every state change.

Must use `?html(...)` so the parse transform can compile it into a
template map.
""".
-callback render(Bindings :: arizona_stateful:bindings()) ->
    arizona_template:template().

-doc """
Handles a client UI event. Optional.

`Event` is the event name dispatched from `az-click`, `az-submit`,
etc. `Payload` carries any auto-collected form data plus explicit
extra fields. Returns updated bindings, resets, and a list of
client-side effects to run after the diff applies.
""".
-callback handle_event(
    arizona_stateful:event_name(),
    arizona_stateful:event_payload(),
    arizona_stateful:bindings()
) -> arizona_stateful:handle_event_ret().

-doc """
Handles a mailbox message. Optional.
""".
-callback handle_info(Info :: term(), arizona_stateful:bindings()) ->
    arizona_stateful:handle_info_ret().

-doc """
Intercepts a parent prop update before it reaches the bindings. Optional.

When a parent re-renders and a child receives new props, this callback
gets the chance to merge / transform them. If not exported, the
framework merges `Props` into `Bindings` directly.

Only fires on `arizona_stateful` instances today -- views are
route-mounted and never receive parent prop updates. Kept in the
shared behaviour so the contract is unified if a future
view-embedding or patch-style URL primitive needs the same
reconciliation hook.
""".
-callback handle_update(Props :: arizona_stateful:bindings(), arizona_stateful:bindings()) ->
    arizona_stateful:handle_update_ret().

-doc """
Cleanup hook called when the instance is removed. Optional.
""".
-callback unmount(arizona_stateful:bindings()) -> term().

-optional_callbacks([handle_event/3, handle_info/2, handle_update/2, unmount/1]).

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc """
Invokes the required `render/1` callback on a handler module.
""".
-spec call_render(Handler, Bindings) -> arizona_template:template() when
    Handler :: module(),
    Bindings :: arizona_stateful:bindings().
call_render(H, Bindings) ->
    H:render(Bindings).

-doc """
Invokes the optional `handle_event/3` callback.

The callback is declared optional but callers that dispatch an event
expect it to be exported -- an unhandled event is a programming error
and will crash with `undef`.
""".
-spec call_handle_event(Handler, Event, Payload, Bindings) ->
    {arizona_stateful:bindings(), arizona_stateful:resets(), arizona_stateful:effects()}
when
    Handler :: module(),
    Event :: binary(),
    Payload :: map(),
    Bindings :: arizona_stateful:bindings().
call_handle_event(H, Event, Payload, Bindings) ->
    H:handle_event(Event, Payload, Bindings).

-doc """
Invokes the optional `handle_info/2` callback.

Returns `ok` if the handler does not export `handle_info/2`, letting
the caller skip the subsequent diff + push cycle.
""".
-spec call_handle_info(Handler, Info, Bindings) ->
    {arizona_stateful:bindings(), arizona_stateful:resets(), arizona_stateful:effects()} | ok
when
    Handler :: module(),
    Info :: term(),
    Bindings :: arizona_stateful:bindings().
call_handle_info(H, Info, Bindings) ->
    case erlang:function_exported(H, handle_info, 2) of
        true -> H:handle_info(Info, Bindings);
        false -> ok
    end.

-doc """
Invokes the optional `handle_update/2` callback.

Falls back to merging `Props` into `Bindings` if the callback is not
exported.
""".
-spec call_handle_update(Handler, Props, Bindings) ->
    arizona_stateful:handle_update_ret()
when
    Handler :: module(),
    Props :: map(),
    Bindings :: arizona_stateful:bindings().
call_handle_update(H, Props, Bindings) ->
    case erlang:function_exported(H, handle_update, 2) of
        true -> H:handle_update(Props, Bindings);
        false -> {maps:merge(Bindings, Props), #{}}
    end.

-doc """
Invokes the optional `unmount/1` callback on a handler module.

No-op if the callback is not exported.
""".
-spec call_unmount(Handler, Bindings) -> ok when
    Handler :: module(),
    Bindings :: arizona_stateful:bindings().
call_unmount(H, Bindings) ->
    case erlang:function_exported(H, unmount, 1) of
        true -> H:unmount(Bindings);
        false -> ok
    end.
