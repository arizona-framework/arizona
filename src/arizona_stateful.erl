-module(arizona_stateful).
-moduledoc """
Behaviour for stateful Arizona handlers.

A stateful handler owns a piece of UI with its own bindings, lifecycle,
and event handling. The framework instantiates one set of bindings per
mounted instance and re-renders / diffs against that instance whenever
its bindings change. The same behaviour serves both **route-level pages**
(spawned as a live-process root, with request data supplied as bindings
by the transport via `arizona_middleware:extract/1` middlewares) and
**embedded components** (instantiated from a parent template via
`?stateful(Handler, Props)`).

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
- `handle_drain/2` -- coordinates a graceful shutdown before the live
  process exits (see "Graceful drain" below)
- `unmount/1` -- cleanup hook on instance removal

## Resets and effects

`Resets` is a map of binding values that should be re-applied on top
of any modifications made by the handler -- typically used to clear
form fields or transient flags after they've been processed.

`Effects` is a list of `t:arizona_effect:cmd/0` to run client-side after
the diff is applied (e.g. focus a field, dispatch a custom event).

## Graceful drain

When the transport adapter receives a drain signal (e.g.
`roadrunner_listener:notify_drain/2` during a rolling deploy, or
`roadrunner_listener:drain/2` for a full listener shutdown), each
live handler's optional `handle_drain/2` runs. If a handler doesn't
export it, the framework defaults to exiting the live process with reason
`{shutdown, drain}` so the WebSocket closes with code 1001 (going
away). The JS client auto-reconnects (preserving any form state)
and the new live process mounts against the new server version.

To push a client-side indicator before closing:

```erlang
handle_drain(Deadline, Bindings) ->
    Remaining = Deadline - erlang:monotonic_time(millisecond),
    {stop, Bindings, [
        arizona_js:dispatch_event(~"draining",
                                  #{~"ms_remaining" => Remaining})
    ]}.
```

To stay alive past drain (e.g. a long-running export the user
explicitly chose to finish), return `{Bindings, Resets, Effects}`
like a normal `handle_info/2`. Return `ok` to acknowledge without
state change.

### `handle_drain/2` vs `unmount/1`

Both run during a graceful shutdown, but they answer different
questions and run at different points in the lifecycle:

| | `handle_drain/2` | `unmount/1` |
|---|------------------|-------------|
| **When** | Before any exit decision | After the process decides to exit |
| **Triggered by** | Listener drain broadcast only | Any exit -- crash, normal, shutdown, drain |
| **Can prevent exit?** | Yes (`{B, R, E}` keeps the handler alive) | No, process is dying |
| **Can push client effects?** | Yes, reliably before close | Racy / unreliable |
| **Has a deadline?** | Yes (integer ms timestamp) | No |
| **Purpose** | Coordinate with the client | Local cleanup |

They compose: a deploy-time drain runs `handle_drain/2` first, then
(if it returned `{stop, ...}` or wasn't exported) the live process
exits normal and `unmount/1` runs during `terminate`. Both are
optional; implement either, both, or neither.

Sequence on a deploy drain when both are implemented:

1. Listener broadcasts `{roadrunner_drain, Deadline}`.
2. Live process's `handle_drain/2` runs -- e.g. pushes a
   "reconnecting" indicator and returns `{stop, B, Effects}`.
3. Push frame leaves the WS session toward the client.
4. Live process exits with reason `{shutdown, drain}`; `terminate/2`
   calls `unmount/1` for cleanup (unsub pubsub, release ETS, etc.).
5. WS session observes the exit and sends close code 1001.
6. Client auto-reconnects to the new server version.

Why `unmount/1` can't replace `handle_drain/2`: by the time
`unmount/1` fires the process is exiting; messages sent from it
race the close frame. `handle_drain/2` runs while the process is
fully alive, so push-before-close ordering is reliable in practice
on a single node (the framework sends the push from the live
process before returning `{stop, normal, _}`, so the WS session
sees the push frame before the `EXIT` signal).

### Snapshot-template skew on post-drain reconnect

The JS client's auto-reconnect path treats the close as a transient
disconnect and preserves the current snapshot for the next session
(it adds `_az_reconnect=1` to the upgrade URL; the server skips the
fresh-mount HTML and resumes diffing). If the **deploy changed the
handler's render template**, the new server's diff baseline won't match
the client's preserved snapshot, and the first diff may misalign
(visual glitches until the next full re-render).

This isn't drain-specific -- the same risk exists for any reconnect
across a deploy -- but drain makes it more likely because clients
reconnect deterministically on every deploy. Mitigations if the
glitch is unacceptable for a given handler:

- In `handle_drain/2`, push `arizona_js:reload()` instead of
  `dispatch_event/2` to force a full page reload on the client
  (loses any client-only state but guarantees template alignment).
- Or use `arizona_js:navigate/1` to redirect the client to a known
  good route on drain.

Default behavior (no effect or a custom `dispatch_event` effect)
takes the reconnect-with-snapshot path; choose explicitly when the
handler's template churns often.
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([call_mount/2]).
-export([call_render/2]).
-export([call_handle_event/4]).
-export([call_handle_info/3]).
-export([call_handle_update/3]).
-export([call_handle_drain/3]).
-export([call_unmount/2]).
-export([format_error/2]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

-ignore_xref([format_error/2]).

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
-export_type([render_ret/0]).
-export_type([handle_event_ret/0]).
-export_type([handle_info_ret/0]).
-export_type([handle_update_ret/0]).
-export_type([handle_drain_ret/0]).
-export_type([unmount_ret/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-type bindings() :: arizona_template:bindings().
-type resets() :: map().
-nominal effect() :: arizona_effect:cmd().
-nominal effects() :: [effect()].

-type event_name() :: binary().
-type event_payload() :: map().

-type mount_ret() :: {bindings(), resets()}.
-type render_ret() :: arizona_template:template().
-type handle_event_ret() :: {bindings(), resets(), effects()}.
-type handle_info_ret() :: {bindings(), resets(), effects()}.
-type handle_update_ret() :: {bindings(), resets()}.
-type handle_drain_ret() ::
    ok
    | {stop, bindings(), effects()}
    | {bindings(), resets(), effects()}.
-type unmount_ret() :: term().

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

-doc """
Returns the template for the current bindings. Called on the first
render and after every state change.

Must use `?html(...)` so the parse transform can compile it into a
template map.
""".
-callback render(Bindings :: bindings()) ->
    render_ret().

-doc """
Handles a client UI event. Optional.

`Event` is the event name dispatched from `az-click`, `az-submit`,
etc. `Payload` carries any auto-collected form data plus explicit
extra fields. Returns updated bindings, resets, and a list of
client-side effects to run after the diff applies.
""".
-callback handle_event(
    event_name(),
    event_payload(),
    bindings()
) -> handle_event_ret().

-doc """
Handles a mailbox message. Optional.
""".
-callback handle_info(Info :: term(), bindings()) ->
    handle_info_ret().

-doc """
Intercepts a parent prop update before it reaches the bindings. Optional.

When a parent re-renders and a child receives new props, this callback
gets the chance to merge / transform them. If not exported, the
framework merges `Props` into `Bindings` directly.

Only fires on embedded instances -- a handler mounted as a route root
never receives parent prop updates.
""".
-callback handle_update(Props :: bindings(), bindings()) ->
    handle_update_ret().

-doc """
Reacts to a transport-level drain signal. Optional.

Fires when the listener broadcasts a graceful-shutdown notice
(typically during a deploy). The handler can push a "reconnecting"
indicator to the client, then return `{stop, ...}` to exit the live
process cleanly so the WebSocket closes with code 1001 ("going away")
and the client reconnects to the new server version.

`Deadline` is the millisecond timestamp by which the listener will
stop waiting. Subtract `erlang:monotonic_time(millisecond)` to get
the remaining grace window.

Returns:
- `ok` -- acknowledge but do nothing
- `{stop, Bindings, Effects}` -- push `Effects`, then exit normal
  (default when this callback is not exported, with empty effects)
- `{Bindings, Resets, Effects}` -- push diff + effects, stay alive
  (the handler opts to keep serving past drain)

Distinct from `unmount/1`: this runs **before** the exit decision and
can push client effects reliably before the close frame. `unmount/1` runs
**after**, during `terminate`, and is the right place for local
cleanup. See the "Graceful drain" section above for the full lifecycle
ordering.
""".
-callback handle_drain(Deadline :: integer(), bindings()) ->
    handle_drain_ret().

-doc """
Cleanup hook called when the instance is removed. Optional.

Fires from `terminate/2` for any exit reason -- crash, normal,
supervisor shutdown, or drain. Use for local cleanup that doesn't
need to talk to the client (release ETS tables, unsubscribe pubsub
topics, close ports, etc.). Messages sent from here race the WS
close frame; for drain-time client coordination use `handle_drain/2`
instead.
""".
-callback unmount(bindings()) -> unmount_ret().

-optional_callbacks([handle_event/3, handle_info/2, handle_update/2, handle_drain/2, unmount/1]).

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

-doc """
Invokes the required `render/1` callback on a handler module.

Re-tags the failure when the immediate stack frame is the user's
`render/1` itself: a missing export becomes `{missing_callback, H,
render, 1}` and a head-pattern mismatch becomes
`{render_no_clause, H, Bindings}`. Errors raised from inside the
callback body propagate untagged.
""".
-spec call_render(Handler, Bindings) -> arizona_template:template() when
    Handler :: module(),
    Bindings :: bindings().
call_render(H, Bindings) ->
    try
        H:render(Bindings)
    catch
        error:undef:ST ->
            arizona_error:raise_or_propagate(
                undef, ST, H, render, {missing_callback, H, render, 1}, [H, Bindings], ?MODULE
            );
        error:function_clause:ST ->
            arizona_error:raise_or_propagate(
                function_clause,
                ST,
                H,
                render,
                {render_no_clause, H, Bindings},
                [H, Bindings],
                ?MODULE
            )
    end.

-doc """
Invokes the optional `handle_event/3` callback.

Re-tags the failure when the immediate stack frame is the user's
`handle_event/3` itself: a missing export becomes
`{missing_callback, H, handle_event, 3}` and a no-matching-clause
crash becomes `{unhandled_event, H, Event, Bindings}`. Errors
raised from inside the callback body propagate untagged.
""".
-spec call_handle_event(Handler, Event, Payload, Bindings) ->
    {bindings(), resets(), effects()}
when
    Handler :: module(),
    Event :: binary(),
    Payload :: map(),
    Bindings :: bindings().
call_handle_event(H, Event, Payload, Bindings) ->
    try
        H:handle_event(Event, Payload, Bindings)
    catch
        error:undef:ST ->
            arizona_error:raise_or_propagate(
                undef,
                ST,
                H,
                handle_event,
                {missing_callback, H, handle_event, 3},
                [H, Event, Payload, Bindings],
                ?MODULE
            );
        error:function_clause:ST ->
            arizona_error:raise_or_propagate(
                function_clause,
                ST,
                H,
                handle_event,
                {unhandled_event, H, Event, Bindings},
                [H, Event, Payload, Bindings],
                ?MODULE
            )
    end.

-doc """
Invokes the optional `handle_info/2` callback.

Returns `ok` if the handler does not export `handle_info/2`, letting
the caller skip the subsequent diff + push cycle. Re-tags a
no-matching-clause crash at the callback's head as
`{unhandled_info, H, Info, Bindings}`; errors raised from inside
the callback body propagate untagged.
""".
-spec call_handle_info(Handler, Info, Bindings) ->
    {bindings(), resets(), effects()} | ok
when
    Handler :: module(),
    Info :: term(),
    Bindings :: bindings().
call_handle_info(H, Info, Bindings) ->
    case erlang:function_exported(H, handle_info, 2) of
        true ->
            try
                H:handle_info(Info, Bindings)
            catch
                error:function_clause:ST ->
                    arizona_error:raise_or_propagate(
                        function_clause,
                        ST,
                        H,
                        handle_info,
                        {unhandled_info, H, Info, Bindings},
                        [H, Info, Bindings],
                        ?MODULE
                    )
            end;
        false ->
            ok
    end.

-doc """
Invokes the optional `handle_update/2` callback.

Falls back to merging `Props` into `Bindings` if the callback is not
exported. Re-tags a no-matching-clause crash at the callback's
head as `{unhandled_update, H, Props, Bindings}`; errors raised
from inside the callback body propagate untagged.
""".
-spec call_handle_update(Handler, Props, Bindings) ->
    handle_update_ret()
when
    Handler :: module(),
    Props :: map(),
    Bindings :: bindings().
call_handle_update(H, Props, Bindings) ->
    case erlang:function_exported(H, handle_update, 2) of
        true ->
            try
                H:handle_update(Props, Bindings)
            catch
                error:function_clause:ST ->
                    arizona_error:raise_or_propagate(
                        function_clause,
                        ST,
                        H,
                        handle_update,
                        {unhandled_update, H, Props, Bindings},
                        [H, Props, Bindings],
                        ?MODULE
                    )
            end;
        false ->
            {maps:merge(Bindings, Props), #{}}
    end.

-doc """
Invokes the optional `handle_drain/2` callback.

Defaults to `{stop, Bindings, []}` when the callback is not exported
-- the framework's safe-default response to a transport drain is to
exit the live process cleanly so the WebSocket closes with code 1001
("going away") and the client reconnects to the new server version. Re-tags a
no-matching-clause crash at the callback's head as `{unhandled_drain,
H, Deadline, Bindings}`; errors raised from inside the callback body
propagate untagged.
""".
-spec call_handle_drain(Handler, Deadline, Bindings) ->
    handle_drain_ret()
when
    Handler :: module(),
    Deadline :: integer(),
    Bindings :: bindings().
call_handle_drain(H, Deadline, Bindings) ->
    case erlang:function_exported(H, handle_drain, 2) of
        true ->
            try
                H:handle_drain(Deadline, Bindings)
            catch
                error:function_clause:ST ->
                    arizona_error:raise_or_propagate(
                        function_clause,
                        ST,
                        H,
                        handle_drain,
                        {unhandled_drain, H, Deadline, Bindings},
                        [H, Deadline, Bindings],
                        ?MODULE
                    )
            end;
        false ->
            {stop, Bindings, []}
    end.

-doc """
Invokes the optional `unmount/1` callback on a handler module.

No-op if the callback is not exported. Re-tags a no-matching-clause
crash at the callback's head as `{unhandled_unmount, H, Bindings}`;
errors raised from inside the callback body propagate untagged.
""".
-spec call_unmount(Handler, Bindings) -> ok when
    Handler :: module(),
    Bindings :: bindings().
call_unmount(H, Bindings) ->
    case erlang:function_exported(H, unmount, 1) of
        true ->
            try
                H:unmount(Bindings)
            catch
                error:function_clause:ST ->
                    arizona_error:raise_or_propagate(
                        function_clause,
                        ST,
                        H,
                        unmount,
                        {unhandled_unmount, H, Bindings},
                        [H, Bindings],
                        ?MODULE
                    )
            end;
        false ->
            ok
    end.

-doc """
Formats dispatcher-tagged errors raised by the `call_*` wrappers above.

Picked up by `erl_error:format_exception/3` via the `error_info`
annotation, so the dev error page (and crash log) reads a sentence
that names the failing module/callback and (when known) the view id.
""".
-spec format_error(Reason, Stacktrace) -> ErrorInfo when
    Reason :: term(),
    Stacktrace :: [tuple()],
    ErrorInfo :: #{general := iolist()}.
format_error({missing_callback, Mod, Name, Arity}, [{_M, _F, Args, _Info} | _]) ->
    %% Bindings is always the LAST arg to every dispatcher (see call_*/N
    %% above). Pull it from the failing stack frame so the message can
    %% name the view that was being served.
    Bindings = lists:last(Args),
    #{
        general => io_lib:format(
            "while serving view ~0tp: module ~s does not export ~s/~b. "
            "Add the callback to make this code path work.",
            [view_id(Bindings), Mod, Name, Arity]
        )
    };
format_error({unhandled_event, Mod, Event, Bindings}, _ST) ->
    #{
        general => io_lib:format(
            "while handling event ~0tp on view ~0tp: no clause in "
            "~s:handle_event/3 matches that event name. Add a clause "
            "or fix the az-click attribute that dispatched it.",
            [Event, view_id(Bindings), Mod]
        )
    };
format_error({unhandled_info, Mod, Info, Bindings}, _ST) ->
    #{
        general => io_lib:format(
            "while delivering message ~0tp on view ~0tp: no clause "
            "in ~s:handle_info/2 matches that message.",
            [Info, view_id(Bindings), Mod]
        )
    };
format_error({unhandled_update, Mod, Props, Bindings}, _ST) ->
    #{
        general => io_lib:format(
            "while applying parent props ~0tp on view ~0tp: no clause "
            "in ~s:handle_update/2 matches.",
            [Props, view_id(Bindings), Mod]
        )
    };
format_error({unhandled_unmount, Mod, Bindings}, _ST) ->
    #{
        general => io_lib:format(
            "while unmounting view ~0tp: no clause in ~s:unmount/1 "
            "matches.",
            [view_id(Bindings), Mod]
        )
    };
format_error({unhandled_drain, Mod, Deadline, Bindings}, _ST) ->
    #{
        general => io_lib:format(
            "while handling drain (deadline=~0tp) on view ~0tp: no clause "
            "in ~s:handle_drain/2 matches.",
            [Deadline, view_id(Bindings), Mod]
        )
    };
format_error({render_no_clause, Mod, Bindings}, _ST) ->
    #{
        general => io_lib:format(
            "while rendering view ~0tp: no clause in ~s:render/1 "
            "matches the current bindings.",
            [view_id(Bindings), Mod]
        )
    }.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

view_id(Bindings) ->
    case Bindings of
        #{id := ViewId} -> ViewId;
        _ -> undefined
    end.
