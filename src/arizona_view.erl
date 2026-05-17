-module(arizona_view).
-moduledoc """
Behaviour for route-mounted Arizona handlers (pages, not embedded
components).

Adds a single mount callback over the shared `arizona_handler`
contract: `mount/2` takes both the initial `Bindings` and an
`arizona_req:request()`, giving a view access to URL path bindings,
query params, cookies, headers, and body via the request accessors.

The other lifecycle callbacks (`render/1`, `handle_event/3`,
`handle_info/2`, `unmount/1`) come from `arizona_handler`. Views
don't implement `handle_update/2` in practice -- they're
route-level and never receive parent prop updates -- but the shared
behaviour allows it for forward compatibility.

Handlers include `arizona_view.hrl`, which declares both behaviours
(`arizona_handler` and `arizona_view`), enables the parse transform,
and brings in the common macros.

## Required callbacks

- `mount/2` -- runs once on instance creation with
  `(Bindings, Request)`; returns initial bindings plus any reset
  values
- `render/1` -- from `arizona_handler`

## Optional callbacks

- `handle_event/3`, `handle_info/2`, `handle_drain/2`, `unmount/1`
  -- from `arizona_handler`

## Graceful drain

When the transport adapter receives a drain signal (e.g.
`roadrunner_listener:notify_drain/2` during a rolling deploy, or
`roadrunner_listener:drain/2` for a full listener shutdown), each
live view's optional `handle_drain/2` runs. If a view doesn't export it,
the framework defaults to exiting the live process cleanly so the
WebSocket closes with code 1000 and the client reconnects to the
new server version.

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
| **Can prevent exit?** | Yes (`{B, R, E}` keeps the view alive) | No, process is dying |
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
4. Live process exits normal; `terminate/2` calls `unmount/1` for
   cleanup (unsub pubsub, release ETS, etc.).
5. WS session observes the exit and sends close code 1000.
6. Client reconnects to the new server version.

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
view's render template**, the new server's diff baseline won't match
the client's preserved snapshot, and the first diff may misalign
(visual glitches until the next full re-render).

This isn't drain-specific — the same risk exists for any reconnect
across a deploy — but drain makes it more likely because clients
reconnect deterministically on every deploy. Mitigations if the
glitch is unacceptable for a given view:

- In `handle_drain/2`, push `arizona_js:reload()` instead of
  `dispatch_event/2` to force a full page reload on the client
  (loses any client-only state but guarantees template alignment).
- Or use `arizona_js:navigate/1` to redirect the client to a known
  good route on drain.

Default behavior (no effect or a custom `dispatch_event` effect)
takes the reconnect-with-snapshot path; choose explicitly when the
view's template churns often.
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([call_mount/3]).

%% --------------------------------------------------------------------
%% Behaviour callbacks
%% --------------------------------------------------------------------

-doc """
Initializes a view instance. Called once when the framework mounts
the handler at the top of a route.

`Bindings` carries the route's static `bindings` config (plus
anything middleware layered on top). `Request` is the current
`arizona_req:request()` -- use its accessors to reach URL path
bindings, query params, cookies, headers, and body.
""".
-callback mount(Bindings, Request) -> arizona_stateful:mount_ret() when
    Bindings :: arizona_stateful:bindings(),
    Request :: az:request().

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc """
Invokes the required `mount/2` callback on a view handler module.
""".
-spec call_mount(Handler, Bindings, Request) -> arizona_stateful:mount_ret() when
    Handler :: module(),
    Bindings :: arizona_stateful:bindings(),
    Request :: az:request().
call_mount(H, Bindings, Req) ->
    H:mount(Bindings, Req).
