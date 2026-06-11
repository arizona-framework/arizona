# MCP enablement

What roadrunner needs to do so Arizona (the layer above it) can implement
an MCP server cleanly, plus where the boundary between the two sits. MCP
here means a Model Context Protocol server reachable by AI agents (Claude
Code, Cursor, Claude Desktop) over its **Streamable HTTP** transport:
JSON-RPC 2.0 messages, client-to-server over HTTP POST, server-to-client
over Server-Sent Events.

This doc lives in Arizona for now because Arizona is where the MCP feature
would be built. The roadrunner-side items it once called out (W1/W2) turned
out to be **already present** in roadrunner: the client-disconnect signal
(W1) ships on h1/h2/h3, and the per-event flush (W2) is verified. So no
roadrunner work gates MCP -- the feature is entirely Arizona
application-layer code. The verification details are kept below (W1/W2)
because the MCP rationale for each is still the clearest record of *why*
those primitives matter.

## The boundary -- MCP is an application concern

This is already settled in roadrunner's `docs/roadmap.md` ("Deferred to
the application layer"): *agent/tool surfaces (MCP and similar) are an
application concern; the layer above mounts them on ordinary roadrunner
routes, no extra listener.* Roadrunner supplies the mechanism (routes,
immutable request/response values, streaming response shapes, the SSE
builder, telemetry); Arizona supplies the JSON-RPC envelope, the tool
registry, the session lifecycle, and the policy.

So nothing in this document adds an MCP-aware feature to roadrunner. It
lists the transport primitives the app layer leans on, marks which
already ship, and isolates the one refinement that is actually
load-bearing for MCP.

## What already ships -- the transport is ~ready

MCP's Streamable HTTP maps onto existing roadrunner API with no new
transport feature:

| MCP transport need | roadrunner today |
| --- | --- |
| POST a JSON-RPC body, read it whole | auto body mode -> `roadrunner_req:body/1` |
| Per-request choice: answer `application/json` **or** open `text/event-stream` | handler picks the response *shape* at runtime: `{Status, Headers, Body}` for JSON, or `{loop, ...}` / `{stream, ...}` for SSE |
| Long-lived server-to-client SSE channel (GET) | `{loop, Status, Headers, State}` + `handle_info/3` + `Push`, no `after` clause -- blocks indefinitely |
| Finite "POST -> stream notifications -> result -> close" | `{loop, ...}` returns `{stop, _}`, or `{stream, ...}` with `Send(Data, fin)` |
| SSE framing (`event:` / `data:` / `id:` / `retry:` / keep-alive comment) | `roadrunner_sse:event/1,2,3`, `comment/1`, `retry/1` |
| Read `Mcp-Session-Id` / `Last-Event-ID` / `Origin` / `Accept`; set arbitrary response headers | `roadrunner_req:header/2` (case-insensitive), lowercase response-header list |
| No timeout killing an open stream | `request_timeout` covers only the initial header read; once headers are sent the loop blocks with no `after` |
| DELETE to end a session | an ordinary handler |

**Version parity is already done.** The `{loop, ...}` (SSE) shape is
wired on all three HTTP versions: h1 via `roadrunner_loop_response`, h2
via `roadrunner_http2_loop_response` (dispatched from
`roadrunner_http2_stream_worker`), and h3 via
`roadrunner_http3_stream_worker:send_loop/info_loop`. The h3 conn-loop
moduledoc confirms it: *"All response shapes are supported (buffered,
`stream`, `loop`, sendfile)."* So an MCP SSE channel works over h1, h2,
and h3 unchanged.

**h2/h3 are a free multiplier, not a prerequisite.** Because h2 and h3
multiplex streams as independent monitored workers, many concurrent MCP
SSE sessions ride one connection with no head-of-line blocking and no
connection exhaustion. On h1 a single SSE GET stream holds the whole
connection, so a client needs a second connection to POST. MCP works on
h1; h2/h3 just make session fan-out nicer. No action needed -- it falls
out of the existing transport abstraction.

## Work to do

### W1 -- Client-disconnect signal for `{loop, ...}` / `{stream, ...}` -- shipped

**Status: already implemented in roadrunner.** This was filed as the one
load-bearing blocker; on inspection roadrunner already delivers it. A
`{loop, ...}` / streaming handler receives a final
`{roadrunner_disconnect, Reason}` through `handle_info/3` the moment the
peer goes away, on all three versions:

- h1 -- `roadrunner_loop_response.erl` arms the socket `{active, once}`
  and, on the transport close/error tag, calls
  `deliver_disconnect(Handler, Push, State, closed)` before ending the
  loop -- so a quiet SSE channel learns of the drop immediately, not on
  its next write.
- h2 -- `roadrunner_http2_loop_response.erl` delivers `reset` on a peer
  `RST_STREAM` and `conn_down` when the owning connection dies.
- h3 -- `roadrunner_http3_stream_worker.erl` delivers `reset` on a peer
  `RESET_STREAM` and `conn_down` on connection death.

`Reason` is one of `closed` | `reset` | `conn_down`, documented on the
`handle_info/3` callback in `roadrunner_handler`.

**Why it matters for MCP (the original rationale):** an MCP *session* is
designed to outlive any single SSE connection -- the `Mcp-Session-Id`
persists across reconnects, and the client may resume a dropped stream via
`Last-Event-ID`. When the channel drops, the session needs to know
*promptly* so it can stop pushing and start buffering for a possible
resume, drop the pubsub / live-view subscriptions it opened for that
channel, and avoid discovering the dead socket only on its next write
(minutes away on a quiet channel). The in-band `{roadrunner_disconnect, _}`
message is exactly that discoverable, correct-by-construction primitive --
no out-of-band monitor between the session and the loop handler needed.

### W2 -- Per-event flush latency on the loop path, esp. over QUIC -- verified

**What:** Confirm that each `Push` (or each `Send(Data, nofin)`) on the
`{loop, ...}` / `{stream, ...}` path flushes to the wire promptly on every
version, with no coalescing that batches multiple SSE events into one
delayed write. On h3 specifically, confirm `quic:send_data/4` emits a
STREAM frame per event rather than buffering until a larger boundary.

**Why:** SSE is only useful if a server-initiated notification reaches the
agent at emit time. An MCP `notifications/*` message delayed until the
next write is a real-time-ness bug, not just a perf nit. This is a
verification + (if needed) a flush-control fix, not a new feature.

**Scope:** small -- a focused test that times inter-event delivery on each
version, plus a flush tweak only if a gap shows up.

**Status: verified.** Each `Push` / `Send(_, nofin)` calls the transport
send exactly once per emission, with no coalescing: h1 frames one chunk
per push (`roadrunner_loop_response:make_push/1`), h2 sends one DATA frame
per push (`roadrunner_http2_loop_response:make_push/2`), and h3 emits one
QUIC STREAM frame per push (`roadrunner_http3_stream_worker:loop_push/3`
via `roadrunner_quic:send_data/4`). No coalescing path exists to tweak.

### W3 -- `{loop, ...}` mailbox / backpressure characterization -- optional

**What:** Characterize what happens when the application pushes
server-to-client notifications faster than the client drains them on the
`{loop, ...}` path. h2 has implicit backpressure (the stream worker stalls
on the flow window); the h1 loop path surfaces flow only as write errors,
and an unbounded loop mailbox is the failure mode.

**Why deferred:** a dev-tool MCP server pushes trivial volume, so this is
not on the critical path. It matters only if a deployment streams
high-volume notifications. Listed so the failure mode is known, not
discovered.

**Scope:** medium if it ever needs a real backpressure signal; otherwise a
documented caveat.

## Explicit non-items

Things that look adjacent but are **not** roadrunner work for MCP:

- **NDJSON streaming response builder** (on roadrunner's roadmap,
  "small"). It is the JSON sibling of `roadrunner_sse`, but MCP's
  Streamable HTTP uses `text/event-stream`, not NDJSON. `roadrunner_sse`
  already covers MCP; NDJSON is for other streaming endpoints. Do not
  sequence MCP behind it.
- **gRPC** (on roadrunner's roadmap, "large"). Different protocol
  (protobuf over h2), no MCP binding, no MCP client speaks it. Unrelated;
  do not sequence MCP behind it.
- **WebSocket over h2/h3** (RFC 8441 / 9220, on roadrunner's roadmap).
  MCP's standard transport is Streamable HTTP, not WebSocket. No client
  will connect to a WS MCP endpoint. Not needed.
- **CORS / security-headers / per-peer rate guard middleware** (on
  roadrunner's roadmap). Useful to an MCP deployment, but generic edge
  middleware, not MCP-specific. They apply to an MCP route the same as any
  other; no MCP work.

## Explicitly the application layer's job (Arizona, not roadrunner)

For completeness, so the boundary is unambiguous. None of these belong in
roadrunner (roadrunner's `docs/roadmap.md`, "Deferred to the application
layer"):

- **JSON-RPC 2.0 envelope** -- parse/encode requests, responses,
  notifications, errors; id correlation.
- **MCP lifecycle** -- `initialize` / `initialized`, capability
  negotiation, protocol-version string, `ping`.
- **Tool registry + JSON Schemas** -- `tools/list`, `tools/call`
  dispatch, and the per-tool input schemas.
- **Session lifecycle** -- minting `Mcp-Session-Id`, mapping a session to
  a process that outlives individual SSE connections, DELETE teardown.
- **Resumability buffer** -- assigning SSE `id`s
  (`roadrunner_sse:event/3`) and replaying buffered events when a client
  reconnects with `Last-Event-ID` (read via `roadrunner_req:header/2`).
  Roadrunner supplies the `id` field and the header; the buffer is the
  app's.
- **Origin validation / auth** -- DNS-rebinding `Origin` checks and any
  token/OAuth policy. Roadrunner exposes the headers; Arizona decides.

## How Arizona wires onto roadrunner (concrete flow)

So the value of W1 is concrete, here is the mapping an Arizona MCP handler
would use:

- **POST `/mcp`** (client-to-server JSON-RPC):
  read the body with `roadrunner_req:body/1`, decode JSON-RPC, dispatch.
  If the response is immediate, return `{200, Headers, Json}`. If the
  server wants to stream progress notifications before the result, return
  `{loop, 200, [{~"content-type", ~"text/event-stream"}, ...], State}` and
  emit `roadrunner_sse:event/2,3` from `handle_info/3`, finishing with
  `{stop, _}`.
- **GET `/mcp`** (server-to-client channel):
  return `{loop, 200, [{~"content-type", ~"text/event-stream"}, ...],
  State}`, register the session's push side, and forward each
  notification as `roadrunner_sse:event/3` (with an `id` for resume).
  **This is the stream that uses the shipped disconnect signal (W1)** --
  when the agent's channel drops, the session learns of it via
  `{roadrunner_disconnect, _}` to buffer-for-resume and drop subscriptions.
- **DELETE `/mcp`** (end session):
  an ordinary handler that tears down the session process.

Everything above leans only on existing roadrunner API, the disconnect
notification (W1) included -- all of it ships today.

## Summary

| Item | Status | Effort | Gates MCP? |
| --- | --- | --- | --- |
| SSE over h1/h2/h3 (`{loop, ...}`) | ships | -- | already done |
| `roadrunner_sse` framing | ships | -- | already done |
| Body read, header read/write, no-timeout streams | ships | -- | already done |
| W1: client-disconnect signal | ships | -- | already done |
| W2: per-event flush latency (esp. h3) | verified | -- | already done |
| W3: loop backpressure | optional | medium | no (volume-dependent) |
| NDJSON / gRPC / WS-over-h2h3 | unrelated | -- | no |

Net: the transport is **fully built** for MCP -- W1 (disconnect) ships and
W2 (per-event flush) is verified, so no roadrunner-side work gates MCP. The
feature is entirely Arizona application-layer code: the JSON-RPC envelope,
MCP lifecycle, tool registry, session lifecycle, and policy.
