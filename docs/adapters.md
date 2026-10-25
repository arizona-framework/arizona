# Server adapters

Arizona runs on any HTTP/WebSocket server through an **adapter**. Two are built in: roadrunner's
(`arizona_roadrunner_*`, the default) and Cowboy's (`arizona_cowboy_*`, selected with
`adapter => arizona_cowboy_server`), and both pass the conformance suite. Cowboy's is also the
worked example of an adapter for a server whose router cannot express the routing contract: it
mounts one catch-all route and matches in `arizona_cowboy_router`. This page is the contract an adapter meets and the core functions it builds on.

Everything security-relevant or user-visible is decided in core, once: what a route means, the
CSRF check, sessions and cookies, what a page, an upgrade, or a controller answers. An adapter
**routes natively** and **maps** core's answers onto its server. What core cannot enforce, the
black-box `arizona_server_conformance_SUITE` checks (see [Conformance](#conformance)).

## Configuration

The application starts the `server` env's listener through the adapter its `adapter` key names
(default `arizona_roadrunner_server`). Every other key but `drain_timeout` is the adapter's own:

```erlang
{arizona, [
    {server, #{
        adapter => my_cowboy_adapter,
        drain_timeout => 5000,
        routes => [...]
        %% ...my_cowboy_adapter's own options
    }}
]}
```

## The pieces

| An adapter provides | Built on | Contract |
| ------------------- | -------- | -------- |
| Listener lifecycle | `arizona_server` behaviour, `arizona_server_registry` | start, stop, drain, recompile routes |
| Request access | `arizona_req` behaviour | parse callbacks, `read_body/1`, `scheme/1`, `resolve_route/3` |
| Its router | `arizona_route` | the [routing contract](#routing) |
| Live pages | `arizona_http:reply/3` | reply as returned, plus framing |
| WebSocket | `arizona_ws:upgrade/3`, `arizona_ws:init/1`, `arizona_socket` | the [WebSocket](#websocket) mapping |
| Controllers | `arizona_controller:dispatch/2` | the [reply kinds](#controllers) |
| Assets | `arizona_route:asset_dir/1` | the server's own static-file handler |
| Response transforms | `arizona_route:transforms/2` | etag, compress, cors, security headers |

### Listener lifecycle -- `arizona_server`

`start/2`, `stop/1`, `drain/2`, `recompile_routes/0`. A listener that is up registers its adapter
with `arizona_server_registry:register/1`, so the dev reloader recompiles its routes after a code
reload (route terms can hold funs from reloaded modules). A listener another framework owns
registers the same way.

**Drain** closes the listen socket and sends each live WebSocket session's live process
`{arizona_drain, Deadline}` through `arizona_socket:drain/2`, with `Deadline` an
`erlang:monotonic_time(millisecond)` value `Timeout` ahead; the view runs `handle_drain/2` and by
default closes `1001`, which clients reconnect from. Sessions still open at the deadline are
killed. A server with no drain of its own (Cowboy) suspends accepting, closes idle keep-alive
connections, and notifies the WebSocket processes it tracks.

### Request access -- `arizona_req`

`parse_bindings/1` (binary keys, percent-decoded values), `parse_params/1`, `parse_cookies/1`,
`parse_headers/1` (lowercase names; a repeated field combined in order, `Cookie` lines joined by a semicolon),
`read_body/1` (`{ok, Body, Raw1}`, or `{error, too_large | trailers_too_large | timeout |
invalid}`), and `scheme/1` (the connection's own, `https` only on TLS). `resolve_route/3` resolves a
navigate target: its full contract is on the callback in `arizona_req`. In short, never crash or
reply on an unknown path (it is client-supplied), match as a `GET` (an HTTP/2 WebSocket upgrade
arrives as `CONNECT`), and match exactly as an HTTP request to that path would.

### Routing

`arizona_route:kind/1` names a route (`live`, `ws`, `asset`, `controller`, `mcp`, `reload`) and
refuses a term that is no route. `mcp` and `reload` are capabilities: an adapter without one
raises `arizona_route:unsupported/2` while compiling its routes, so the listener refuses to
start. For the rest, compile from `arizona_route` rather than restating its rules:

- `path/1`, `methods/1` -- the pattern and the allowlist (`GET` implies `HEAD`; `any`).
- `steps/1` -- the pipeline, the CSRF `check_origin` step first unless opted out.
- `transforms/2` -- the response transforms, validated, compression outermost and on by default
  for pages and assets.
- `live/2`, `controller/1`, `asset_dir/1` -- what each kind is served with.

The router itself must behave as follows; most servers' routers do not, so expect to match
routes yourself:

- Routes are tried in declaration order; the first match wins. A route the earlier ones fully
  shadow is a configuration error, refused at start.
- `:name` captures one segment, `*name` the rest (zero or more segments, as a list). Captured
  values are percent-decoded; literals match the raw bytes, case-sensitively. Empty segments and
  a trailing slash are ignored.
- Several routes may share a path with different methods. A path that matches but whose method
  no matching route allows gets `405` with an `Allow` header listing the methods of **every**
  route matching that URL, across different patterns.
- A route with a `cors` transform answers `OPTIONS` even when it declares no such method: the
  preflight reaches the transform (`204` plus the `Access-Control-*` headers), and a plain
  `OPTIONS` gets `204` with the `Allow` set a `405` there would carry.

### Live pages

`arizona_http:reply(Handler, ArzReq, RouteOpts)` runs the pipeline and the render (or the halt)
and returns `{Status, Headers, Body, ArzReq1}`. Send the headers as listed, add your framing (a
`content-length`), and hand your server back `arizona_req:raw(ArzReq1)`: reading the body
advances the raw request.

### WebSocket

On the upgrade request, `arizona_ws:upgrade(QS, Adapter, Raw)` (with `QS` the parsed query
string) answers `{reply, Status, Headers, Raw1}` -- send it as a plain HTTP response -- or
`{upgrade, Headers, State}`: upgrade, with `Headers` on the `101`. In the process that owns the
upgraded connection, `arizona_ws:init(State)` starts the socket. Feed text frames to
`arizona_socket:handle_in/2` and every other message to `arizona_socket:handle_info/2`. Each
returns `{ok, Socket}`, `{reply, Data, Socket}`, `{reply_many, Frames, Socket}` (send the text
frames in order), or `{close, Code, Reason, Socket}`.

### Controllers

`arizona_controller:dispatch(arizona_route:controller(Route), ArzReq)` runs the pipeline and the
action and returns `{Reply, ArzReq1}`, the reply's headers already carrying the flushed
cookies. A reply is one of four kinds:

| Kind | Shape |
| ---- | ----- |
| buffered | `{Status, Headers, Body}` -- every adapter |
| stream | `{stream, Status, Headers, Fun}` -- call `Fun(Send)`, `Send(Data, nofin \| fin)` |
| sendfile | `{sendfile, Status, Headers, {Path, Offset, Length}}` |
| loop | `{loop, Status, Headers, State}` |

The last three are capabilities: one your server cannot send raises
`arizona_controller:unsupported_reply/2` rather than degrading. For a loop, route every message
the connection receives to `Handler:handle_info(Msg, Push, State)` (the route's handler), where
`Push(Data)` writes one chunk; `{ok, State}` continues and `{stop, State}` ends the response.
Translate your server's client-disconnect signal into one final `{arizona_disconnect, Reason}`.

### Assets

Serve `arizona_route:asset_dir/1` with your server's static handler, from the `*path` capture:
`GET` and `HEAD` only (`405` otherwise), `404` for a missing file, nothing outside the directory
under any spelling of `..`, and the route's `cache_control` option, when set, on a cacheable
`200` only.

### Response transforms

`arizona_route:transforms/2` lists them in wrapping order, first outermost; implement each with
whatever your server offers:

- `etag` -- a weak validator of the body; a matching `If-None-Match` gets `304`. It hashes the
  uncompressed body, which is why compression wraps it.
- `compress` -- gzip or deflate by `Accept-Encoding`, for a body above a size threshold
  (roadrunner's is 860 bytes).
- `{cors, Config}` -- preflight and response headers for the configured origins.
- `security_headers` / `{security_headers, Config}` -- by default `x-content-type-options:
  nosniff`, `x-frame-options: SAMEORIGIN`, and `referrer-policy:
  strict-origin-when-cross-origin`; `Config` overrides or drops each one (`false`) and opts into
  `content-security-policy` and, over HTTPS only, `strict-transport-security` (`hsts`).

## The flush rule

Every response built from an Arizona request goes through `arizona_req:flush/1` exactly once (or
`arizona_req:halt_reply/1`, which includes it) -- pages, controller replies of every kind, and
the WebSocket `101` -- and that is also the session store's commit point. The `reply`, `upgrade`,
and `dispatch` functions above do it for you. Header lines come back **in send order**: emit them
as listed, since a browser keeps the last `Set-Cookie` for a name.

## Conformance

`arizona_server_conformance_SUITE` boots listeners through a driver
(`arizona_conformance_driver`: `start/3`, `stop/1`, `drain/2`, and the optional
`supports_reply/1` and `supports_route/1`) and asserts only what crosses the wire. Write a driver
for your adapter and run the suite with it:

```sh
ARIZONA_CONFORMANCE_DRIVER=my_cowboy_conformance \
    rebar3 ct --suite=arizona_server_conformance_SUITE
```

A capability your driver reports unsupported is skipped for replies and must be refused for
routes. Run it in your adapter's CI against Arizona's `main`, so a change in core that breaks
your adapter shows up there.

Not covered by the suite, so not proven for any adapter: HTTP/2 WebSockets (its client speaks
HTTP/1.1) and request-size limits, which each server configures its own way.
