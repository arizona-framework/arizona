# MCP servers

Arizona can serve the [Model Context Protocol](https://modelcontextprotocol.io) (MCP), so an AI
agent (Claude Code, Cursor, the MCP Inspector, ...) can call into your app. You write a handler
module that exposes **tools**, **resources**, and **prompts**, and mount it on a route. The
transport is MCP's Streamable HTTP (JSON-RPC over HTTP POST, with an optional SSE channel for
server-initiated messages); it rides the same roadrunner listener as the rest of your app.

Two shapes fall out of the same engine:

- A **dev tool**, in the spirit of [Tidewave](https://github.com/tidewave-ai/tidewave_phoenix):
  a coding agent introspects your *running* dev app (its routes, its docs) and runs code against
  it. Localhost, single developer, trusted.
- A **user-facing server**: your app exposes its features as tools to end-users' agents at
  runtime. Untrusted and concurrent, so the session, auth, and resource-limit features earn their
  keep.

Arizona ships a batteries-included dev tool, [`arizona_dev_mcp`][dev]. Add it to your **dev**
server's routes and point a coding agent at it:

```erlang
arizona_dev_mcp:route(~"/mcp")
```

`route/1` defaults to `sessions => true` (so `eval` is a persistent REPL); `route/2` merges extra
opts. You can mount it as-is (jump to [Introspection tools](#introspection-tools)) or write your own
-- this guide covers both.

[dev]: https://github.com/arizona-framework/arizona/blob/main/src/arizona_dev_mcp.erl

## The behaviour

An MCP server is a module with `-behaviour(arizona_mcp)`. Only three callbacks are required --
`init/1`, `tools/1`, and `handle_tool/4` -- the rest (resources, prompts, completion, logging) are
optional and gated by the capabilities `init/1` advertises.

```erlang
-module(my_mcp).
-behaviour(arizona_mcp).
-export([init/1, tools/1, handle_tool/4]).

init(_InitParams) ->
    %% {ok, ServerInfo, Capabilities, State}
    {ok, #{name => ~"my_app_dev", version => ~"0.1.0"}, #{tools => #{}}, #{}}.

tools(_State) ->
    [
        #{
            name => ~"list_routes",
            description => ~"List the routes this app serves",
            input_schema => #{type => ~"object", properties => #{}}
        }
    ].

handle_tool(~"list_routes", _Args, _Ctx, State) ->
    Lines = [io_lib:format("~p ~ts", [Type, Path]) || {Type, Path, _, _} <- my_app:routes()],
    {reply, iolist_to_binary(lists:join($\n, Lines)), State}.
```

`handle_tool/4` returns `{reply, Result, State}` (a binary, or a map with `content` /
`structured_content`) or `{error, Message, State}` for an in-band tool error. The returned `State`
threads back into the session, so state mutated by one call is visible to the next.

## Mounting it

Add an `mcp` route. For a dev tool, run it in **session mode** so per-session state (like a REPL's
bindings) persists across calls:

```erlang
{mcp, ~"/mcp", my_mcp, #{sessions => true}}
```

The `Origin` check is a DNS-rebinding defense: a request with no `Origin` (a CLI agent, or the
Inspector's proxy) is allowed; a browser `Origin` must be in the `origins` allowlist, which defaults
to empty. So a CLI agent needs no `origins` at all -- omitting it blocks every browser origin, the
safest posture. Add `origins => [~"http://localhost:PORT"]` only to allow a direct browser-based
client.

### Connecting an agent

```bash
claude mcp add --transport http my-app-dev http://localhost:4040/mcp
# or, to explore by hand:
npx @modelcontextprotocol/inspector
```

## Introspection tools

The built-in `arizona_dev_mcp` ships these, each a thin wrapper over data the running node already
has. Mount it with `arizona_dev_mcp:route(~"/mcp")` and point your agent at `/mcp`:

- `list_routes` -- the app's routes, read from the `arizona` `server` app env.
- `describe_component` -- a component's kind (stateful/stateless), its exports, and its moduledoc.
- `get_docs` -- a module's or function's documentation via `code:get_doc/1` (EEP-48).
- `get_source_location` -- where a module (or function) is defined.
- `reloader_status` -- the dev reloader's current compile error via `arizona_reloader:get_error/0`.
- `app_info` -- `application:get_key/2`, `erlang:system_info/1`.
- `render_component` -- render a view/component module to HTML with given bindings.
- `eval` -- run Erlang in the live node (see below).

## `eval`: powerful, and a footgun

`arizona_dev_mcp`'s `eval` tool runs Erlang in the live node and keeps its bindings across calls (a
REPL the agent drives), by scanning/parsing/evaluating against the session's `erl_eval` bindings:

```erlang
handle_tool(~"eval", #{~"code" := Code}, _Ctx, #{bindings := Bindings} = State) ->
    case eval_code(Code, Bindings) of
        {ok, Value, NewBindings} -> {reply, format_value(Value), State#{bindings := NewBindings}};
        {error, Message} -> {error, Message, State}
    end.
```

This is what makes a dev tool genuinely useful -- the agent can inspect any state and call any
function. It is also **arbitrary remote code execution**, so `eval` is always available but the dev
route is **localhost-only by default**: `arizona_dev_mcp:route/1,2` set `allow_remote_access => false`,
and the MCP handler refuses any request whose peer is not a loopback address -- regardless of which
interface the listener bound. That peer check (mirroring Tidewave, which is localhost-only by default
rather than gated by a per-tool switch) is the primary guard, alongside the `Origin` check and
keeping this a dev-only dependency; set `allow_remote_access => true` only on a network you trust.

**The peer check is void behind a proxy or tunnel.** It trusts the immediate TCP peer, so a same-host
reverse proxy or a dev tunnel (ngrok/cloudflared/port-forward) makes every remote client look like
loopback -- and binding the listener to loopback does *not* help (the tunnel connects to loopback
anyway). Whenever the port is reachable through anything but a direct local socket, gate `eval` with
an `auth` hook (a shared secret) instead; `proto_opts => #{ip => {127,0,0,1}}` only hardens a *direct*
remote connection, and `proxy_protocol => true` lets a client spoof the peer. Running it in session
mode means a slow eval runs in the session's worker, so it is cancellable and bounded by
`request_timeout_ms` rather than wedging the session.

## Route options

All optional, with safe defaults; a route that sets none behaves as a plain stateless MCP server.

| Option | Default | Meaning |
| ------ | ------- | ------- |
| `sessions` | `false` | Opt into stateful sessions (`initialize` mints an `Mcp-Session-Id`). |
| `origins` | `[]` | `Origin` allowlist (no-`Origin` requests are always allowed). |
| `auth` | none | A per-route auth hook run after the `Origin` check. |
| `allow_remote_access` | `false` | Localhost-only unless `true`: refuse a non-loopback peer (void behind a same-host proxy/tunnel -- use `auth`). |
| `max_sessions` | unbounded | Cap on live sessions for this route; `initialize` past it gets a `503`. |
| `session_ttl_ms` | `300000` | Idle timeout before an abandoned session is reaped. |
| `request_timeout_ms` | `60000` | How long a buffered request waits before the client gets a timeout. |
| `session_max_pending` | `100` | Cap on queued buffered requests per session. |
| `session_keepalive_ms` | `30000` | SSE keep-alive comment interval (`infinity` disables). |
| `page_size` | `50` | Page size for `*/list` pagination. |

See [architecture.md](architecture.md) for how the transport, sessions, and dispatch work under the
hood.
