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

The rest of this guide builds the dev tool. The full worked example is
[`arizona_demo_mcp`][demo], mounted at `/mcp` in the dev server.

[demo]: https://github.com/arizona-framework/arizona/blob/main/test/support/arizona_demo_mcp.erl

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

Add an `mcp` route. For a dev tool, run it in **session mode** (so per-session state, like a REPL's
bindings, persists across calls) and gate it with the `Origin` allowlist:

```erlang
{mcp, ~"/mcp", my_mcp, #{
    sessions => true,
    origins => [~"http://localhost:4040"]
}}
```

The `Origin` check is a DNS-rebinding defense: a request with no `Origin` (a CLI agent) is allowed;
a browser `Origin` must be in the allowlist. That alone is the reason to keep the gate on even on
localhost -- it stops a malicious web page from driving your dev MCP server.

### Connecting an agent

```bash
claude mcp add --transport http my-app-dev http://localhost:4040/mcp
# or, to explore by hand:
npx @modelcontextprotocol/inspector
```

## Introspection tools

The demo exposes a handful, each a thin wrapper over data the running node already has:

- `list_routes` -- `arizona_test_server:routes()` (point this at wherever your app declares routes).
- `get_docs` -- a module's or function's documentation via `code:get_doc/1` (EEP-48).
- `app_info` -- `application:get_key/2`, `erlang:system_info/1`.
- `reloader_status` -- the dev reloader's current compile error via `arizona_reloader:get_error/0`.

## `eval`: powerful, and a footgun

The demo's `eval` tool runs Erlang in the live node and keeps its bindings across calls (a REPL the
agent drives), by scanning/parsing/evaluating against the session's `erl_eval` bindings:

```erlang
handle_tool(~"eval", #{~"code" := Code}, _Ctx, #{bindings := Bindings} = State) ->
    case eval_code(Code, Bindings) of
        {ok, Value, NewBindings} -> {reply, format_value(Value), State#{bindings := NewBindings}};
        {error, Message} -> {error, Message, State}
    end.
```

This is what makes a dev tool genuinely useful -- the agent can inspect any state and call any
function. It is also **arbitrary remote code execution**. Only ever mount `eval` on a dev-only,
localhost route behind the `Origin` allowlist, and never on anything reachable by an untrusted
client. Running it in session mode means a slow eval runs in the session's worker, so it is
cancellable and bounded by `request_timeout_ms` rather than wedging the session.

## Route options

All optional, with safe defaults; a route that sets none behaves as a plain stateless MCP server.

| Option | Default | Meaning |
| ------ | ------- | ------- |
| `sessions` | `false` | Opt into stateful sessions (`initialize` mints an `Mcp-Session-Id`). |
| `origins` | `[]` | `Origin` allowlist (no-`Origin` requests are always allowed). |
| `auth` | none | A per-route auth hook run after the `Origin` check. |
| `max_sessions` | unbounded | Cap on live sessions for this route; `initialize` past it gets a `503`. |
| `session_ttl_ms` | `300000` | Idle timeout before an abandoned session is reaped. |
| `request_timeout_ms` | `60000` | How long a buffered request waits before the client gets a timeout. |
| `session_max_pending` | `100` | Cap on queued buffered requests per session. |
| `session_keepalive_ms` | `30000` | SSE keep-alive comment interval (`infinity` disables). |
| `page_size` | `50` | Page size for `*/list` pagination. |

See [architecture.md](architecture.md) for how the transport, sessions, and dispatch work under the
hood.
