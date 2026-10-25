-module(arizona_server).
-moduledoc """
Behaviour for an HTTP/WebSocket server adapter: what boots, drains, stops, and
refreshes the routes of the listeners that serve Arizona.

`arizona_roadrunner_server` is the built-in implementation. The application
starts the `server` env's listener through the adapter named by its `adapter`
key (default `arizona_roadrunner_server`); every other key of that map is the
adapter's own options:

```erlang
{arizona, [
    {server, #{
        adapter => arizona_roadrunner_server,
        drain_timeout => 5000,
        transport_opts => [{port, 4040}],
        routes => [...]
    }}
]}
```

`drain_timeout` is read by the application, not handed to the adapter.

## Contract

An adapter is a callback module. It routes natively and must reproduce the
wire behaviour `arizona_server_conformance_SUITE` pins down; the request side
goes through an `arizona_req` adapter, the WebSocket through `arizona_ws` and
`arizona_socket`.

Each listener is reported to `arizona_server_registry` once it is up
(`listener_up/3`) and when it stops (`listener_down/1`), so a hot reload reaches
it (see `recompile_routes/0`) and the node's dev-mode signals follow its `reload`
and dev MCP routes. An adapter whose listeners are owned by another framework
reports them the same way.
""".

%% --------------------------------------------------------------------
%% Callbacks
%% --------------------------------------------------------------------

-doc """
Starts a listener named `Name` from the adapter's options (the `server` env map
minus `adapter` and `drain_timeout`). Starts whatever applications the server
needs: Arizona lists none of them, so a server-less consumer never boots one.
""".
-callback start(Name :: atom(), Opts :: map()) -> {ok, pid()} | {error, Reason :: term()}.

-doc """
Stops the listener named `Name` at once, forgetting its routes. Returns
`{error, not_found}` when no such listener runs.
""".
-callback stop(Name :: atom()) -> ok | {error, not_found}.

-doc """
Drains the listener named `Name`, then stops it like `stop/1`, waiting up to
`Timeout` milliseconds.

The listen socket closes and every live WebSocket session is told to wind down
by sending its live process `{arizona_drain, Deadline}`, where `Deadline` is an
`erlang:monotonic_time(millisecond)` value `Timeout` ahead. The live process
runs the view's `handle_drain/2` and, by default, closes the socket `1001`,
which clients reconnect from. Sessions still open at the deadline are killed.
Returns `{error, not_found}` when no such listener runs.
""".
-callback drain(Name :: atom(), Timeout :: non_neg_integer()) -> ok | {error, not_found}.

-doc """
Tells every live WebSocket session of the listener named `Name` to wind down,
as `drain/2` does, without stopping the listener: it keeps accepting, so a
client that reconnects is served by it. This is the rolling-deploy drain: the
sessions move over while the node keeps answering, and the operator stops the
listener (or the node) later.

`Deadline` in `{arizona_drain, Deadline}` is `Timeout` milliseconds ahead, as
in `drain/2`; nothing enforces it, so a view that stays alive past it stays.
Returns `{error, not_found}` when no such listener runs.
""".
-callback notify_drain(Name :: atom(), Timeout :: non_neg_integer()) -> ok | {error, not_found}.

-doc """
Recompiles the routes of every listener this adapter runs, from the route lists
they were started with. The dev reloader calls it after loading new code and
before broadcasting the reload, so a route term holding a fun from a reloaded
module is rebuilt before the purge can strand it. A listener that is gone is
skipped.
""".
-callback recompile_routes() -> ok.

-doc """
What the adapter can do beyond the baseline every adapter serves: the
capability route kinds (`arizona_route`: `mcp`, `reload`) and controller reply
kinds (`arizona_controller`: `stream`, `sendfile`, `loop`) it supports. Core
refuses the rest before anything happens -- a route at route compilation, a
reply before the session commits. Omitted, every kind is supported.
""".
-callback capabilities() ->
    #{
        routes := [mcp | reload],
        replies := [stream | sendfile | loop]
    }.

-optional_callbacks([capabilities/0]).
