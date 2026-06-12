-module(arizona_mcp).
-moduledoc """
Behaviour for MCP (Model Context Protocol) servers.

An Arizona app implements this behaviour to expose **tools** to AI
agents over MCP's Streamable HTTP transport, then mounts it on a route:

```erlang
{mcp, ~"/mcp", my_mcp, #{origins => [~"http://localhost:3000"]}}
```

`arizona_mcp_handler` drives the transport (JSON-RPC over HTTP POST) and
calls back into this module to negotiate the connection and run tools.

## Callbacks

- `init/1` -- runs per request to build the server identity, the
  advertised capability map, and a handler state value threaded into the
  tool callbacks. Called once on `initialize`, and again to source the
  state for `tools/list` / `tools/call`.
- `tools/1` -- returns the tool metadata advertised by `tools/list`.
- `handle_tool/3` -- runs one `tools/call`, dispatched by name (the MCP
  analogue of how `arizona_stateful` dispatches an event by name).
- `resources/1` + `read_resource/2` -- optional; list and read resources.
- `prompts/1` + `get_prompt/3` -- optional; list and render prompts.
- `channels/1` -- optional; pubsub channels a session subscribes to for
  server-initiated notifications (see below).
- `terminate/2` -- optional cleanup hook.

Resources and prompts are gated on the advertised capability map: the
transport routes `resources/*` / `prompts/*` only when `init/1` advertised
the matching `resources` / `prompts` capability, returning `method not
found` otherwise. A server advertising a capability must implement its
callbacks. Tools are always available (their callbacks are required).

## Tool results vs protocol errors

`handle_tool/3` distinguishes two failure kinds, matching the MCP spec:

- `{reply, Result, State}` -- the tool succeeded; `Result` becomes a
  `tools/call` result with `isError: false`.
- `{error, ToolError, State}` -- the tool *ran* but failed (bad domain
  input, upstream timeout); it becomes a `tools/call` result with
  `isError: true`, **not** a JSON-RPC error. The agent sees the failure
  in-band and can react.

A protocol-level fault (unknown method, unknown tool name, malformed
params) is the transport's concern and surfaces as a JSON-RPC error,
never reaching `handle_tool/3`.

## State

`init/1` is the state constructor. In **stateless** mode the transport
calls it per request and discards the returned state. In **session** mode
(a route with `sessions => true`) it runs once on `initialize` and the
state lives in the session process, read by that session's later requests.
The state is not yet mutated by method calls -- threading a stateful
callback's returned state back into the session is a later refinement, and
the callback contract here does not change when it lands.

## Server-initiated notifications

A session (a route with `sessions => true`) can stream `notifications/*`
messages to a client that has opened the server-to-client SSE channel
(`GET` on the route). Two ways to push, both no-ops when the addressed
session has no channel attached:

- **Broadcast** -- `broadcast/3` publishes to a pubsub channel; every
  session subscribed to it (via the optional `channels/1` callback)
  forwards the notification. Natural for `notifications/*/list_changed`
  and resource-updated fan-out, and decoupled (the caller holds no session
  reference).
- **Targeted** -- `notify/3` pushes to one session by its `Mcp-Session-Id`.
  The id is handed to the handler at `initialize` under the `mcp_session_id`
  key of the init params, so a handler can keep it and address its own
  session later.

## Operational notes

Session mode starts one process per `initialize`, and the count is
unbounded -- a public deployment should gate the endpoint with the `auth`
hook and a reverse-proxy rate limit. An abandoned session is reaped after
its idle TTL (`session_ttl_ms`, default 5 minutes). A tool that never
returns holds its session and its connection until it does (a request waits
on the tool indefinitely). The optional `terminate/2` callback runs when a
session ends (DELETE, idle TTL, or shutdown); it does **not** run in
stateless mode, which has no session to end.
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([broadcast/3]).
-export([notify/3]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

%% The server-push API is called by applications, not from within arizona.
-ignore_xref([broadcast/3, notify/3]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([init_params/0]).
-export_type([channel/0]).
-export_type([server_info/0]).
-export_type([capabilities/0]).
-export_type([state/0]).
-export_type([tool/0]).
-export_type([content_block/0]).
-export_type([tool_result/0]).
-export_type([tool_error/0]).
-export_type([resource/0]).
-export_type([resource_contents/0]).
-export_type([resource_error/0]).
-export_type([prompt/0]).
-export_type([prompt_argument/0]).
-export_type([prompt_message/0]).
-export_type([prompt_result/0]).
-export_type([prompt_error/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-nominal init_params() :: map().

-nominal channel() :: arizona_pubsub:channel().

-nominal server_info() :: #{
    name := binary(),
    version := binary(),
    title => binary()
}.

-nominal capabilities() :: map().

-nominal state() :: term().

-nominal tool() :: #{
    name := binary(),
    description := binary(),
    %% A JSON Schema object, rendered verbatim as the tool's `inputSchema`.
    input_schema := map(),
    title => binary()
}.

-nominal content_block() :: #{type := binary(), _ => term()}.

%% A bare binary is shorthand for a single text content block.
-nominal tool_result() ::
    binary()
    | #{content := [content_block()], structured_content => map()}.

-nominal tool_error() ::
    binary()
    | #{content := [content_block()]}.

-nominal resource() :: #{
    uri := binary(),
    name := binary(),
    description => binary(),
    mime_type => binary()
}.

%% A bare binary is shorthand for a single text content entry keyed by the
%% requested uri. The map form supplies its own content entries verbatim.
-nominal resource_contents() ::
    binary()
    | #{contents := [map()]}.

-nominal resource_error() :: binary().

-nominal prompt() :: #{
    name := binary(),
    description => binary(),
    arguments => [prompt_argument()]
}.

-nominal prompt_argument() :: #{
    name := binary(),
    description => binary(),
    required => boolean()
}.

-nominal prompt_message() :: #{role := binary(), content := map()}.

-nominal prompt_result() :: #{
    messages := [prompt_message()],
    description => binary()
}.

-nominal prompt_error() :: binary().

%% --------------------------------------------------------------------
%% Behaviour callbacks
%% --------------------------------------------------------------------

-doc """
Build the server identity, advertised capabilities, and handler state.

`InitParams` carries the client's `initialize` params (its protocol
version, capabilities, and `clientInfo`) when invoked from `initialize`,
or the empty map when the transport reconstructs state for a later
request. In session mode it also carries the atom key `mcp_session_id`.

`Capabilities` must be an **atom-keyed** map (e.g. `#{tools => #{}}`): the
transport gates `resources/*` and `prompts/*` by matching the atom
capability name, so a binary-keyed capability would never be served.
""".
-callback init(InitParams :: init_params()) ->
    {ok, ServerInfo :: server_info(), Capabilities :: capabilities(), State :: state()}.

-doc "Return the tool metadata advertised by `tools/list`.".
-callback tools(State :: state()) -> [tool()].

-doc """
Run one `tools/call`, dispatched by tool name. `Args` is the decoded
`arguments` object. Return `{reply, Result, State}` on success or
`{error, ToolError, State}` for an in-band tool failure.
""".
-callback handle_tool(Name :: binary(), Args :: map(), State :: state()) ->
    {reply, tool_result(), state()}
    | {error, tool_error(), state()}.

-doc """
Return the resource metadata advertised by `resources/list`. Optional;
implement it (with `read_resource/2`) when advertising the `resources`
capability.
""".
-callback resources(State :: state()) -> [resource()].

-doc """
Read one resource by uri, dispatched from `resources/read`. The uri is
guaranteed to be one returned by `resources/1`. Return
`{reply, Contents, State}` on success or `{error, Message, State}` for a
resource-level failure (surfaced as a `-32002` error).
""".
-callback read_resource(Uri :: binary(), State :: state()) ->
    {reply, resource_contents(), state()}
    | {error, resource_error(), state()}.

-doc """
Return the prompt metadata advertised by `prompts/list`. Optional;
implement it (with `get_prompt/3`) when advertising the `prompts`
capability.
""".
-callback prompts(State :: state()) -> [prompt()].

-doc """
Render one prompt by name, dispatched from `prompts/get`. `Args` is the
decoded `arguments` object. Return `{reply, Result, State}` on success or
`{error, Message, State}` for a prompt-level failure.
""".
-callback get_prompt(Name :: binary(), Args :: map(), State :: state()) ->
    {reply, prompt_result(), state()}
    | {error, prompt_error(), state()}.

-doc """
Return the pubsub channels this session subscribes to for server-initiated
notifications. Anything published to one of these channels via `broadcast/3`
is forwarded to the session's SSE channel. Optional; defaults to none.
""".
-callback channels(State :: state()) -> [channel()].

-doc "Optional cleanup hook.".
-callback terminate(Reason :: term(), State :: state()) -> term().

-optional_callbacks([
    resources/1,
    read_resource/2,
    prompts/1,
    get_prompt/3,
    channels/1,
    terminate/2
]).

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc """
Broadcast a server-initiated notification to every session subscribed to
`Channel` (via its `channels/1` callback). Each subscribed session with an
attached SSE channel forwards `notifications/<Method>` with `Params` to its
client; sessions without an attached channel ignore it.
""".
-spec broadcast(Channel, Method, Params) -> ok when
    Channel :: channel(),
    Method :: binary(),
    Params :: map().
broadcast(Channel, Method, Params) ->
    arizona_pubsub:broadcast(Channel, {arizona_mcp_notification, Method, Params}).

-doc """
Push a server-initiated notification to one session by its `Mcp-Session-Id`.
A no-op if the session is unknown/expired or has no SSE channel attached.
The id is supplied to the handler at `initialize` (the `mcp_session_id` key
of the init params).
""".
-spec notify(SessionId, Method, Params) -> ok when
    SessionId :: binary(),
    Method :: binary(),
    Params :: map().
notify(SessionId, Method, Params) ->
    case arizona_mcp_session_registry:lookup(SessionId) of
        {ok, Pid} -> arizona_mcp_session:notify(Pid, Method, Params);
        error -> ok
    end.
