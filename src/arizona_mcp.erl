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
- `handle_tool/4` -- runs one `tools/call`, dispatched by name (the MCP
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

## Pagination

`tools/1` / `resources/1` / `prompts/1` always return their **full** list;
the transport paginates the `*/list` results with opaque cursors. A list at or
under the route's `page_size` opt (default 50) returns in one page with no
cursor; a larger one returns a page plus a `nextCursor` the client echoes to
fetch the next. The callbacks never see a cursor -- pagination is entirely the
transport's concern.

The cursor is an offset into the list the callback returns, so it assumes the
list is **stable across pages**: if the callback returns a different list
mid-pagination (an item added or removed between page requests), the client may
skip or repeat an item. This is fine for the near-static lists tools/resources/
prompts usually are -- and a client that receives a `list_changed` notification
re-lists from the start anyway.

## Tool results vs protocol errors

`handle_tool/4` distinguishes two failure kinds, matching the MCP spec:

- `{reply, Result, State}` -- the tool succeeded; `Result` becomes a
  `tools/call` result with `isError: false`.
- `{error, ToolError, State}` -- the tool *ran* but failed (bad domain
  input, upstream timeout); it becomes a `tools/call` result with
  `isError: true`, **not** a JSON-RPC error. The agent sees the failure
  in-band and can react.

A protocol-level fault (unknown method, unknown tool name, malformed
params) is the transport's concern and surfaces as a JSON-RPC error,
never reaching `handle_tool/4`.

## Progress

A `tools/call` may carry a `_meta.progressToken`; when it does, the tool can
stream `notifications/progress` to the client while it runs, before the final
result. `handle_tool/4`'s third argument is a progress context: call
`progress/2,3` with it to emit a progress update. When the client sent no
token (or in any non-streaming call) the context is inert and `progress/2,3`
is a no-op, so a tool can always call it unconditionally.

Streaming is automatic and transport-owned: a token-bearing `tools/call` from a
client that accepts `text/event-stream` answers its POST as an SSE stream (the
progress notifications, then the result, then close) instead of a buffered JSON
body. A client that does not accept SSE gets the buffered reply and the progress
is dropped. The tool just emits; it does not choose the transport.

## State

`init/1` is the state constructor; `handle_tool/4`, `read_resource/2`, and
`get_prompt/3` each return a new state alongside their result.

In **session** mode (a route with `sessions => true`) `init/1` runs once on
`initialize` and the state lives in the session process. Each callback's
returned state threads back into the session, so a `tools/call` that
increments a counter is visible to the session's next request -- the session
is genuinely stateful. The new state carries back whether the callback
succeeded (`{reply, ...}`) or failed in-band (`{error, ...}`), since a
ran-but-failed callback may have mutated state legitimately; only a *crash*
(a `-32603`) discards it, leaving the prior state intact.

In **stateless** mode the transport calls `init/1` per request and discards
the returned state, so the threaded-back state does not persist across
requests -- every request starts from a fresh `init/1`.

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

## Resource subscriptions and templates

A client subscribes to a resource with `resources/subscribe` (advertise
`resources => #{subscribe => true}` from `init/1`); the server announces a
change with `resource_updated/1`, which fans `notifications/resources/updated`
out to every subscribed session. It is built on the same pubsub path as
`broadcast/3`: subscribe joins a per-uri channel, `resource_updated/1`
broadcasts to it, and `resources/unsubscribe` (or session exit) leaves. Like
the other pushes it needs a session with an attached SSE channel.

The optional `resource_templates/1` callback advertises URI templates
(`mem://user/{id}`) through `resources/templates/list`. Omit it for a server
with no parameterized resources.

## Logging and completion

With `logging => #{}` advertised, the client sets a minimum severity via
`logging/setLevel` and the server pushes logs with `log/3` (a
`notifications/message`); a message below the session's level is dropped (the
default is `info`). With `completions => #{}` advertised, the optional
`complete/3` callback autocompletes a prompt or resource argument from
`completion/complete` -- it returns the matching strings and the transport caps
them at 100.

## Operational notes

Session mode starts one process per `initialize`, and the count is
unbounded -- a public deployment should gate the endpoint with the `auth`
hook and a reverse-proxy rate limit. An abandoned session is reaped after
its idle TTL (`session_ttl_ms`, default 5 minutes). A tool that never
returns holds its session and its connection until it does (a request waits
on the tool indefinitely). The optional `terminate/2` callback runs when a
session ends (DELETE, idle TTL, or shutdown); it does **not** run in
stateless mode, which has no session to end.

Clients that use `fetch` (browsers and the official MCP SDK) refuse to
connect to ports on the WHATWG Fetch "bad ports" blocklist, so mount the
listener on a normal HTTP port (not, e.g., 4045 / 6000 / 6666). The SSE
channel rides roadrunner's version-parity `{loop, ...}` path, so it works
over HTTP/1.1, h2, and h3 (h2/h3 over TLS). The endpoint is verified against
the official MCP SDK client.
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([broadcast/3]).
-export([notify/3]).
-export([resource_updated/1]).
-export([log/3]).
-export([level_severity/1]).
-export([parse_level/1]).
-export([progress/2]).
-export([progress/3]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

%% The server-push and progress APIs are called by applications, not from
%% within arizona.
-ignore_xref([broadcast/3, notify/3]).
-ignore_xref([resource_updated/1]).
-ignore_xref([log/3]).
-ignore_xref([progress/2, progress/3]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([init_params/0]).
-export_type([channel/0]).
-export_type([server_info/0]).
-export_type([capabilities/0]).
-export_type([state/0]).
-export_type([progress_ctx/0]).
-export_type([progress_opts/0]).
-export_type([tool/0]).
-export_type([content_block/0]).
-export_type([tool_result/0]).
-export_type([tool_error/0]).
-export_type([resource/0]).
-export_type([resource_template/0]).
-export_type([resource_contents/0]).
-export_type([resource_error/0]).
-export_type([prompt/0]).
-export_type([prompt_argument/0]).
-export_type([prompt_message/0]).
-export_type([prompt_result/0]).
-export_type([prompt_error/0]).
-export_type([log_level/0]).
-export_type([complete_ref/0]).
-export_type([complete_argument/0]).

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

%% Opaque to the app: a streaming `tools/call`'s context carries the client's
%% progress token and the connection to emit on; an inert context (`token` is
%% `undefined`) makes `progress/2,3` a no-op for non-streaming calls.
-nominal progress_ctx() :: #{
    token := binary() | integer() | undefined,
    to := pid() | undefined
}.

-nominal progress_opts() :: #{
    total => number(),
    message => binary()
}.

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

-nominal resource_template() :: #{
    uri_template := binary(),
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

%% The RFC 5424 / syslog severities MCP uses, ascending. `logging/setLevel` sets
%% the minimum; a message at or above it is delivered.
-nominal log_level() ::
    debug
    | info
    | notice
    | warning
    | error
    | critical
    | alert
    | emergency.

%% What `completion/complete` is completing: a prompt or a resource argument.
-nominal complete_ref() :: {prompt, binary()} | {resource, binary()}.

%% The argument being completed: its name and the partial value typed so far.
-nominal complete_argument() :: {binary(), binary()}.

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
`arguments` object. `Ctx` is the progress context: pass it to `progress/2,3`
to stream `notifications/progress` while the tool runs (a no-op when the
client sent no `_meta.progressToken`). Return `{reply, Result, State}` on
success or `{error, ToolError, State}` for an in-band tool failure.
""".
-callback handle_tool(
    Name :: binary(), Args :: map(), Ctx :: progress_ctx(), State :: state()
) ->
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
Return the URI-template metadata advertised by `resources/templates/list`.
Optional; a server with no parameterized resources omits it (the transport
then returns an empty template list).
""".
-callback resource_templates(State :: state()) -> [resource_template()].

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
Autocomplete a prompt or resource argument, dispatched from
`completion/complete`. `Ref` is `{prompt, Name}` or `{resource, Uri}`; `Arg` is
`{ArgName, PartialValue}`. Return `{reply, Values, State}` with the matching
completion strings (the transport caps them at 100 and sets `hasMore`).
Optional; implement it when advertising the `completions` capability.
""".
-callback complete(Ref :: complete_ref(), Arg :: complete_argument(), State :: state()) ->
    {reply, [binary()], state()}.

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
    resource_templates/1,
    prompts/1,
    get_prompt/3,
    complete/3,
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

-doc """
Announce that a resource changed to every session subscribed to its `Uri` (via
`resources/subscribe`). Each subscribed session forwards
`notifications/resources/updated` to its client; sessions that did not subscribe
(or have no SSE channel) ignore it. Decoupled -- the caller holds no session
reference.
""".
-spec resource_updated(Uri) -> ok when
    Uri :: binary().
resource_updated(Uri) ->
    broadcast({mcp_resource, Uri}, ~"notifications/resources/updated", #{~"uri" => Uri}).

-doc """
Send a log message (a `notifications/message`) to one session by its
`Mcp-Session-Id`. Dropped if `Level` is below the level the client set via
`logging/setLevel` (default `info`), or if the session is unknown/expired or has
no SSE channel. `Data` is any JSON-encodable term. Like `notify/3`, the id comes
from the handler's `initialize` params.
""".
-spec log(SessionId, Level, Data) -> ok when
    SessionId :: binary(),
    Level :: log_level(),
    Data :: term().
log(SessionId, Level, Data) ->
    %% Severity is resolved caller-side, so a bad level crashes the caller
    %% rather than the session.
    Severity = level_severity(Level),
    case arizona_mcp_session_registry:lookup(SessionId) of
        {ok, Pid} -> arizona_mcp_session:log(Pid, Severity, Level, Data);
        error -> ok
    end.

-doc false.
%% The numeric severity of a level, ascending (debug lowest). The transport
%% delivers a message only when its severity is at or above the session's set
%% minimum. Framework-internal; shared by the handler and `log/3`.
-spec level_severity(log_level()) -> 0..7.
level_severity(debug) -> 0;
level_severity(info) -> 1;
level_severity(notice) -> 2;
level_severity(warning) -> 3;
level_severity(error) -> 4;
level_severity(critical) -> 5;
level_severity(alert) -> 6;
level_severity(emergency) -> 7.

-doc false.
%% Parse a wire log level string into its atom, or `error` for an unknown one
%% (`logging/setLevel` answers a -32602). Framework-internal.
-spec parse_level(binary()) -> {ok, log_level()} | error.
parse_level(~"debug") -> {ok, debug};
parse_level(~"info") -> {ok, info};
parse_level(~"notice") -> {ok, notice};
parse_level(~"warning") -> {ok, warning};
parse_level(~"error") -> {ok, error};
parse_level(~"critical") -> {ok, critical};
parse_level(~"alert") -> {ok, alert};
parse_level(~"emergency") -> {ok, emergency};
parse_level(_Other) -> error.

-doc """
Emit a `notifications/progress` update from a running `tools/call`, using the
`Ctx` handed to `handle_tool/4`. `Progress` is the amount done so far. A no-op
when the client sent no `_meta.progressToken` (the context is inert), so a tool
can call it unconditionally.
""".
-spec progress(Ctx, Progress) -> ok when
    Ctx :: progress_ctx(),
    Progress :: number().
progress(Ctx, Progress) ->
    progress(Ctx, Progress, #{}).

-doc """
Emit a `notifications/progress` update with extras. `Opts` may carry `total`
(the expected final amount) and `message` (a human-readable status). A no-op
when the context is inert.
""".
-spec progress(Ctx, Progress, Opts) -> ok when
    Ctx :: progress_ctx(),
    Progress :: number(),
    Opts :: progress_opts().
progress(#{token := undefined}, _Progress, _Opts) ->
    ok;
progress(#{token := Token, to := To}, Progress, Opts) ->
    Params0 = #{~"progressToken" => Token, ~"progress" => Progress},
    Params1 = maybe_put_opt(~"total", total, Opts, Params0),
    Params = maybe_put_opt(~"message", message, Opts, Params1),
    To ! {mcp_progress, arizona_jsonrpc:notification(~"notifications/progress", Params)},
    ok.

%% Copy an optional progress field onto the params under its wire name.
maybe_put_opt(WireKey, OptKey, Opts, Params) ->
    case Opts of
        #{OptKey := Value} -> Params#{WireKey => Value};
        _ -> Params
    end.
