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
  analogue of `arizona_stateful:handle_event/3`).
- `resources/1` + `read_resource/2` -- optional; list and read resources.
- `prompts/1` + `get_prompt/3` -- optional; list and render prompts.
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

## State, per phase

`init/1` is the state constructor. Today the transport is stateless --
it calls `init/1` per request and discards the returned state after the
response. When per-session servers land, `init/1` will run once on
`initialize` and the state will live in the session process across the
session's requests; the callback contract here does not change.
""".

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([init_params/0]).
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
request.
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

-doc "Optional cleanup hook.".
-callback terminate(Reason :: term(), State :: state()) -> term().

-optional_callbacks([
    resources/1,
    read_resource/2,
    prompts/1,
    get_prompt/3,
    terminate/2
]).
