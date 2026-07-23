-module(arizona_jsonrpc).
-moduledoc """
JSON-RPC 2.0 envelope codec.

Pure, transport-free helpers shared by the MCP transport
(`arizona_mcp_handler`): decode a request body into a normalized
message, and build the success / error response objects.

A decoded message is `#{method := binary(), params := map(), id := id()}`.
An absent (or null) `id` decodes to `undefined`, marking a
*notification* -- a message the server must never reply to. A present `id`
that is neither a string nor an integer (MCP's two request-id types) is an
*invalid request*, not a notification, so its sender still gets an answer.

Only single messages are decoded; a top-level JSON array (a JSON-RPC
*batch*) decodes to `{error, invalid_request}`. Batch support was
dropped from the MCP protocol, so a single-message codec is current.

Error codes follow the JSON-RPC 2.0 spec:

| Code | Meaning |
|------|---------|
| `-32700` | Parse error -- invalid JSON |
| `-32600` | Invalid Request -- not a well-formed request object |
| `-32601` | Method not found |
| `-32602` | Invalid params |
| `-32603` | Internal error |
""".

%% Local `error/3` shadows the auto-imported `erlang:error/3`; this module
%% never raises, so drop the import to keep the JSON-RPC builder unambiguous.
-compile({no_auto_import, [error/3]}).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([decode/1]).
-export([result/2]).
-export([error/3]).
-export([notification/2]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([request/0]).
-export_type([id/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-nominal id() :: binary() | integer() | null.

-nominal request() :: #{
    method := binary(),
    params := map(),
    %% `undefined` => notification (never answered).
    id := id() | undefined
}.

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc """
Decode a request body into a normalized JSON-RPC message.

Returns `{error, parse_error}` for invalid JSON, or
`{error, invalid_request}` for valid JSON that is not a well-formed
JSON-RPC 2.0 request object (wrong/absent `jsonrpc` version, a
non-binary or absent `method`, a non-object `params`, a present `id`
that is neither a string nor an integer, or a top-level array).
""".
-spec decode(Body) -> {ok, request()} | {error, parse_error | invalid_request} when
    Body :: iodata().
decode(Body) ->
    try json:decode(iolist_to_binary(Body)) of
        Decoded -> validate(Decoded)
    catch
        error:_ -> {error, parse_error}
    end.

-doc "Build a JSON-RPC success response object.".
-spec result(id(), Result) -> map() when
    Result :: term().
result(Id, Result) ->
    #{~"jsonrpc" => ~"2.0", ~"id" => Id, ~"result" => Result}.

-doc """
Build a JSON-RPC notification object -- a request with no `id`, which the
peer never answers. Used for server-initiated `notifications/*` messages.
""".
-spec notification(Method, Params) -> map() when
    Method :: binary(),
    Params :: map().
notification(Method, Params) ->
    #{~"jsonrpc" => ~"2.0", ~"method" => Method, ~"params" => Params}.

-doc "Build a JSON-RPC error response object (no `data` member).".
-spec error(id(), Code, Message) -> map() when
    Code :: integer(),
    Message :: binary().
error(Id, Code, Message) ->
    #{
        ~"jsonrpc" => ~"2.0",
        ~"id" => Id,
        ~"error" => #{~"code" => Code, ~"message" => Message}
    }.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

validate(#{~"jsonrpc" := ~"2.0", ~"method" := Method} = Msg) when is_binary(Method) ->
    maybe
        {ok, Params} ?= params(Msg),
        {ok, Id} ?= id(Msg),
        {ok, #{method => Method, params => Params, id => Id}}
    else
        error -> {error, invalid_request}
    end;
validate(_) ->
    {error, invalid_request}.

%% Params, when present, must be an object (MCP is by-name only). An
%% absent params member is the empty object.
params(#{~"params" := Params}) when is_map(Params) -> {ok, Params};
params(#{~"params" := _}) -> error;
params(_) -> {ok, #{}}.

%% A present string/integer id is a request id; an absent or null id is a
%% notification (`undefined`). A present id of any other JSON type -- a
%% fractional number (`2.5`, or the `1e2` a decoder also reads as a float), a
%% boolean, an array, an object -- is none of the two: MCP ids are
%% string-or-integer. Reporting it invalid answers the client that sent it,
%% where reading it as a notification would leave the request unanswered
%% forever (HTTP 202, no body).
id(#{~"id" := Id}) when is_binary(Id); is_integer(Id) -> {ok, Id};
id(#{~"id" := null}) -> {ok, undefined};
id(#{~"id" := _}) -> error;
id(_) -> {ok, undefined}.
