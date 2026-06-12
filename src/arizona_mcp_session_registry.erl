-module(arizona_mcp_session_registry).
-moduledoc """
Lookup layer mapping an `Mcp-Session-Id` to its session process.

A thin, process-free API over a public named ETS table owned by
`arizona_mcp_sup` (created once in the supervisor's `init/1`, before any
session starts, so it outlives every session). Lookups are lock-free
concurrent reads straight from the roadrunner connection process -- no
message passing on the request hot path.

Sessions add themselves on start and remove themselves on terminate; a
`lookup/1` that finds a dead pid (a session that crashed without running
`terminate/2`) sweeps the stale row and reports `error`.
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([create_table/0]).
-export([add/2]).
-export([remove/1]).
-export([lookup/1]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------

-define(TABLE, arizona_mcp_sessions).

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc """
Create the registry table. Called once by `arizona_mcp_sup:init/1`; the
supervisor process owns the table so it survives session churn.
""".
-spec create_table() -> ok.
create_table() ->
    _ = ets:new(?TABLE, [named_table, public, {read_concurrency, true}]),
    ok.

-doc "Record a session id -> pid mapping. Called from the session's `init/1`.".
-spec add(SessionId, Pid) -> ok when
    SessionId :: binary(),
    Pid :: pid().
add(SessionId, Pid) ->
    true = ets:insert(?TABLE, {SessionId, Pid}),
    ok.

-doc "Drop a session id mapping. Called from the session's `terminate/2`.".
-spec remove(SessionId) -> ok when
    SessionId :: binary().
remove(SessionId) ->
    true = ets:delete(?TABLE, SessionId),
    ok.

-doc """
Resolve a session id to its live process. Returns `error` for an unknown
id, or for a stale row whose process has died (sweeping the row).
""".
-spec lookup(SessionId) -> {ok, pid()} | error when
    SessionId :: binary().
lookup(SessionId) ->
    case ets:lookup(?TABLE, SessionId) of
        [{_, Pid}] ->
            case is_process_alive(Pid) of
                true ->
                    {ok, Pid};
                false ->
                    ok = remove(SessionId),
                    error
            end;
        [] ->
            error
    end.
