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
-export([add/3]).
-export([remove/1]).
-export([lookup/1]).
-export([count/1]).

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

-doc """
Record a session id -> pid mapping, tagged with its route key (the request
path it was opened on) so `count/1` can enforce a per-route `max_sessions`
cap. Called from the session's `init/1`.
""".
-spec add(SessionId, Pid, RouteKey) -> ok when
    SessionId :: binary(),
    Pid :: pid(),
    RouteKey :: binary().
add(SessionId, Pid, RouteKey) ->
    true = ets:insert(?TABLE, {SessionId, Pid, RouteKey}),
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
        [{_, Pid, _Mod}] ->
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

-doc """
Count the live sessions opened on route `RouteKey`, for the per-route
`max_sessions` cap. Dead pids found on the way (sessions killed without running
`terminate/2`) are swept, mirroring `lookup/1`'s lazy sweep -- a crashed
session's client never presents its id again, so without this sweep its stale
row would consume `max_sessions` capacity forever. The count is still
approximate under a burst of concurrent `initialize`s -- acceptable for a soft
capacity limit.

The scan is linear in the table, so an `initialize` on a capped route pays
roughly one microsecond per 15 live sessions -- bounded by the cap the operator
chose, and only on routes that set one. A cached per-route counter would make
it constant-time but could not self-heal: a session killed without running
`terminate/2` would leak an increment and lock its route out for good, where a
stale row is swept here or the next time its id is looked up.
""".
-spec count(RouteKey) -> non_neg_integer() when
    RouteKey :: binary().
count(RouteKey) ->
    Rows = ets:select(?TABLE, [{{'$1', '$2', RouteKey}, [], [{{'$1', '$2'}}]}]),
    Dead = [Id || {Id, Pid} <:- Rows, not is_process_alive(Pid)],
    ok = lists:foreach(fun remove/1, Dead),
    length(Rows) - length(Dead).
