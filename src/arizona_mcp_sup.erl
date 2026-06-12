-module(arizona_mcp_sup).
-moduledoc """
Dynamic supervisor for MCP session processes.

Owns the `arizona_mcp_session_registry` ETS table (created in `init/1`, so
it outlives every session) and starts one `arizona_mcp_session` child per
opened session. Children are `temporary`: a crashed session is gone and
must not be auto-restarted (its in-memory state is lost and its id is
meaningless), so the client re-`initialize`s for a fresh one.

Always supervised under `arizona_sup` -- an idle supervisor plus an empty
ETS table is a negligible footprint, and it avoids gating on route config
that is not known when `arizona_sup` starts.
""".

-behaviour(supervisor).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([start_link/0]).
-export([start_session/3]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

%% Referenced only by `arizona_sup`'s child spec (an MFA tuple, not a call).
-ignore_xref([start_link/0]).

%% --------------------------------------------------------------------
%% supervisor callback exports
%% --------------------------------------------------------------------

-export([init/1]).

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc "Start the session supervisor under the well-known name `arizona_mcp_sup`.".
-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, #{}).

-doc """
Start a session process holding `Session`, registered under `SessionId`.
`SessionOpts` carries the idle `ttl_ms` and the resumability `buffer_max`.
""".
-spec start_session(SessionId, Session, SessionOpts) -> supervisor:startchild_ret() when
    SessionId :: binary(),
    Session :: arizona_mcp_handler:session(),
    SessionOpts :: arizona_mcp_session:session_opts().
start_session(SessionId, Session, SessionOpts) ->
    supervisor:start_child(?MODULE, [SessionId, Session, SessionOpts]).

%% --------------------------------------------------------------------
%% supervisor Callbacks
%% --------------------------------------------------------------------

-spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} when
    Args :: map(),
    SupFlags :: supervisor:sup_flags(),
    ChildSpec :: supervisor:child_spec().
init(#{}) ->
    ok = arizona_mcp_session_registry:create_table(),
    SupFlags = #{strategy => simple_one_for_one},
    ChildSpec = #{
        id => arizona_mcp_session,
        start => {arizona_mcp_session, start_link, []},
        restart => temporary,
        type => worker
    },
    {ok, {SupFlags, [ChildSpec]}}.
