-module(arizona_sup).
-moduledoc """
Top-level supervisor for the Arizona application.

Always supervises `arizona_pubsub` (the `pg`-based pubsub scope) and
`arizona_mcp_sup` (the MCP session supervisor, which owns the session
registry and starts per-session processes on demand), and owns the hot
reloader's beam-facts cache table (`arizona_reloader_consistency:create_table/0`,
created in `init/1` so the cache outlives the short-lived processes that read
it) and the dev MCP's log ring table (`arizona_dev_log:create_table/0`, for the
same reason). Also supervises one
`arizona_watcher` per rule when the dev-mode reloader is enabled via the
`reloader` application env, and the configured server-side session store when
the `session_store` env names a backend that exports `child_spec/0` (e.g.
`arizona_session_store_ets`). Live processes are not managed here -- they're
started ad hoc by the transport layer (`arizona_socket:init/4`) and linked
to the calling WebSocket process so they share its lifetime.

## Reloader config

```erlang
{arizona, [
    {reloader, #{
        enabled => true,
        rules => [
            #{directory => "src",
              patterns => [".*\\\\.erl$"],
              callback => fun arizona_reloader:reload_erl/1}
        ]
    }}
]}
```

Each rule map is passed to `arizona_watcher:start_link/2` with
`directory` stripped out. Malformed config (missing `directory`,
non-list `rules`) crashes the supervisor init so boot errors are
obvious.
""".
-behaviour(supervisor).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([start_link/0]).

%% --------------------------------------------------------------------
%% supervisor callback exports
%% --------------------------------------------------------------------

-export([init/1]).

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc """
Starts the supervisor under the well-known name `arizona_sup`.
""".
-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, #{}).

%% --------------------------------------------------------------------
%% supervisor Callbacks
%% --------------------------------------------------------------------

-spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} when
    Args :: map(),
    SupFlags :: supervisor:sup_flags(),
    ChildSpec :: supervisor:child_spec().
init(#{}) ->
    Reloader = arizona_config:get_env(reloader, #{}),
    ok = log_reloader_state(Reloader),
    %% Owned by this process so it spans the node's lifetime: the checks that
    %% read it run from short-lived callers (the dev MCP dispatch worker), and a
    %% cache owned by one of those dies with it. Ungated for the same reason
    %% `arizona_mcp_sup`'s registry is -- an empty named table costs nothing and
    %% the dev MCP drift check calls in whether the reloader is enabled or not.
    ok = arizona_reloader_consistency:create_table(),
    %% Same ownership reason: the dev MCP's log ring is written by whatever
    %% process logged and read by a short-lived tool dispatch, so it needs an
    %% owner that outlives both. Only the empty table is created here -- the
    %% handler that writes to it is installed by `arizona_dev_mcp:init/1`, so a
    %% node that never mounts the dev MCP route pays nothing per log event.
    ok = arizona_dev_log:create_table(),
    Children =
        [pubsub_spec(), mcp_sup_spec()] ++ store_specs() ++ watcher_specs(Reloader),
    {ok, {#{strategy => one_for_one}, Children}}.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% One boot line stating the reloader state, so a dev node silently missing hot
%% reload (env absent or disabled -- zero watchers started, nothing else says
%% so) is visible. Enabled is ordinary dev-mode operation: `info`. Disabled
%% logs at `notice`: OTP's default primary level is `notice`, so the line shows
%% on a default node -- where an `info` would be filtered before any handler --
%% without being an alarming warning. The clauses mirror watcher_specs/1, so
%% the line always states what actually started.
log_reloader_state(#{enabled := true, rules := Rules}) when is_list(Rules) ->
    logger:info("Arizona hot reload enabled: watching ~ts", [summarize_rules(Rules)]);
log_reloader_state(_Reloader) ->
    logger:notice("Arizona hot reload off; set the arizona `reloader` app env to enable it").

summarize_rules(Rules) ->
    lists:join("; ", [summarize_rule(Rule) || Rule <- Rules]).

summarize_rule(#{directory := Dir} = Rule) ->
    Patterns = maps:get(patterns, Rule, [".*"]),
    io_lib:format("~ts (~ts)", [Dir, lists:join(", ", Patterns)]).

pubsub_spec() ->
    #{
        id => arizona_pubsub,
        start => {arizona_pubsub, start_link, []},
        type => worker
    }.

%% Always supervised: the MCP session supervisor owns the session registry
%% (ETS) and starts per-session processes on demand. An idle supervisor with
%% an empty table costs nothing when no MCP route is configured.
mcp_sup_spec() ->
    #{
        id => arizona_mcp_sup,
        start => {arizona_mcp_sup, start_link, []},
        type => supervisor
    }.

%% When a server-side session store is configured (`session_store` app env), supervise
%% it if the backend declares a `child_spec/0` (the ETS store owns its table; a backend
%% that runs its own process can omit the callback).
store_specs() ->
    case arizona_config:get_env(session_store, undefined) of
        undefined ->
            [];
        Mod ->
            _ = code:ensure_loaded(Mod),
            case erlang:function_exported(Mod, child_spec, 0) of
                true -> [Mod:child_spec()];
                false -> []
            end
    end.

watcher_specs(#{enabled := true, rules := Rules}) when is_list(Rules) ->
    [watcher_spec(I, R) || {I, R} <- lists:enumerate(Rules)];
watcher_specs(_) ->
    [].

watcher_spec(I, #{directory := Dir} = Rule) ->
    Opts = maps:without([directory], Rule),
    #{
        id => {arizona_watcher, I},
        start => {arizona_watcher, start_link, [Dir, Opts]},
        type => worker
    }.
