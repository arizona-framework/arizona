-module(arizona_dev_log).
-moduledoc """
A bounded in-memory log buffer for the dev MCP's `get_logs` tool.

An agent driving a dev node over MCP has no terminal. Every other diagnostic
is reachable -- routes, docs, source locations, reloader state, arbitrary
`eval` -- but runtime log output goes to `logger_std_h`, which in a default dev
config writes to stdout and nowhere else. So the one artifact that names the
module and line of a crash is the one an agent cannot read, and a live process
dying on every interaction reads back as a healthy app.

This module closes that: a `logger` handler that keeps the last
1024 formatted events in a fixed-size ETS ring, queried by
`arizona_dev_mcp`'s `get_logs` tool.

## Ownership and lifetime

Split in two, because the two halves want different lifetimes and different
gates:

- The **table** is created by `arizona_sup:init/1` (`create_table/0`) so it is
  owned by a process that spans the node's lifetime, the same reason the
  reloader's beam-facts cache lives there. An empty table costs nothing.
- The **handler** is installed by `arizona_dev_mcp:init/1` (`install/0`), so
  only an app that actually mounts the dev MCP route pays the per-event write.
  A production node that never mounts `/mcp` captures nothing. It is removed
  again by `arizona_app:prep_stop/1` (`uninstall/0`), before the table's owner
  goes down.

The consequence worth knowing: capture starts at the first MCP session, not at
boot. Events logged before an agent first connects are not in the buffer. Call
`install/0` from your own dev boot code if you need coverage from startup.

## Level

The handler sets no level of its own, and deliberately does not touch the
primary logger level. It cannot: `logger` applies the primary level *before*
consulting any handler, so a handler asking for `debug` under a `notice`
primary still receives nothing below `notice`. Raising the primary level to
widen capture would change the running system's behaviour in order to observe
it. So the buffer holds whatever the app's own configuration already allows,
and `tail/1` filters at read time. Crash reports are `error`, which passes any
sane primary level, so the case this exists for is unaffected.

## Self-exclusion

Output produced by the MCP's own tool calls is skipped, so an agent reading
`get_logs` after an `eval` sees the app's behaviour rather than its own.
`arizona_dev_mcp` marks the calling process via `mark_self/0` and `log/2` drops
events carrying that metadata. Marking is per-process and `logger` metadata is
not inherited, so a process the tool call *spawns* is still captured -- which is
usually what you want, since that is where a crash surfaces.
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([create_table/0]).
-export([install/0]).
-export([uninstall/0]).
-export([mark_self/0]).
-export([tail/1]).

%% --------------------------------------------------------------------
%% logger handler callback exports
%% --------------------------------------------------------------------

-export([log/2]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

%% Called by `logger`, never from this project.
-ignore_xref([log/2]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([opts/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-nominal opts() :: #{
    count => pos_integer(),
    level => logger:level(),
    grep => binary()
}.

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------

-define(TABLE, ?MODULE).
%% Matches the size Tidewave's equivalent settled on. Bounded is the point: the
%% cost of always-on capture has to be a constant, not a function of uptime.
-define(SIZE, 1024).
-define(DEFAULT_COUNT, 50).
%% Metadata key marking an event as the MCP's own tool-call output.
-define(SKIP, arizona_dev_log_skip).

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc """
Creates the ring table. Called from `arizona_sup:init/1` so the supervisor owns
it and it outlives the short-lived processes that read and write it.
""".
-spec create_table() -> ok.
create_table() ->
    ?TABLE = ets:new(?TABLE, [named_table, public, {write_concurrency, true}]),
    true = ets:insert(?TABLE, {seq, 0}),
    ok.

-doc """
Installs the `logger` handler. Idempotent, so every MCP session can call it
without checking whether an earlier one already did.
""".
-spec install() -> ok.
install() ->
    case logger:add_handler(?TABLE, ?MODULE, #{}) of
        ok -> ok;
        {error, {already_exist, _}} -> ok
    end.

-doc """
Removes the handler. Called from `arizona_app:prep_stop/1`, before the
supervision tree that owns the table goes down -- otherwise the next log event
during shutdown writes to a dead table, and `logger` removes the handler for
crashing rather than because it was asked to. Tolerates not being installed,
which is the ordinary case for an app that never mounts the dev MCP route.
""".
-spec uninstall() -> ok.
uninstall() ->
    case logger:remove_handler(?TABLE) of
        ok -> ok;
        {error, {not_found, _}} -> ok
    end.

-doc """
Marks the calling process so its log output is kept out of the buffer. Called
by `arizona_dev_mcp` before dispatching a tool, so an agent does not read back
its own `eval` output as if it were the app's behaviour.
""".
-spec mark_self() -> ok.
mark_self() ->
    logger:update_process_metadata(#{?SKIP => true}).

-doc """
The buffered events, oldest first, after filtering.

`count` (default 50) takes from the newest end. `level` keeps
events at least as severe as the one given. `grep` keeps events matching a
case-insensitive regular expression. A malformed `grep` answers `{error, _}`
rather than raising, so the tool can report it in-band.
""".
-spec tail(Opts) -> {ok, [binary()]} | {error, binary()} when Opts :: opts().
tail(Opts) ->
    case compile_grep(maps:get(grep, Opts, undefined)) of
        {ok, Re} ->
            Count = maps:get(count, Opts, ?DEFAULT_COUNT),
            Level = maps:get(level, Opts, undefined),
            %% Only the 4-tuples are entries; the counter row is `{seq, N}`.
            Rows = lists:keysort(2, ets:match_object(?TABLE, {'_', '_', '_', '_'})),
            Matching = [
                Bin
             || {_Slot, _Seq, EventLevel, Bin} <- Rows,
                level_at_least(EventLevel, Level),
                matches(Bin, Re)
            ],
            {ok, last(Matching, Count)};
        {error, Reason} ->
            {error, Reason}
    end.

%% --------------------------------------------------------------------
%% logger handler Callbacks
%% --------------------------------------------------------------------

-spec log(LogEvent, Config) -> ok when
    LogEvent :: logger:log_event(),
    Config :: logger:handler_config().
log(#{meta := #{?SKIP := true}}, _Config) ->
    ok;
log(#{level := Level} = LogEvent, _Config) ->
    %% Formatted at write time so the buffer holds plain binaries: a read is
    %% then just a filter, and nothing in the event (pids, refs, a report's
    %% closure over process state) is retained past the moment it was logged.
    Bin = format(LogEvent),
    Seq = ets:update_counter(?TABLE, seq, 1),
    true = ets:insert(?TABLE, {Seq rem ?SIZE, Seq, Level, Bin}),
    ok.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% `logger_formatter` fills its own defaults from a bare config, and renders
%% both string and report events -- the latter being the crash reports this
%% buffer exists to capture. Left multi-line on purpose: a flattened stacktrace
%% is the part an agent most needs to read.
format(LogEvent) ->
    case unicode:characters_to_binary(logger_formatter:format(LogEvent, #{})) of
        Bin when is_binary(Bin) -> Bin;
        Error -> error({log_not_encodable, Error})
    end.

compile_grep(undefined) ->
    {ok, undefined};
compile_grep(Pattern) ->
    case re:compile(Pattern, [caseless, unicode]) of
        {ok, Re} ->
            {ok, Re};
        {error, {Reason, At}} ->
            {error,
                iolist_to_binary(
                    io_lib:format("invalid grep pattern at ~p: ~ts", [At, Reason])
                )}
    end.

matches(_Bin, undefined) ->
    true;
matches(Bin, Re) ->
    re:run(Bin, Re, [{capture, none}]) =:= match.

level_at_least(_EventLevel, undefined) ->
    true;
level_at_least(EventLevel, Level) ->
    logger:compare_levels(EventLevel, Level) =/= lt.

last(List, Count) ->
    lists:nthtail(max(0, length(List) - Count), List).
