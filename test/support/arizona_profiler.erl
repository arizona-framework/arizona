-module(arizona_profiler).
-moduledoc """
eprof/fprof wrapper for `scripts/profile.escript`.

Profile workloads run in-process: the escript is both the load driver
and the system under load. Seeds profiling with `[self()]` plus
`set_on_spawn => true`, so processes spawned during the workload
(the live `gen_server` from `arizona_socket:init/4`, cowboy connection
processes, pubsub subscribers) get auto-traced.

eprof is the default tool — fast, gives total time per MFA. fprof is
opt-in via `start_fprof/1` + `stop_fprof_and_dump/2` for the richer
call-tree analysis (OWN/ACC time, callers/callees) when eprof's flat
profile isn't enough.
""".

-export([start/0]).
-export([stop_and_dump/2]).
-export([start_fprof/1]).
-export([stop_fprof_and_dump/2]).

%% The `sink_loop/0` receive is indefinite by design -- it waits for
%% io_request messages from the eprof gen_server (forwarded while we
%% own its group_leader) and for an explicit `stop` from the caller
%% once analyze returns. Adding a timeout would be arbitrary or risk
%% dropping io_request messages mid-analyze.
-elvis([{elvis_style, no_receive_without_timeout, disable}]).

-spec start() -> ok.
start() ->
    {ok, _} = eprof:start(),
    profiling = eprof:start_profiling(
        [self()], {'_', '_', '_'}, [{set_on_spawn, true}]
    ),
    ok.

-doc """
Stop eprof, write the analysis log to `Path`, and filter out MFAs
whose total time is below `MinMs` milliseconds.

`eprof:analyze/2` writes to BOTH the log file (via `eprof:log/1`) and
the eprof gen_server's stdout. The analyze runs inside the eprof
process, so we swap in a sink group_leader on that process for the
duration of the call. That keeps the caller's stdout free for a clean
single-print of the log file content with our own ordering.
""".
-spec stop_and_dump(Path, MinMs) -> ok when
    Path :: file:filename(),
    MinMs :: float().
stop_and_dump(Path, MinMs) ->
    profiling_stopped = eprof:stop_profiling(),
    ok = eprof:log(Path),
    MinUs = trunc(MinMs * 1000),
    EprofPid = whereis(eprof),
    {group_leader, OldGl} = erlang:process_info(EprofPid, group_leader),
    Sink = spawn(fun sink_loop/0),
    true = group_leader(Sink, EprofPid),
    try
        eprof:analyze(total, [{filter, [{time, MinUs}]}])
    after
        true = group_leader(OldGl, EprofPid),
        Sink ! stop
    end,
    stopped = eprof:stop(),
    ok.

sink_loop() ->
    receive
        {io_request, From, ReplyAs, _Req} ->
            From ! {io_reply, ReplyAs, ok},
            sink_loop();
        stop ->
            ok
    end.

-doc """
Start an `fprof` trace seeded with `[self()]` and `set_on_spawn`.
`TraceFile` is the binary trace output; pass it to
`stop_fprof_and_dump/2` along with the desired analysis output path.
""".
-spec start_fprof(TraceFile) -> ok when
    TraceFile :: file:filename().
start_fprof(TraceFile) ->
    ok = fprof:trace([
        start,
        {procs, [self()]},
        {file, TraceFile},
        verbose
    ]),
    ok.

-doc """
Stop the fprof trace, profile, and write the totals analysis to
`AnalysisPath`. Output groups results in a `totals` section followed
by per-MFA blocks with `OWN` (exclusive) and `ACC` (inclusive) time.
""".
-spec stop_fprof_and_dump(TraceFile, AnalysisPath) -> ok when
    TraceFile :: file:filename(),
    AnalysisPath :: file:filename().
stop_fprof_and_dump(TraceFile, AnalysisPath) ->
    fprof:trace(stop),
    ok = fprof:profile([{file, TraceFile}]),
    ok = fprof:analyse([
        {dest, AnalysisPath},
        {cols, 120},
        {totals, true},
        {sort, own},
        no_callers,
        no_details
    ]),
    ok = fprof:stop(),
    ok.
