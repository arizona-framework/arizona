#!/usr/bin/env escript
%%% Performance bench for Arizona.
%%%
%%% Run via `make bench` (or `./scripts/bench.escript`). Never wired
%%% into `make ci` / `make precommit` -- numbers are noisy under
%%% shared CI runners and need human comparison, not auto-thresholds.
%%%
%%% Usage: ./bench.escript [runs] [--timetrap-ns N]
%%%   runs           number of measured trials (default 100)
%%%   --timetrap-ns  fail with exit 1 if mean per-op exceeds N nanoseconds
%%%                  (off by default; intended for local pre-push hooks,
%%%                  not CI -- shared runners have ~30% variance)

-mode(compile).

-define(WARMUP, 2).
-define(OPS_PER_TRIAL, 1000).
-define(DEFAULT_RUNS, 100).

main(Args) ->
    {Runs, Timetrap} = parse_args(Args),
    ProjectDir = project_dir(),
    ok = setup_code_paths(ProjectDir),
    {ok, _} = application:ensure_all_started(arizona),

    print_header(Runs, ProjectDir),

    Stats = bench_render_view_small(Runs),
    report(<<"render_view_small">>, Stats),

    check_timetrap(Stats, Timetrap).

%% ---------------------------------------------------------------------------
%% Workloads
%% ---------------------------------------------------------------------------

bench_render_view_small(Runs) ->
    Req = arizona_req_test_adapter:new(),
    %% Sanity-check before timing: render once outside the loop. Catches
    %% a regression where the function silently returns [] (which would
    %% otherwise look like an impossibly fast bench).
    Sample = arizona_render:render_view_to_iolist(arizona_root_counter, Req, #{}),
    case iolist_size(Sample) > 0 of
        true ->
            ok;
        false ->
            io:format("error: render_view_to_iolist returned empty output~n"),
            halt(1)
    end,
    %% No iolist_size in the timed loop: BEAM does not DCE opaque module
    %% calls, so the call always executes. Adding iolist_size walked the
    %% returned tree and biased the measurement upward by ~10%.
    Fun = fun() ->
        arizona_render:render_view_to_iolist(arizona_root_counter, Req, #{})
    end,
    run_workload(Fun, Runs).

%% ---------------------------------------------------------------------------
%% Harness
%% ---------------------------------------------------------------------------

run_workload(Fun, Runs) ->
    %% Warmup: prime the JIT, populate term/binary heap. Discard.
    lists:foreach(fun(_) -> trial(Fun) end, lists:seq(1, ?WARMUP)),
    %% Measured trials. Per-trial value is the mean per-op time in
    %% nanoseconds (batch divided by op count).
    PerOpNs = [
        begin
            erlang:garbage_collect(self()),
            TrialNs = trial(Fun),
            TrialNs / ?OPS_PER_TRIAL
        end
     || _ <- lists:seq(1, Runs)
    ],
    stats(PerOpNs).

trial(Fun) ->
    T0 = erlang:monotonic_time(nanosecond),
    loop(Fun, ?OPS_PER_TRIAL),
    T1 = erlang:monotonic_time(nanosecond),
    T1 - T0.

loop(_Fun, 0) ->
    ok;
loop(Fun, N) ->
    Fun(),
    loop(Fun, N - 1).

%% ---------------------------------------------------------------------------
%% Stats
%%
%% Sample variance (divides by N-1, not N). With N>=100, p50/p99 are
%% real percentile picks; with smaller N, "p99" approximates max-of-N
%% rather than a true tail latency. v1 default is N=100.
%% ---------------------------------------------------------------------------

stats(Samples) ->
    Sorted = lists:sort(Samples),
    Len = length(Sorted),
    Sum = lists:sum(Sorted),
    Mean = Sum / Len,
    Variance =
        case Len of
            1 -> 0.0;
            _ -> lists:sum([(X - Mean) * (X - Mean) || X <- Sorted]) / (Len - 1)
        end,
    Stdev = math:sqrt(Variance),
    P50 = lists:nth(max(1, Len div 2), Sorted),
    P99 = lists:nth(max(1, (Len * 99) div 100), Sorted),
    OpsPerSec = round(1_000_000_000 / Mean),
    #{
        n_trials => Len,
        ops_per_trial => ?OPS_PER_TRIAL,
        mean_ns => Mean,
        stdev_ns => Stdev,
        p50_ns => P50,
        p99_ns => P99,
        ops_per_s => OpsPerSec
    }.

%% ---------------------------------------------------------------------------
%% Reporting
%% ---------------------------------------------------------------------------

print_header(Runs, ProjectDir) ->
    io:format(
        "~narizona bench v1 | git: ~s | OTP ~s | runs=~b ops/trial=~b~n",
        [git_short_sha(ProjectDir), erlang:system_info(otp_release), Runs, ?OPS_PER_TRIAL]
    ).

report(Label, #{
    n_trials := N,
    ops_per_trial := Ops,
    mean_ns := MeanNs,
    stdev_ns := StdevNs,
    p50_ns := P50Ns,
    p99_ns := P99Ns,
    ops_per_s := OpsPerSec
}) ->
    io:format(
        "~-22s N=~b ops=~b  mean=~s  stdev=~s  p50=~s  p99=~s  ops/s=~s~n",
        [
            Label,
            N,
            Ops,
            fmt_time(MeanNs),
            fmt_time(StdevNs),
            fmt_time(P50Ns),
            fmt_time(P99Ns),
            fmt_int(OpsPerSec)
        ]
    ).

fmt_time(Ns) when Ns >= 1000 ->
    io_lib:format("~.1fµs", [Ns / 1000]);
fmt_time(Ns) ->
    io_lib:format("~bns", [trunc(Ns)]).

fmt_int(N) ->
    %% Group thousands with commas: 81300 -> "81,300".
    Reversed = lists:reverse(integer_to_list(N)),
    lists:reverse(group_thousands(Reversed)).

group_thousands([A, B, C, D | Rest]) ->
    [A, B, C, $, | group_thousands([D | Rest])];
group_thousands(Rest) ->
    Rest.

git_short_sha(ProjectDir) ->
    %% Use -C so the SHA reflects the project, not whatever cwd we were
    %% invoked from. Best effort; benches still run if not in a git checkout.
    Cmd = "git -C " ++ ProjectDir ++ " rev-parse --short HEAD 2>/dev/null",
    case os:cmd(Cmd) of
        "" -> "unknown";
        Sha -> string:trim(Sha)
    end.

%% ---------------------------------------------------------------------------
%% Timetrap (optional CI gate)
%% ---------------------------------------------------------------------------

check_timetrap(_Stats, infinity) ->
    ok;
check_timetrap(#{mean_ns := MeanNs}, MaxNs) when MeanNs =< MaxNs ->
    io:format("~ntimetrap: ~b ns <= ~b ns [PASS]~n", [trunc(MeanNs), MaxNs]),
    ok;
check_timetrap(#{mean_ns := MeanNs}, MaxNs) ->
    io:format("~ntimetrap: ~b ns > ~b ns [FAIL]~n", [trunc(MeanNs), MaxNs]),
    halt(1).

%% ---------------------------------------------------------------------------
%% Args + paths
%% ---------------------------------------------------------------------------

parse_args(Args) ->
    %% Walk all args so the timetrap flag works regardless of position.
    parse_args(Args, ?DEFAULT_RUNS, infinity).

parse_args([], Runs, Timetrap) ->
    {Runs, Timetrap};
parse_args(["--timetrap-ns", NsStr | Rest], Runs, _Timetrap) ->
    parse_args(Rest, Runs, list_to_integer(NsStr));
parse_args([RunsStr | Rest], _Runs, Timetrap) ->
    parse_args(Rest, list_to_integer(RunsStr), Timetrap).

setup_code_paths(BaseDir) ->
    %% Prefer the test profile's lib dir so test/support/ modules
    %% (e.g. arizona_req_test_adapter, arizona_root_counter) are available.
    %% Fall back to default if test profile isn't compiled.
    Candidates = [
        filename:join([BaseDir, "_build", "test", "lib"]),
        filename:join([BaseDir, "_build", "default", "lib"])
    ],
    LibDir =
        case lists:filter(fun filelib:is_dir/1, Candidates) of
            [Found | _] ->
                Found;
            [] ->
                io:format("error: no compiled libs found; run 'rebar3 as test compile' first~n"),
                halt(1)
        end,
    {ok, Libs} = file:list_dir(LibDir),
    lists:foreach(
        fun(Lib) ->
            %% Standard ebin/ output:
            EbinDir = filename:join([LibDir, Lib, "ebin"]),
            case filelib:is_dir(EbinDir) of
                true -> code:add_pathz(EbinDir);
                false -> ok
            end,
            %% Test profile compiles test/support/ beams into a sibling
            %% test/ directory (not ebin). Add it when present so fixtures
            %% like arizona_req_test_adapter and arizona_root_counter resolve.
            TestDir = filename:join([LibDir, Lib, "test"]),
            case filelib:is_dir(TestDir) of
                true -> code:add_pathz(TestDir);
                false -> ok
            end
        end,
        Libs
    ),
    ok.

project_dir() ->
    filename:dirname(filename:absname(filename:dirname(escript:script_name()))).
