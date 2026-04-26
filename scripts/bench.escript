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

    Workloads = [
        {<<"render_view_small">>, fun bench_render_view_small/1},
        {<<"render_view_with_layout">>, fun bench_render_view_with_layout/1},
        {<<"mount_only">>, fun bench_mount_only/1},
        {<<"diff_no_change">>, fun bench_diff_no_change/1},
        {<<"diff_simple_event">>, fun bench_diff_simple_event/1}
    ],
    Results = [{Label, Fun(Runs)} || {Label, Fun} <- Workloads],
    [report(Label, Stats) || {Label, Stats} <- Results],

    check_timetrap_all(Results, Timetrap).

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

bench_render_view_with_layout(Runs) ->
    %% Same as render_view_small but wraps the page in arizona_layout
    %% (DOCTYPE + html/head/body + nav + script tag). Every real HTTP
    %% GET goes through layout application; this catches regressions in
    %% `apply_layouts/3` and the layout's auto-detected `az_nodiff`
    %% semantics that render_view_small bypasses.
    Req = arizona_req_test_adapter:new(),
    Opts = #{layouts => [{arizona_layout, render}]},
    Sample = arizona_render:render_view_to_iolist(arizona_root_counter, Req, Opts),
    case iolist_size(Sample) > 0 of
        true ->
            ok;
        false ->
            io:format("error: render_view_to_iolist with layout returned empty~n"),
            halt(1)
    end,
    Fun = fun() ->
        arizona_render:render_view_to_iolist(arizona_root_counter, Req, Opts)
    end,
    run_workload(Fun, Runs).

bench_mount_only(Runs) ->
    %% Each iteration spawns a fresh live process via arizona_socket:init/4
    %% (start_link + mount/2). Measures the cold WS-connect cost: dep
    %% tracking setup, handler mount/2 invocation, gen_server start.
    %%
    %% Cleanup strategy: synchronously kill + wait per iteration. This
    %% keeps the system in steady state -- ~1 alive live process at any
    %% instant during the trial -- which matches typical app conditions
    %% (a few concurrent WS connections, not thousands).
    %%
    %% The measurement therefore includes ~1 µs of cleanup harness
    %% overhead (monitor + DOWN receive). This is a CONSTANT that does
    %% not change with mount-path optimizations, so regressions in mount
    %% itself still surface as proportional changes.
    %%
    %% An alternative -- spawn a worker per trial and let link
    %% propagation cascade-kill all live processes at trial exit -- was
    %% tried and abandoned: it accumulated 1000 alive processes inside
    %% the timed region, which inflated the mean (later iterations ran
    %% against scheduler/GC load from earlier ones) and tripled the
    %% stdev (~16% vs ~2%).
    %%
    %% The Socket is the record `{socket, Pid, ViewId, Req}` -- we use
    %% element/2 instead of including the record header in this escript.
    Req = arizona_req_test_adapter:new(),
    case arizona_socket:init(arizona_root_counter, #{}, Req, #{}) of
        {ok, TestSock} when is_pid(element(2, TestSock)) ->
            kill_live(element(2, TestSock));
        Other ->
            io:format("error: init returned unexpected ~p~n", [Other]),
            halt(1)
    end,
    Fun = fun() ->
        {ok, Sock} = arizona_socket:init(arizona_root_counter, #{}, Req, #{}),
        kill_live(element(2, Sock)),
        ok
    end,
    run_workload(Fun, Runs).

kill_live(Pid) ->
    Mon = monitor(process, Pid),
    unlink(Pid),
    exit(Pid, kill),
    receive
        {'DOWN', Mon, process, Pid, _} -> ok
    end.

bench_diff_no_change(Runs) ->
    %% Mount once, then dispatch N `noop` events. `noop` returns the same
    %% bindings, so `compute_changed/2` is empty and `arizona_diff:diff/4`
    %% short-circuits via the dep-skip fast path -- the cheapest event in
    %% the system. If `compute_changed` or the dep-skip wiring regresses,
    %% this workload spikes first.
    Req = arizona_req_test_adapter:new(),
    {ok, Socket} = arizona_socket:init(arizona_root_counter, #{}, Req, #{}),
    %% Pre-encode the JSON event payload outside the timed region so we're
    %% only measuring handle_in + dispatch + diff, not term construction.
    Json = iolist_to_binary(json:encode([~"counter", ~"noop", #{}])),
    %% Sanity-check: noop returns {ok, _} (no ops, no effects). Fail loud
    %% on a regression that would silently make the bench measure the
    %% wrong path (e.g. if noop started returning {reply, ...}).
    {ok, _} = arizona_socket:handle_in(Json, Socket),
    %% The Socket record is invariant under noop dispatch (no field changes)
    %% so we reuse the same Socket for every iteration -- no state threading
    %% needed in the inner loop.
    Fun = fun() ->
        {ok, _} = arizona_socket:handle_in(Json, Socket),
        ok
    end,
    run_workload(Fun, Runs).

bench_diff_simple_event(Runs) ->
    %% Mount once, then dispatch N `inc` events. Each event changes the
    %% `count` binding, so `compute_changed/2` returns one key and
    %% `arizona_diff:diff/4` emits one OP_TEXT op. Compared against
    %% `diff_no_change`, the delta is the cost of actually running the
    %% diff engine on a single-binding change -- the typical hot path
    %% for stateful UIs reacting to user input.
    Req = arizona_req_test_adapter:new(),
    {ok, Socket} = arizona_socket:init(arizona_root_counter, #{}, Req, #{}),
    Json = iolist_to_binary(json:encode([~"counter", ~"inc", #{}])),
    %% Sanity: inc must return {reply, Data, _} with non-empty Data
    %% (encoded JSON, returned as an iolist by arizona_socket:encode_reply).
    case arizona_socket:handle_in(Json, Socket) of
        {reply, Data, _} ->
            case iolist_size(Data) > 0 of
                true ->
                    ok;
                false ->
                    io:format("error: inc returned empty reply~n"),
                    halt(1)
            end;
        Other ->
            io:format("error: inc returned unexpected ~p~n", [Other]),
            halt(1)
    end,
    Fun = fun() ->
        {reply, _, _} = arizona_socket:handle_in(Json, Socket),
        ok
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
        "~-26s N=~b ops=~b  mean=~s  stdev=~s  p50=~s  p99=~s  ops/s=~s~n",
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

check_timetrap_all(_Results, infinity) ->
    ok;
check_timetrap_all(Results, MaxNs) ->
    Failed = [
        {Label, MeanNs}
     || {Label, #{mean_ns := MeanNs}} <- Results, MeanNs > MaxNs
    ],
    case Failed of
        [] ->
            io:format("~ntimetrap: all workloads <= ~b ns [PASS]~n", [MaxNs]),
            ok;
        _ ->
            lists:foreach(
                fun({Label, MeanNs}) ->
                    io:format(
                        "~ntimetrap: ~s mean=~b ns > ~b ns [FAIL]~n",
                        [Label, trunc(MeanNs), MaxNs]
                    )
                end,
                Failed
            ),
            halt(1)
    end.

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
