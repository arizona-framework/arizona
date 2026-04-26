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
%%%
%%% Workload definitions live in this file (the WHAT). Harness primitives
%%% (timing, stats, reporting, transport clients) live in
%%% `arizona_bench_lib` (the HOW), which is loaded from the test profile.

-mode(compile).

-define(DEFAULT_RUNS, 100).

main(Args) ->
    {Runs, Timetrap} = parse_args(Args),
    ProjectDir = project_dir(),
    ok = setup_code_paths(ProjectDir),
    {ok, _} = application:ensure_all_started(arizona),

    arizona_bench_lib:print_header(Runs, ProjectDir),

    Workloads = [
        {<<"render_view_small">>, fun bench_render_view_small/1},
        {<<"render_view_with_layout">>, fun bench_render_view_with_layout/1},
        {<<"render_view_page">>, fun bench_render_view_page/1},
        {<<"render_each_100">>, fun bench_render_each_100/1},
        {<<"mount_only">>, fun bench_mount_only/1},
        {<<"diff_no_change">>, fun bench_diff_no_change/1},
        {<<"diff_simple_event">>, fun bench_diff_simple_event/1},
        {<<"stream_insert_1k">>, fun bench_stream_insert_1k/1},
        {<<"stream_reorder_100">>, fun bench_stream_reorder_100/1},
        {<<"http_get_e2e">>, fun bench_http_get_e2e/1},
        {<<"http_get_e2e_10c">>, fun bench_http_get_e2e_10c/1},
        {<<"ws_event_e2e">>, fun bench_ws_event_e2e/1},
        {<<"ws_event_e2e_10c">>, fun bench_ws_event_e2e_10c/1}
    ],
    Results = [{Label, Fun(Runs)} || {Label, Fun} <- Workloads],
    [arizona_bench_lib:report(Label, Stats) || {Label, Stats} <- Results],

    check_timetrap_all(Results, Timetrap).

%% ---------------------------------------------------------------------------
%% Workloads
%% ---------------------------------------------------------------------------

bench_render_view_small(Runs) ->
    %% Trivial 2-dynamic view, no layout. Lower bound on render cost.
    %% Note: BEAM does not DCE opaque module calls, so the bench loop
    %% doesn't need iolist_size to force evaluation -- adding it walked
    %% the returned tree and biased the measurement upward by ~10%.
    arizona_bench_lib:run_view_render_workload(arizona_root_counter, #{}, Runs).

bench_render_view_with_layout(Runs) ->
    %% Same as render_view_small but wraps the page in arizona_layout
    %% (DOCTYPE + html/head/body + nav + script tag). Every real HTTP
    %% GET goes through layout application; this catches regressions in
    %% `apply_layouts/3` and the layout's auto-detected `az_nodiff`
    %% semantics that render_view_small bypasses.
    arizona_bench_lib:run_view_render_workload(
        arizona_root_counter,
        #{layouts => [{arizona_layout, render}]},
        Runs
    ).

bench_render_view_page(Runs) ->
    %% Renders arizona_page: a route-level view with three embedded
    %% `arizona_counter` stateful children, plus an empty stream and a
    %% conditional connected-status block. Exercises the multi-child
    %% snapshot path -- each `?stateful(arizona_counter, ...)` allocates
    %% a child view snapshot, runs the child template, and propagates
    %% the result back. Catches regressions in `arizona_template:stateful/2`,
    %% `arizona_render:make_ssr_child_snap/1`, and child-view fingerprint
    %% propagation that the simpler render_view_* workloads bypass.
    arizona_bench_lib:run_view_render_workload(arizona_page, #{}, Runs).

bench_render_each_100(Runs) ->
    %% Renders arizona_about with its `tags` binding overridden to a
    %% 100-element list. The view uses `?each` to render one `<li>` per
    %% tag, exercising `arizona_eval:render_list_items/3` and
    %% `arizona_render:zip_list_fp/2` -- the per-item iteration path
    %% real apps rely on for any non-trivial list rendering.
    Tags = [iolist_to_binary(io_lib:format("tag~b", [I])) || I <- lists:seq(1, 100)],
    arizona_bench_lib:run_view_render_workload(
        arizona_about,
        #{bindings => #{tags => Tags}},
        Runs
    ).

bench_stream_insert_1k(Runs) ->
    %% Per trial: insert 1000 unique items into a fresh empty stream
    %% (direct calls to arizona_stream:insert/2 -- no view, no diff).
    %% Reports the per-insert cost averaged across stream sizes 0..999.
    %%
    %% Designed to catch O(n) regressions in insert/2 (every additional
    %% `Order ++ [Key]` walks the order list). A fix that keeps insert
    %% at O(1) leaves per-op time roughly constant; an O(n) regression
    %% makes per-op time scale with stream size.
    KeyFun = fun(#{id := Id}) -> Id end,
    Items = [#{id => I, text => integer_to_binary(I)} || I <- lists:seq(1, 1000)],
    SampleStream = lists:foldl(
        fun(I, S) -> arizona_stream:insert(S, I) end,
        arizona_stream:new(KeyFun),
        Items
    ),
    case length(arizona_stream:to_list(SampleStream)) of
        1000 ->
            ok;
        Other ->
            io:format("error: expected 1000 stream items, got ~p~n", [Other]),
            halt(1)
    end,
    Trial = fun() ->
        Stream0 = arizona_stream:new(KeyFun),
        T0 = erlang:monotonic_time(nanosecond),
        _ = lists:foldl(fun(I, S) -> arizona_stream:insert(S, I) end, Stream0, Items),
        T1 = erlang:monotonic_time(nanosecond),
        T1 - T0
    end,
    arizona_bench_lib:run_workload_custom(Trial, Runs, 1000).

bench_stream_reorder_100(Runs) ->
    %% Mount arizona_datatable seeded with a 100-row stream, then dispatch
    %% repeated `sort` events on the `id` column. The sort handler flips
    %% asc<->desc each call, producing a full reverse of the row order.
    %% A full reverse is the worst case for LIS reorder: only one item
    %% stays put, the other 99 are emitted as MOVE ops.
    %%
    %% Measures the full event roundtrip including the LIS reorder
    %% computation in `arizona_diff` and stream MOVE op emission.
    Items = [
        #{id => I, name => iolist_to_binary(io_lib:format("name~b", [I])), age => 20 + I rem 50}
     || I <- lists:seq(1, 100)
    ],
    Stream = arizona_stream:new(fun(#{id := Id}) -> Id end, Items),
    arizona_bench_lib:run_socket_event_workload(
        arizona_datatable,
        #{rows => Stream},
        [~"page", ~"sort", #{~"col" => ~"id"}],
        Runs
    ).

bench_http_get_e2e(Runs) ->
    %% End-to-end HTTP GET: cowboy + gen_tcp keep-alive, single client.
    %% Measures full request: cowboy parsing + Arizona HTTP handler +
    %% view mount/render + response writing.
    Routes = [
        {live, <<"/">>, arizona_root_counter, #{layouts => [{arizona_layout, render}]}}
    ],
    arizona_bench_lib:with_cowboy(bench_http_e2e, Routes, fun(Port) ->
        arizona_bench_lib:with_http_socket(Port, fun(Sock) ->
            case arizona_bench_lib:http_get(Sock, Port) of
                {200, Body} when byte_size(Body) > 0 ->
                    ok;
                Other ->
                    io:format("error: GET returned ~p~n", [Other]),
                    halt(1)
            end,
            Fun = fun() ->
                {200, _} = arizona_bench_lib:http_get(Sock, Port),
                ok
            end,
            arizona_bench_lib:run_workload(Fun, Runs)
        end)
    end).

bench_http_get_e2e_10c(Runs) ->
    %% Same as http_get_e2e but with 10 concurrent clients. Each trial
    %% broadcasts `go` to 10 worker procs; each runs 100 GETs on its own
    %% keep-alive socket. Per-op time = wall_clock / 1000.
    Routes = [
        {live, <<"/">>, arizona_root_counter, #{layouts => [{arizona_layout, render}]}}
    ],
    arizona_bench_lib:with_cowboy(bench_http_e2e_10c, Routes, fun(Port) ->
        arizona_bench_lib:run_concurrent_workload(10, 100, Runs, fun(Coordinator) ->
            spawn_link(fun() ->
                arizona_bench_lib:http_client_worker(Coordinator, Port, 100)
            end)
        end)
    end).

bench_ws_event_e2e(Runs) ->
    %% End-to-end WS event: cowboy + gen_tcp WS, single client. Each
    %% iteration sends one `inc` text frame and reads one reply.
    %% Path: cowboy ws + arizona_socket:handle_in + live dispatch +
    %% diff + reply encode.
    Routes = [
        {live, <<"/">>, arizona_root_counter, #{layouts => [{arizona_layout, render}]}},
        {ws, <<"/ws">>, #{}}
    ],
    arizona_bench_lib:with_cowboy(bench_ws_e2e, Routes, fun(Port) ->
        arizona_bench_lib:with_ws_socket(Port, <<"/">>, fun(Sock) ->
            Json = iolist_to_binary(json:encode([~"counter", ~"inc", #{}])),
            ok = arizona_bench_lib:ws_send(Sock, Json),
            case arizona_bench_lib:ws_recv(Sock, 5000) of
                {text, Reply} when byte_size(Reply) > 0 ->
                    ok;
                Other ->
                    io:format("error: WS inc returned ~p~n", [Other]),
                    halt(1)
            end,
            Fun = fun() ->
                ok = arizona_bench_lib:ws_send(Sock, Json),
                {text, _} = arizona_bench_lib:ws_recv(Sock, 5000),
                ok
            end,
            arizona_bench_lib:run_workload(Fun, Runs)
        end)
    end).

bench_ws_event_e2e_10c(Runs) ->
    %% Same as ws_event_e2e but with 10 concurrent clients. Each trial
    %% broadcasts `go` to 10 WS worker procs; each sends 100 `inc` events.
    Routes = [
        {live, <<"/">>, arizona_root_counter, #{layouts => [{arizona_layout, render}]}},
        {ws, <<"/ws">>, #{}}
    ],
    Json = iolist_to_binary(json:encode([~"counter", ~"inc", #{}])),
    arizona_bench_lib:with_cowboy(bench_ws_e2e_10c, Routes, fun(Port) ->
        arizona_bench_lib:run_concurrent_workload(10, 100, Runs, fun(Coordinator) ->
            spawn_link(fun() ->
                arizona_bench_lib:ws_client_worker(Coordinator, Port, Json, 100)
            end)
        end)
    end).

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
            arizona_bench_lib:kill_live(element(2, TestSock));
        Other ->
            io:format("error: init returned unexpected ~p~n", [Other]),
            halt(1)
    end,
    Fun = fun() ->
        {ok, Sock} = arizona_socket:init(arizona_root_counter, #{}, Req, #{}),
        arizona_bench_lib:kill_live(element(2, Sock)),
        ok
    end,
    arizona_bench_lib:run_workload(Fun, Runs).

bench_diff_no_change(Runs) ->
    %% Mount once, then dispatch N `noop` events. `noop` returns the same
    %% bindings, so `compute_changed/2` is empty and `arizona_diff:diff/4`
    %% short-circuits via the dep-skip fast path -- the cheapest event in
    %% the system.
    arizona_bench_lib:run_socket_event_workload(
        arizona_root_counter, #{}, [~"counter", ~"noop", #{}], Runs
    ).

bench_diff_simple_event(Runs) ->
    %% Mount once, then dispatch N `inc` events. Each event changes the
    %% `count` binding, so `compute_changed/2` returns one key and
    %% `arizona_diff:diff/4` emits one OP_TEXT op. Compared against
    %% `diff_no_change`, the delta is the cost of running the diff engine
    %% on a single-binding change.
    arizona_bench_lib:run_socket_event_workload(
        arizona_root_counter, #{}, [~"counter", ~"inc", #{}], Runs
    ).

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
    %% (e.g. arizona_bench_lib, arizona_req_test_adapter, fixtures) are
    %% available. Fall back to default if test profile isn't compiled.
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
            EbinDir = filename:join([LibDir, Lib, "ebin"]),
            case filelib:is_dir(EbinDir) of
                true -> code:add_pathz(EbinDir);
                false -> ok
            end,
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
