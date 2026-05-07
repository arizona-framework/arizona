#!/usr/bin/env escript
%%% Performance profiler for Arizona.
%%%
%%% Companion to `scripts/bench.escript`. Whereas bench reports per-op
%%% wall-clock stats (good for catching regressions), profile runs each
%%% workload once under eprof or fprof and dumps a per-MFA breakdown
%%% (good for finding hot paths to optimize).
%%%
%%% Run via `make prof` (or `./scripts/profile.escript`). Like bench,
%%% never wired into ci/precommit -- this is a developer tool.
%%%
%%% Usage: ./profile.escript [--only LABEL ...] [--tool eprof|fprof]
%%%                          [--ops N] [--min-ms F] [--out-dir DIR]
%%%   --only LABEL    restrict to specific workload(s) by label; repeatable.
%%%                   Unknown labels exit 1. Default: all profilable workloads.
%%%   --tool          eprof (default) | fprof. fprof gives a richer call tree
%%%                   (OWN/ACC time, callers) at higher trace cost.
%%%   --ops N         ops per profiling pass (default 1000). One pass, no
%%%                   warmup, no multi-trial averaging -- profile fidelity is
%%%                   what matters, not statistical confidence in timing.
%%%   --min-ms F      hide MFAs whose total time < F ms in the eprof analysis
%%%                   (default 1.0). fprof analysis ignores this.
%%%   --out-dir DIR   directory for profile-<label>.log (default /tmp).

-mode(compile).

-define(DEFAULT_OPS, 1000).
-define(DEFAULT_MIN_MS, 1.0).
-define(DEFAULT_OUT_DIR, "/tmp").

main(Args) ->
    Opts = parse_args(Args),
    ProjectDir = project_dir(),
    ok = setup_code_paths(ProjectDir),
    {ok, _} = application:ensure_all_started(arizona),

    All = profilers(),
    Selected = filter_workloads(All, maps:get(only, Opts, [])),
    lists:foreach(
        fun({Label, Fun}) -> Fun(Label, Opts) end,
        Selected
    ).

profilers() ->
    [
        {~"render_view_page", fun prof_render_view_page/2},
        {~"render_each_100", fun prof_render_each_100/2},
        {~"diff_simple_event", fun prof_diff_simple_event/2},
        {~"stream_reorder_100", fun prof_stream_reorder_100/2}
    ].

filter_workloads(All, []) ->
    All;
filter_workloads(All, Labels) ->
    Known = [L || {L, _} <- All],
    Unknown = [L || L <- Labels, not lists:member(L, Known)],
    case Unknown of
        [] ->
            ok;
        _ ->
            io:format("error: unknown workload(s): ~p~n", [Unknown]),
            io:format("available: ~p~n", [Known]),
            halt(1)
    end,
    [{L, F} || {L, F} <- All, lists:member(L, Labels)].

%% ---------------------------------------------------------------------------
%% Workloads
%% ---------------------------------------------------------------------------

%% Each workload mirrors a `bench.escript` workload but inlines the loop
%% so the profiler captures only the unit of work, not the bench harness's
%% timing/stats machinery.

prof_render_view_page(Label, Opts) ->
    %% Mirrors bench_render_view_page (bench.escript:107). Renders
    %% arizona_page (route-level view with three embedded arizona_counter
    %% stateful children). Exercises the multi-child snapshot path:
    %% arizona_template:stateful, arizona_render:make_ssr_child_snap, and
    %% child-view fingerprint propagation.
    Req = arizona_req_test_adapter:new(),
    Sample = arizona_render:render_view_to_iolist(arizona_page, Req, #{}),
    sanity_render(arizona_page, Sample),
    Op = fun() ->
        arizona_render:render_view_to_iolist(arizona_page, Req, #{})
    end,
    profile_loop(Label, Op, Opts).

prof_render_each_100(Label, Opts) ->
    %% Mirrors bench_render_each_100 (bench.escript:115). Renders
    %% arizona_about with a 100-element `tags` binding -- exercises
    %% arizona_eval:render_list_items, arizona_render:zip_list_fp, and
    %% the per-item iteration path real apps rely on.
    Tags = [iolist_to_binary(io_lib:format("tag~b", [I])) || I <- lists:seq(1, 100)],
    Bindings = #{bindings => #{tags => Tags}},
    Req = arizona_req_test_adapter:new(),
    Sample = arizona_render:render_view_to_iolist(arizona_about, Req, Bindings),
    sanity_render(arizona_about, Sample),
    Op = fun() ->
        arizona_render:render_view_to_iolist(arizona_about, Req, Bindings)
    end,
    profile_loop(Label, Op, Opts).

prof_diff_simple_event(Label, Opts) ->
    %% Mirrors bench_diff_simple_event (bench.escript:489). Mount once,
    %% then loop `inc` events on arizona_root_counter. Each event mutates
    %% the `count` binding, so arizona_diff:diff/4 emits one OP_TEXT op.
    %% Exercises full WS event roundtrip: arizona_socket:handle_in +
    %% handler dispatch + diff + reply encode.
    Req = arizona_req_test_adapter:new(),
    {ok, Socket} = arizona_socket:init(arizona_root_counter, #{}, Req, #{}),
    Json = iolist_to_binary(json:encode([~"counter", ~"inc", #{}])),
    sanity_handle_in(Json, Socket),
    Op = fun() ->
        case arizona_socket:handle_in(Json, Socket) of
            {ok, _} -> ok;
            {reply, _, _} -> ok
        end
    end,
    profile_loop(Label, Op, Opts).

prof_stream_reorder_100(Label, Opts) ->
    %% Mirrors bench_stream_reorder_100 (bench.escript:313). Mount
    %% arizona_datatable seeded with a 100-row stream, then loop sort
    %% events. Each sort flips asc<->desc, producing a full reverse --
    %% the LIS reorder worst case (1 item stays put, 99 emit MOVE ops).
    Items = [
        #{
            id => I,
            name => iolist_to_binary(io_lib:format("name~b", [I])),
            age => 20 + I rem 50
        }
     || I <- lists:seq(1, 100)
    ],
    Stream = arizona_stream:new(fun(#{id := Id}) -> Id end, Items),
    Req = arizona_req_test_adapter:new(),
    {ok, Socket} = arizona_socket:init(
        arizona_datatable, #{rows => Stream}, Req, #{}
    ),
    Json = iolist_to_binary(
        json:encode([~"page", ~"sort", #{~"col" => ~"id"}])
    ),
    sanity_handle_in(Json, Socket),
    Op = fun() ->
        case arizona_socket:handle_in(Json, Socket) of
            {ok, _} -> ok;
            {reply, _, _} -> ok
        end
    end,
    profile_loop(Label, Op, Opts).

sanity_render(Module, Sample) ->
    case iolist_size(Sample) > 0 of
        true ->
            ok;
        false ->
            io:format("error: render_view_to_iolist ~p returned empty~n", [Module]),
            halt(1)
    end.

sanity_handle_in(Json, Socket) ->
    case arizona_socket:handle_in(Json, Socket) of
        {ok, _} ->
            ok;
        {reply, _, _} ->
            ok;
        Other ->
            io:format("error: handle_in returned unexpected ~p~n", [Other]),
            halt(1)
    end.

%% ---------------------------------------------------------------------------
%% Profile harness
%% ---------------------------------------------------------------------------

profile_loop(Label, OpFun, Opts) ->
    Tool = maps:get(tool, Opts, eprof),
    Ops = maps:get(ops, Opts, ?DEFAULT_OPS),
    MinMs = maps:get(min_ms, Opts, ?DEFAULT_MIN_MS),
    OutDir = maps:get(out_dir, Opts, ?DEFAULT_OUT_DIR),
    LabelStr = binary_to_list(Label),
    LogPath = filename:join(OutDir, "arizona_profile_" ++ LabelStr ++ ".log"),
    erlang:garbage_collect(self()),
    case Tool of
        eprof ->
            ok = arizona_profiler:start(),
            run_n(OpFun, Ops),
            ok = arizona_profiler:stop_and_dump(LogPath, MinMs);
        fprof ->
            TracePath = LogPath ++ ".trace",
            ok = arizona_profiler:start_fprof(TracePath),
            run_n(OpFun, Ops),
            ok = arizona_profiler:stop_fprof_and_dump(TracePath, LogPath)
    end,
    print_result(Label, Tool, MinMs, LogPath).

run_n(_Fun, 0) ->
    ok;
run_n(Fun, N) ->
    Fun(),
    run_n(Fun, N - 1).

print_result(Label, eprof, MinMs, Path) ->
    io:format(
        "~nprofile (eprof, ~s, total time, rows >= ~.2f ms) -> ~s~n",
        [Label, MinMs, Path]
    ),
    print_file(Path);
print_result(Label, fprof, _MinMs, Path) ->
    io:format("~nprofile (fprof, ~s, OWN time, sort=own) -> ~s~n", [Label, Path]),
    print_file(Path).

print_file(Path) ->
    case file:read_file(Path) of
        {ok, Bin} ->
            io:put_chars(Bin),
            io:nl();
        {error, Reason} ->
            io:format("  could not read ~s: ~p~n", [Path, Reason])
    end.

%% ---------------------------------------------------------------------------
%% Args + paths
%% ---------------------------------------------------------------------------

parse_args(Args) ->
    parse_args(Args, #{only => []}).

parse_args([], Opts) ->
    Opts#{only => lists:reverse(maps:get(only, Opts))};
parse_args(["--only", Label | Rest], Opts) ->
    Only = [list_to_binary(Label) | maps:get(only, Opts)],
    parse_args(Rest, Opts#{only => Only});
parse_args(["--tool", "eprof" | Rest], Opts) ->
    parse_args(Rest, Opts#{tool => eprof});
parse_args(["--tool", "fprof" | Rest], Opts) ->
    parse_args(Rest, Opts#{tool => fprof});
parse_args(["--tool", Other | _Rest], _Opts) ->
    io:format("error: --tool must be eprof or fprof, got ~s~n", [Other]),
    halt(1);
parse_args(["--ops", NStr | Rest], Opts) ->
    parse_args(Rest, Opts#{ops => list_to_integer(NStr)});
parse_args(["--min-ms", FStr | Rest], Opts) ->
    parse_args(Rest, Opts#{min_ms => list_to_float_lenient(FStr)});
parse_args(["--out-dir", Dir | Rest], Opts) ->
    parse_args(Rest, Opts#{out_dir => Dir});
parse_args([Other | _Rest], _Opts) ->
    io:format("error: unknown arg ~s~n", [Other]),
    halt(1).

list_to_float_lenient(Str) ->
    case string:to_float(Str) of
        {Float, ""} ->
            Float;
        _ ->
            float(list_to_integer(Str))
    end.

setup_code_paths(BaseDir) ->
    %% Prefer the test profile's lib dir so test/support/ modules
    %% (arizona_profiler, arizona_req_test_adapter, fixtures) are
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
