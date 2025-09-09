-module(arizona_static_benchmark).
-moduledoc ~"""
Arizona Static Site Generator Performance Benchmark

Comprehensive benchmarking for the arizona_static module, measuring HTML
generation performance across different parallelism strategies.

## Usage

```erlang
% Quick benchmark with defaults
arizona_static_benchmark:run().

% Custom benchmark
arizona_static_benchmark:run(#{
    route_count => 50,
    timeout => 15000,
    iterations => 5
}).
```

## Benchmark Results

Test Configuration: 100 routes, 30s timeout, 3 iterations

```
=== Arizona Static Site Generator Benchmark ===
Routes: 100 | Timeout: 30000ms | Iterations: 3

=== Benchmark Results ===
Strategy     Routes   HTML Files   Time (ms)    Files/sec
-----------------------------------------------------------------
sequential   100      100          32           3092.78
parallel     100      100          5            20000.00
mixed        100      100          7            14285.71

🏆 Best performing strategy: parallel (20000.00 files/sec)
```

**Performance Analysis:**
- Parallel: **6.5x faster** than sequential (20,000 vs 3,093 files/sec)
- Mixed: **4.6x faster** than sequential (14,286 vs 3,093 files/sec)
- Parallel generation provides significant performance improvements

## Script Usage

```bash
./scripts/benchmark_static.sh [routes] [timeout_ms] [iterations]
```
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([run/0]).
-export([run/1]).
-export([benchmark_sequential/1]).
-export([benchmark_parallel/1]).
-export([benchmark_mixed/1]).

%% --------------------------------------------------------------------
%% Types
%% --------------------------------------------------------------------

-type benchmark_config() :: #{
    route_count => pos_integer(),
    timeout => pos_integer(),
    iterations => pos_integer()
}.

-type benchmark_result() :: #{
    route_count := pos_integer(),
    html_files_generated := pos_integer(),
    total_time_ms := pos_integer(),
    files_per_second := float(),
    strategy := sequential | parallel | mixed
}.

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-spec run() -> ok.
run() ->
    run(#{}).

-spec run(Config) -> ok when
    Config :: benchmark_config().
run(Config) ->
    RouteCount = maps:get(route_count, Config, 100),
    Timeout = maps:get(timeout, Config, 30_000),
    Iterations = maps:get(iterations, Config, 3),

    io:format("~n=== Arizona Static Site Generator Benchmark ===~n"),
    io:format("Routes: ~p | Timeout: ~pms | Iterations: ~p~n~n", [RouteCount, Timeout, Iterations]),

    % Setup test server
    setup_benchmark_server(),

    % Run benchmarks
    SeqResults = run_benchmark_iterations(sequential, RouteCount, Timeout, Iterations),
    ParResults = run_benchmark_iterations(parallel, RouteCount, Timeout, Iterations),
    MixedResults = run_benchmark_iterations(mixed, RouteCount, Timeout, Iterations),

    % Cleanup
    cleanup_benchmark_server(),

    % Display results
    display_benchmark_results([SeqResults, ParResults, MixedResults]),

    ok.

%% --------------------------------------------------------------------
%% Benchmark Implementations
%% --------------------------------------------------------------------

-spec benchmark_sequential(RouteCount) -> benchmark_result() when
    RouteCount :: pos_integer().
benchmark_sequential(RouteCount) ->
    run_benchmark(sequential, RouteCount, 30_000).

-spec benchmark_parallel(RouteCount) -> benchmark_result() when
    RouteCount :: pos_integer().
benchmark_parallel(RouteCount) ->
    run_benchmark(parallel, RouteCount, 30_000).

-spec benchmark_mixed(RouteCount) -> benchmark_result() when
    RouteCount :: pos_integer().
benchmark_mixed(RouteCount) ->
    run_benchmark(mixed, RouteCount, 30_000).

%% --------------------------------------------------------------------
%% Internal Functions
%% --------------------------------------------------------------------

setup_benchmark_server() ->
    % Create mock view module
    {ok, _} = create_benchmark_view_module(),

    % Ensure applications are started
    {ok, _} = application:ensure_all_started(gun),
    ok = application:set_env(arizona, server, #{
        enabled => true,
        % Random port
        transport_opts => [{port, 0}],
        routes => [
            {view, ~"/", arizona_benchmark_view, #{}}
        ]
    }),
    {ok, _} = application:ensure_all_started(arizona),

    ok.

cleanup_benchmark_server() ->
    % Stop apps
    ok = application:stop(gun),
    ok = application:stop(arizona),

    % Clean up mock module
    true = code:purge(arizona_benchmark_view),
    true = code:delete(arizona_benchmark_view),

    ok.

create_benchmark_view_module() ->
    % Create a simple view module for benchmarking
    MockViewCode = merl:qquote(~""""
    -module('@module').
    -compile({parse_transform, arizona_parse_transform}).
    -behaviour(arizona_view).

    -export([mount/2]).
    -export([render/1]).

    mount(_Args, _Req) ->
        arizona_view:new('@module', #{
            id => ~"benchmark_view"
        }, none).

    render(_Bindings) ->
        arizona_template:from_string(~"""
        <!DOCTYPE html>
        <html>
        <head>
            <title>Benchmark Page</title>
        </head>
        <body>
            <h1>Benchmark Content</h1>
            <p>This is a benchmark page for measuring generation performance.</p>
        </body>
        </html>
        """).
    """", [{module, merl:term(arizona_benchmark_view)}]),

    merl:compile_and_load(MockViewCode).

run_benchmark_iterations(Strategy, RouteCount, Timeout, Iterations) ->
    Results = [run_benchmark(Strategy, RouteCount, Timeout) || _ <- lists:seq(1, Iterations)],
    average_results(Results, Strategy).

run_benchmark(Strategy, RouteCount, Timeout) ->
    % Create temporary output directory
    OutputDir = create_temp_output_dir(),

    % Generate route paths based on strategy
    RoutePaths = generate_route_paths(Strategy, RouteCount),

    Config = #{
        route_paths => RoutePaths,
        output_dir => list_to_binary(OutputDir),
        timeout => Timeout
    },

    % Measure generation time
    StartTime = erlang:system_time(millisecond),
    Result = arizona_static:generate(Config),
    EndTime = erlang:system_time(millisecond),

    % Cleanup
    ok = file:del_dir_r(OutputDir),

    case Result of
        ok ->
            TotalTime = EndTime - StartTime,
            HtmlCount = count_html_routes(RoutePaths),
            #{
                route_count => RouteCount,
                html_files_generated => HtmlCount,
                total_time_ms => TotalTime,
                files_per_second =>
                    case TotalTime of
                        % Assume 1ms if too fast to measure
                        0 -> float(HtmlCount * 1000);
                        _ -> (HtmlCount / TotalTime) * 1000.0
                    end,
                strategy => Strategy
            };
        {error, Reason} ->
            error({benchmark_failed, Strategy, Reason})
    end.

generate_route_paths(sequential, RouteCount) ->
    % Generate variations of the root path with query parameters
    #{
        list_to_binary(io_lib:format("/?page=~p", [N])) => #{}
     || N <- lists:seq(1, RouteCount)
    };
generate_route_paths(parallel, RouteCount) ->
    % Generate variations of the root path with query parameters
    #{
        list_to_binary(io_lib:format("/?page=~p", [N])) => #{parallel => true}
     || N <- lists:seq(1, RouteCount)
    };
generate_route_paths(mixed, RouteCount) ->
    % Generate mixed parallel/sequential with query parameters
    maps:from_list([
        case N rem 2 of
            0 -> {list_to_binary(io_lib:format("/?page=~p", [N])), #{parallel => true}};
            1 -> {list_to_binary(io_lib:format("/?page=~p", [N])), #{}}
        end
     || N <- lists:seq(1, RouteCount)
    ]).

count_html_routes(RoutePaths) ->
    % All routes in benchmark generate HTML
    map_size(RoutePaths).

create_temp_output_dir() ->
    TmpBase = "/tmp/arizona_benchmark_" ++ integer_to_list(erlang:system_time()),
    ok = filelib:ensure_dir(filename:join(TmpBase, "dummy")),
    TmpBase.

average_results(Results, Strategy) ->
    Count = length(Results),
    TotalTime = lists:sum([maps:get(total_time_ms, R) || R <- Results]),
    TotalFiles = lists:sum([maps:get(html_files_generated, R) || R <- Results]),
    RouteCount = maps:get(route_count, hd(Results)),

    AvgTime = TotalTime / Count,
    AvgFilesPerSec =
        case TotalTime of
            % Assume 1ms if too fast to measure
            0 -> float(TotalFiles * 1000);
            _ -> (TotalFiles / TotalTime) * 1000.0
        end,

    #{
        route_count => RouteCount,
        html_files_generated => round(TotalFiles / Count),
        total_time_ms => round(AvgTime),
        files_per_second => AvgFilesPerSec,
        strategy => Strategy
    }.

display_benchmark_results(Results) ->
    io:format("~n=== Benchmark Results ===~n"),
    io:format(
        "~-12s ~-8s ~-12s ~-12s ~-15s~n",
        ["Strategy", "Routes", "HTML Files", "Time (ms)", "Files/sec"]
    ),
    io:format("~s~n", [lists:duplicate(65, $-)]),

    lists:foreach(
        fun(Result) ->
            #{
                strategy := Strategy,
                route_count := RouteCount,
                html_files_generated := HtmlFiles,
                total_time_ms := TimeMs,
                files_per_second := FilesPerSec
            } = Result,

            io:format(
                "~-12s ~-8w ~-12w ~-12w ~-15.2f~n",
                [Strategy, RouteCount, HtmlFiles, TimeMs, FilesPerSec]
            )
        end,
        Results
    ),

    io:format("~n"),

    % Find best performing strategy
    SortedResults = lists:sort(
        fun(A, B) ->
            maps:get(files_per_second, A) >= maps:get(files_per_second, B)
        end,
        Results
    ),
    BestResult = hd(SortedResults),

    BestStrategy = maps:get(strategy, BestResult),
    BestRate = maps:get(files_per_second, BestResult),

    io:format("🏆 Best performing strategy: ~p (~.2f files/sec)~n~n", [BestStrategy, BestRate]).
