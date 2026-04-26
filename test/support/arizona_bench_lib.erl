-module(arizona_bench_lib).
-moduledoc """
Performance bench harness primitives.

Used by `scripts/bench.escript`. Holds three groups of helpers:

- **Measurement**: `run_workload/2`, `run_workload_custom/3`, `stats/1`.
  Standard usage runs `Fun()` `?OPS_PER_TRIAL` times per trial; custom
  trials let workloads override the unit of work (e.g., concurrent
  benches that broadcast `go` once and time the round-trip across N
  worker processes).
- **Reporting**: `print_header/2`, `report/2`, `fmt_time/1`, `fmt_int/1`,
  `git_short_sha/1`. Pretty stdout output; JSON variant lives in v5.
- **Transport clients**: HTTP keep-alive (`http_get/2`,
  `http_client_worker/3`) and WebSocket (`ws_connect/3`, `ws_send/2`,
  `ws_recv/2`, `ws_client_worker/4`). Both are hand-rolled gen_tcp
  clients; the WS frame encoder is lifted from
  `arizona_cowboy_ws_SUITE.erl:659-793`. Avoids pulling `gun` (or any
  HTTP client) into the test deps.
- **Concurrency**: `await_ready/1`, `await_done/1` -- spawn-and-signal
  barrier used by the multi-client workloads.

Constants:
- `?WARMUP = 2`: warmup trials discarded before measurement (lets the
  JIT compile, primes the heap).
- `?OPS_PER_TRIAL = 1000`: standard inner-loop count.
""".

-export([
    %% Measurement
    run_workload/2,
    run_workload_custom/3,
    run_concurrent_workload/4,
    run_view_render_workload/3,
    run_socket_event_workload/4,
    stats/1,
    %% Reporting
    print_header/2,
    report/2,
    fmt_time/1,
    fmt_int/1,
    git_short_sha/1,
    %% Process helpers
    kill_live/1,
    await_ready/1,
    await_done/1,
    %% Cowboy server lifecycle
    with_cowboy/3,
    %% HTTP client
    http_get/2,
    http_client_worker/3,
    with_http_socket/2,
    %% WebSocket client
    ws_connect/3,
    ws_send/2,
    ws_recv/2,
    ws_client_worker/4,
    with_ws_socket/3
]).

-define(WARMUP, 2).
-define(OPS_PER_TRIAL, 1000).

%% --------------------------------------------------------------------
%% Measurement
%% --------------------------------------------------------------------

-doc """
Run a workload that times one call to `Fun()` per op, with
`?OPS_PER_TRIAL` ops per trial and `Runs` measured trials (after
2 discarded warmup trials). Returns a stats map.
""".
-spec run_workload(Fun, Runs) -> Stats when
    Fun :: fun(() -> term()),
    Runs :: pos_integer(),
    Stats :: map().
run_workload(Fun, Runs) ->
    lists:foreach(fun(_) -> trial(Fun) end, lists:seq(1, ?WARMUP)),
    PerOpNs = [
        begin
            erlang:garbage_collect(self()),
            TrialNs = trial(Fun),
            TrialNs / ?OPS_PER_TRIAL
        end
     || _ <- lists:seq(1, Runs)
    ],
    Stats = stats(PerOpNs),
    Stats#{ops_per_trial => ?OPS_PER_TRIAL}.

-doc """
Run a workload with a custom trial function. `TrialFun()` runs one
trial worth of work and returns total elapsed nanoseconds. The harness
divides by `OpsPerTrial` to get the per-op time. Use for workloads
where the unit of measurement is a batch (e.g., 1000 stream inserts
as one trial) or where multiple processes coordinate (e.g., 10
concurrent clients each doing 100 ops).
""".
-spec run_workload_custom(TrialFun, Runs, OpsPerTrial) -> Stats when
    TrialFun :: fun(() -> non_neg_integer()),
    Runs :: pos_integer(),
    OpsPerTrial :: pos_integer(),
    Stats :: map().
run_workload_custom(TrialFun, Runs, OpsPerTrial) ->
    lists:foreach(fun(_) -> TrialFun() end, lists:seq(1, ?WARMUP)),
    PerOpNs = [
        begin
            erlang:garbage_collect(self()),
            TrialNs = TrialFun(),
            TrialNs / OpsPerTrial
        end
     || _ <- lists:seq(1, Runs)
    ],
    Stats = stats(PerOpNs),
    Stats#{ops_per_trial => OpsPerTrial}.

-doc """
Run a concurrent workload with N persistent worker processes.

`SpawnFn(Coordinator)` is called once per worker before the timed
trials and must spawn a process that:
  1. Sends `{ready, self()}` to `Coordinator` after setup completes.
  2. Loops: on `go`, runs `OpsPerClient` ops then sends `{done, self()}`;
     on `stop`, exits.

Each trial broadcasts `go` to all workers and waits for all `done`
messages. Per-op time is wall-clock / (NClients * OpsPerClient).
The harness sends `stop` to each worker after measurement (or on
crash via `try/after`) so connections/sockets are cleaned up.
""".
-spec run_concurrent_workload(NClients, OpsPerClient, Runs, SpawnFn) -> Stats when
    NClients :: pos_integer(),
    OpsPerClient :: pos_integer(),
    Runs :: pos_integer(),
    SpawnFn :: fun((pid()) -> pid()),
    Stats :: map().
run_concurrent_workload(NClients, OpsPerClient, Runs, SpawnFn) ->
    OpsPerTrial = NClients * OpsPerClient,
    Coordinator = self(),
    Workers = [SpawnFn(Coordinator) || _ <- lists:seq(1, NClients)],
    ok = await_ready(NClients),
    Trial = fun() ->
        T0 = erlang:monotonic_time(nanosecond),
        lists:foreach(fun(W) -> W ! go end, Workers),
        ok = await_done(NClients),
        T1 = erlang:monotonic_time(nanosecond),
        T1 - T0
    end,
    try
        run_workload_custom(Trial, Runs, OpsPerTrial)
    after
        lists:foreach(fun(W) -> W ! stop end, Workers)
    end.

-doc """
Run a workload that times one SSR render of a view via
`arizona_render:render_view_to_iolist/3`. Uses the canonical test
request from `arizona_req_test_adapter:new/0`. Sanity-checks that
the first render produces non-empty output before timing.
""".
-spec run_view_render_workload(Module, Opts, Runs) -> Stats when
    Module :: module(),
    Opts :: map(),
    Runs :: pos_integer(),
    Stats :: map().
run_view_render_workload(Module, Opts, Runs) ->
    Req = arizona_req_test_adapter:new(),
    Sample = arizona_render:render_view_to_iolist(Module, Req, Opts),
    case iolist_size(Sample) > 0 of
        true ->
            ok;
        false ->
            io:format("error: render_view_to_iolist ~p returned empty~n", [Module]),
            halt(1)
    end,
    Fun = fun() ->
        arizona_render:render_view_to_iolist(Module, Req, Opts)
    end,
    run_workload(Fun, Runs).

-doc """
Run a workload that mounts a view (via `arizona_socket:init/4`),
then times one `arizona_socket:handle_in/2` per op with the given
event term (JSON-encoded once outside the timed region). Reuses the
same Socket across iterations -- valid for events whose handlers
don't mutate the Socket record. The sanity check accepts either
`{ok, _}` (no-change events) or `{reply, _, _}` (events emitting
ops); the inner loop accepts either to keep the harness generic.
""".
-spec run_socket_event_workload(Module, Bindings, Event, Runs) -> Stats when
    Module :: module(),
    Bindings :: map(),
    Event :: term(),
    Runs :: pos_integer(),
    Stats :: map().
run_socket_event_workload(Module, Bindings, Event, Runs) ->
    Req = arizona_req_test_adapter:new(),
    {ok, Socket} = arizona_socket:init(Module, Bindings, Req, #{}),
    Json = iolist_to_binary(json:encode(Event)),
    case arizona_socket:handle_in(Json, Socket) of
        {ok, _} ->
            ok;
        {reply, _, _} ->
            ok;
        Other ->
            io:format("error: handle_in returned unexpected ~p~n", [Other]),
            halt(1)
    end,
    Fun = fun() ->
        case arizona_socket:handle_in(Json, Socket) of
            {ok, _} -> ok;
            {reply, _, _} -> ok
        end
    end,
    run_workload(Fun, Runs).

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

-doc """
Compute stats from per-op nanosecond samples. Returns map with
`mean_ns`, `stdev_ns` (sample stdev, N-1), `p50_ns`, `p99_ns`,
`ops_per_s`, `n_trials`. The `ops_per_trial` field is set by the
caller (`run_workload/2` or `run_workload_custom/3`).

With small N (~10), p99 reflects trial-to-trial variance, not per-op
latency tail. v1 default is N=100 trials, where percentiles are real
sample picks.
""".
-spec stats([number()]) -> map().
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
        mean_ns => Mean,
        stdev_ns => Stdev,
        p50_ns => P50,
        p99_ns => P99,
        ops_per_s => OpsPerSec
    }.

%% --------------------------------------------------------------------
%% Reporting
%% --------------------------------------------------------------------

-doc """
Pretty-print the bench banner and column header. `n_trials` and
`ops_per_trial` are constant across workloads under the standard
harness, so they're shown once here rather than repeated per row.
""".
-spec print_header(pos_integer(), file:filename()) -> ok.
print_header(Runs, ProjectDir) ->
    io:format(
        "~narizona bench v1 | git: ~s | OTP ~s | runs=~b ops/trial=~b~n~n",
        [
            git_short_sha(ProjectDir),
            erlang:system_info(otp_release),
            Runs,
            ?OPS_PER_TRIAL
        ]
    ),
    io:format(
        "~-26s ~8s ~8s ~8s ~8s ~12s~n",
        ["workload", "mean", "stdev", "p50", "p99", "ops/s"]
    ).

-doc "Pretty-print one workload's stats row aligned with the header.".
-spec report(binary(), map()) -> ok.
report(Label, #{
    mean_ns := MeanNs,
    stdev_ns := StdevNs,
    p50_ns := P50Ns,
    p99_ns := P99Ns,
    ops_per_s := OpsPerSec
}) ->
    io:format(
        "~-26s ~s ~s ~s ~s ~s~n",
        [
            Label,
            pad_left(fmt_time(MeanNs), 8),
            pad_left(fmt_time(StdevNs), 8),
            pad_left(fmt_time(P50Ns), 8),
            pad_left(fmt_time(P99Ns), 8),
            pad_left(fmt_int(OpsPerSec), 12)
        ]
    ).

-doc """
Format a duration with a space between number and unit (`130 ns`,
`9.2 µs`). One decimal for microseconds, integer for sub-microsecond.
""".
-spec fmt_time(number()) -> iolist().
fmt_time(Ns) when Ns >= 1000 ->
    io_lib:format("~.1f µs", [Ns / 1000]);
fmt_time(Ns) ->
    io_lib:format("~b ns", [trunc(Ns)]).

-doc "Format an integer with comma thousand separators.".
-spec fmt_int(integer()) -> string().
fmt_int(N) ->
    Reversed = lists:reverse(integer_to_list(N)),
    lists:reverse(group_thousands(Reversed)).

%% Right-align an iolist by padding with spaces to `Width` display
%% characters. Counts via `string:length/1` so multi-byte UTF-8 chars
%% (e.g. µ) consume one column, matching how a terminal renders them.
%% `io:format`'s built-in `~Ns` pads by bytes, which would mis-align
%% rows that include µs values vs ns-only ones.
pad_left(IOData, Width) ->
    Flat = unicode:characters_to_list(IOData),
    Chars = string:length(Flat),
    case Chars >= Width of
        true -> Flat;
        false -> [lists:duplicate(Width - Chars, $\s), Flat]
    end.

group_thousands([A, B, C, D | Rest]) ->
    [A, B, C, $, | group_thousands([D | Rest])];
group_thousands(Rest) ->
    Rest.

-doc """
Best-effort short git SHA for the project directory. Returns
`"unknown"` if not in a git checkout. Uses `git -C <dir>` so the SHA
reflects the project, not whatever cwd we were invoked from.
""".
-spec git_short_sha(file:filename()) -> string().
git_short_sha(ProjectDir) ->
    Cmd = "git -C " ++ ProjectDir ++ " rev-parse --short HEAD 2>/dev/null",
    case os:cmd(Cmd) of
        "" -> "unknown";
        Sha -> string:trim(Sha)
    end.

%% --------------------------------------------------------------------
%% Process helpers
%% --------------------------------------------------------------------

-doc """
Synchronously kill a linked process and wait for it to die. Used by
mount-style benches to keep the system in steady state -- ~1 alive
process at any instant -- so per-iteration measurements don't drift
upward as dying processes accumulate.

Adds ~1 µs of constant overhead to the measurement (monitor + DOWN
receive). The constant doesn't change with mount-path optimizations,
so regressions in the mount itself still surface as proportional
changes.
""".
-spec kill_live(pid()) -> ok.
kill_live(Pid) ->
    Mon = monitor(process, Pid),
    unlink(Pid),
    exit(Pid, kill),
    receive
        {'DOWN', Mon, process, Pid, _} -> ok
    end.

-doc """
Block until N `{ready, _}` messages have been received. Used by
multi-client workloads to wait for all worker processes to finish
their setup before broadcasting `go`.
""".
-spec await_ready(non_neg_integer()) -> ok.
await_ready(0) ->
    ok;
await_ready(N) ->
    receive
        {ready, _} -> await_ready(N - 1)
    after 10000 ->
        error({ready_timeout, N})
    end.

-doc """
Block until N `{done, _}` messages have been received. Used by
multi-client workloads to wait for all worker processes to finish
their per-trial work.
""".
-spec await_done(non_neg_integer()) -> ok.
await_done(0) ->
    ok;
await_done(N) ->
    receive
        {done, _} -> await_done(N - 1)
    after 30000 ->
        error({done_timeout, N})
    end.

%% --------------------------------------------------------------------
%% Cowboy server lifecycle
%% --------------------------------------------------------------------

-doc """
Start a cowboy listener with the given routes, run `Fun(Port)`, then
stop the listener regardless of whether `Fun` returned normally or
crashed. Returns `Fun`'s result.

`Port` is the OS-assigned port (cowboy listens on 0; the actual port
is looked up via `ranch:get_port/1`). `max_keepalive` is set to
infinity because the bench loops easily exceed cowboy's default of
100 requests per connection.
""".
-spec with_cowboy(atom(), list(), fun((inet:port_number()) -> Result)) -> Result when
    Result :: term().
with_cowboy(Name, Routes, Fun) ->
    {ok, _} = application:ensure_all_started(cowboy),
    {ok, _} = arizona_cowboy_server:start(Name, #{
        transport_opts => [{port, 0}],
        proto_opts => #{max_keepalive => infinity},
        routes => Routes
    }),
    Port = ranch:get_port(Name),
    try
        Fun(Port)
    after
        arizona_cowboy_server:stop(Name)
    end.

%% --------------------------------------------------------------------
%% HTTP client (keep-alive, packet=http_bin parsing)
%% --------------------------------------------------------------------

-doc """
Send one HTTP/1.1 GET on an already-open keep-alive socket and read
the response. Returns `{Status, Body}` where `Body` is a binary.
Uses `inet:setopts` to switch packet modes between header parsing
(`http_bin`) and body reading (`raw`).
""".
-spec http_get(gen_tcp:socket(), inet:port_number()) -> {integer(), binary()}.
http_get(Sock, Port) ->
    Req = [
        "GET / HTTP/1.1\r\n",
        "Host: 127.0.0.1:",
        integer_to_list(Port),
        "\r\n",
        "Connection: keep-alive\r\n",
        "\r\n"
    ],
    ok = gen_tcp:send(Sock, Req),
    inet:setopts(Sock, [{packet, http_bin}]),
    {ok, {http_response, _, Status, _}} = gen_tcp:recv(Sock, 0, 5000),
    Len = read_http_headers(Sock, 0),
    inet:setopts(Sock, [{packet, raw}]),
    Body =
        case Len of
            0 ->
                <<>>;
            _ ->
                {ok, B} = gen_tcp:recv(Sock, Len, 5000),
                B
        end,
    {Status, Body}.

read_http_headers(Sock, Len) ->
    case gen_tcp:recv(Sock, 0, 5000) of
        {ok, {http_header, _, 'Content-Length', _, V}} ->
            read_http_headers(Sock, binary_to_integer(V));
        {ok, {http_header, _, _, _, _}} ->
            read_http_headers(Sock, Len);
        {ok, http_eoh} ->
            Len
    end.

-doc """
Persistent HTTP client worker. Connects, signals `{ready, self()}`,
then loops: `go` -> run N requests -> `{done, self()}` -> repeat.
`stop` closes the socket and exits.
""".
-spec http_client_worker(pid(), inet:port_number(), pos_integer()) -> no_return().
http_client_worker(Coordinator, Port, OpsPerCall) ->
    {ok, Sock} = gen_tcp:connect(
        "127.0.0.1", Port, [binary, {active, false}, {packet, raw}]
    ),
    Coordinator ! {ready, self()},
    http_worker_loop(Coordinator, Sock, Port, OpsPerCall).

http_worker_loop(Coordinator, Sock, Port, OpsPerCall) ->
    receive
        go ->
            run_http_loop(Sock, Port, OpsPerCall),
            Coordinator ! {done, self()},
            http_worker_loop(Coordinator, Sock, Port, OpsPerCall);
        stop ->
            gen_tcp:close(Sock)
    end.

run_http_loop(_Sock, _Port, 0) ->
    ok;
run_http_loop(Sock, Port, N) ->
    {200, _} = http_get(Sock, Port),
    run_http_loop(Sock, Port, N - 1).

-doc """
Open a keep-alive HTTP socket to localhost:Port, run `Fun(Sock)`, then
close it -- with `try/after` so the socket is released even if the
workload crashes.
""".
-spec with_http_socket(inet:port_number(), fun((gen_tcp:socket()) -> Result)) -> Result when
    Result :: term().
with_http_socket(Port, Fun) ->
    {ok, Sock} = gen_tcp:connect(
        "127.0.0.1", Port, [binary, {active, false}, {packet, raw}]
    ),
    try
        Fun(Sock)
    after
        gen_tcp:close(Sock)
    end.

%% --------------------------------------------------------------------
%% WebSocket client (gen_tcp, hand-rolled frame codec)
%%
%% Lifted from arizona_cowboy_ws_SUITE.erl:659-793. If the WS suite
%% needs the same helpers, lift them to a shared spot then; for now
%% we accept the duplication.
%% --------------------------------------------------------------------

-doc """
Open a WS connection to `Host:Port` for view path `Path`. Sends the
HTTP/1.1 Upgrade request with `_az_path=<Path>` query string,
expects a 101 response, returns `{ok, Sock}` ready for `ws_send/2`
and `ws_recv/2`.
""".
-spec ws_connect(string(), inet:port_number(), binary()) -> {ok, gen_tcp:socket()}.
ws_connect(Host, Port, Path) ->
    {ok, Sock} = gen_tcp:connect(
        Host, Port, [binary, {active, false}, {packet, http_bin}]
    ),
    ok = ws_handshake(Sock, Port, Path),
    ok = inet:setopts(Sock, [{packet, raw}]),
    {ok, Sock}.

ws_handshake(Sock, Port, Path) ->
    Key = base64:encode(crypto:strong_rand_bytes(16)),
    Req = [
        "GET /ws?_az_path=",
        uri_string:quote(Path),
        " HTTP/1.1\r\n",
        "Host: 127.0.0.1:",
        integer_to_list(Port),
        "\r\n",
        "Upgrade: websocket\r\n",
        "Connection: Upgrade\r\n",
        "Sec-WebSocket-Key: ",
        Key,
        "\r\n",
        "Sec-WebSocket-Version: 13\r\n",
        "\r\n"
    ],
    ok = gen_tcp:send(Sock, Req),
    {ok, {http_response, _, 101, _}} = gen_tcp:recv(Sock, 0, 5000),
    drain_ws_headers(Sock).

drain_ws_headers(Sock) ->
    case gen_tcp:recv(Sock, 0, 5000) of
        {ok, http_eoh} -> ok;
        {ok, {http_header, _, _, _, _}} -> drain_ws_headers(Sock)
    end.

-doc "Send a text frame (opcode 1) with masking (clients must mask).".
-spec ws_send(gen_tcp:socket(), iodata()) -> ok.
ws_send(Sock, Payload) ->
    Frame = ws_encode_text(iolist_to_binary(Payload)),
    gen_tcp:send(Sock, Frame).

-doc """
Receive and decode one server frame. Returns `{text|binary|unknown,
Payload}` or `{raw, Data}` for unrecognized framing.
""".
-spec ws_recv(gen_tcp:socket(), timeout()) -> {atom(), binary()} | {error, term()}.
ws_recv(Sock, Timeout) ->
    case gen_tcp:recv(Sock, 0, Timeout) of
        {ok, Data} -> ws_decode(Data);
        {error, Reason} -> {error, Reason}
    end.

ws_encode_text(Payload) ->
    Len = byte_size(Payload),
    Mask = crypto:strong_rand_bytes(4),
    Masked = ws_mask(Payload, Mask, 0, <<>>),
    case Len of
        L when L < 126 ->
            <<1:1, 0:3, 1:4, 1:1, L:7, Mask/binary, Masked/binary>>;
        L when L < 65536 ->
            <<1:1, 0:3, 1:4, 1:1, 126:7, L:16, Mask/binary, Masked/binary>>;
        L ->
            <<1:1, 0:3, 1:4, 1:1, 127:7, L:64, Mask/binary, Masked/binary>>
    end.

ws_mask(<<>>, _Mask, _I, Acc) ->
    Acc;
ws_mask(<<B, Rest/binary>>, Mask, I, Acc) ->
    <<_:I/binary, M, _/binary>> = <<Mask/binary, Mask/binary, Mask/binary, Mask/binary>>,
    ws_mask(Rest, Mask, (I + 1) rem 4, <<Acc/binary, (B bxor M)>>).

ws_decode(<<_Fin:1, _Rsv:3, Opcode:4, 0:1, Len:7, Rest/binary>>) when Len < 126 ->
    Payload = binary:part(Rest, 0, Len),
    {ws_opcode_to_type(Opcode), Payload};
ws_decode(<<_Fin:1, _Rsv:3, Opcode:4, 0:1, 126:7, Len:16, Rest/binary>>) ->
    Payload = binary:part(Rest, 0, Len),
    {ws_opcode_to_type(Opcode), Payload};
ws_decode(Data) ->
    {raw, Data}.

ws_opcode_to_type(1) -> text;
ws_opcode_to_type(2) -> binary;
ws_opcode_to_type(_) -> unknown.

-doc """
Persistent WS client worker. Connects, signals `{ready, self()}`,
then loops: `go` -> send N events from `Json` and recv N replies ->
`{done, self()}` -> repeat. `stop` closes the socket and exits.
""".
-spec ws_client_worker(pid(), inet:port_number(), binary(), pos_integer()) -> no_return().
ws_client_worker(Coordinator, Port, Json, OpsPerCall) ->
    {ok, Sock} = ws_connect("127.0.0.1", Port, <<"/">>),
    Coordinator ! {ready, self()},
    ws_worker_loop(Coordinator, Sock, Json, OpsPerCall).

ws_worker_loop(Coordinator, Sock, Json, OpsPerCall) ->
    receive
        go ->
            run_ws_loop(Sock, Json, OpsPerCall),
            Coordinator ! {done, self()},
            ws_worker_loop(Coordinator, Sock, Json, OpsPerCall);
        stop ->
            gen_tcp:close(Sock)
    end.

run_ws_loop(_Sock, _Json, 0) ->
    ok;
run_ws_loop(Sock, Json, N) ->
    ok = ws_send(Sock, Json),
    {text, _} = ws_recv(Sock, 5000),
    run_ws_loop(Sock, Json, N - 1).

-doc """
Open a WS connection to localhost:Port for view `Path`, run `Fun(Sock)`,
then close it -- with `try/after` so the socket is released even if
the workload crashes.
""".
-spec with_ws_socket(inet:port_number(), binary(), fun((gen_tcp:socket()) -> Result)) ->
    Result
when
    Result :: term().
with_ws_socket(Port, Path, Fun) ->
    {ok, Sock} = ws_connect("127.0.0.1", Port, Path),
    try
        Fun(Sock)
    after
        gen_tcp:close(Sock)
    end.
