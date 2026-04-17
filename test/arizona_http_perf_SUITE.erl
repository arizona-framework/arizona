-module(arizona_http_perf_SUITE).

-export([all/0, groups/0, init_per_suite/1, end_per_suite/1]).
-export([
    ssr_get_1_client/1,
    ssr_get_10_clients/1,
    ws_event_1_client/1,
    ws_event_10_clients/1,
    bench_render_1/1,
    bench_render_10/1,
    bench_event_1/1,
    bench_event_10/1
]).

-define(N_RUNS, 1000).

all() ->
    [{group, tests}].

groups() ->
    [
        {tests, [parallel], [
            ssr_get_1_client,
            ssr_get_10_clients,
            ws_event_1_client,
            ws_event_10_clients,
            bench_render_1,
            bench_render_10,
            bench_event_1,
            bench_event_10
        ]}
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(cowboy),
    Port = 14041,
    Routes = [
        {live, <<"/">>, arizona_counter, #{}},
        {ws, <<"/ws">>, #{}}
    ],
    {ok, _} = arizona_cowboy_server:start(http_perf, #{
        transport_opts => [{port, Port}],
        routes => Routes
    }),
    [{port, Port} | Config].

end_per_suite(_Config) ->
    arizona_cowboy_server:stop(http_perf).

ssr_get_1_client(Config) ->
    log_result("ssr_get_1_client", do_bench_get(1, ?N_RUNS, Config)).

ssr_get_10_clients(Config) ->
    log_result("ssr_get_10_clients", do_bench_get(10, ?N_RUNS, Config)).


ws_event_1_client(Config) ->
    log_result("ws_event_1_client", do_bench_ws(1, ?N_RUNS, Config)).

ws_event_10_clients(Config) ->
    log_result("ws_event_10_clients", do_bench_ws(10, ?N_RUNS, Config)).

bench_render_1(_Config) ->
    log_result("bench_render_1", do_bench_render(1, ?N_RUNS)).

bench_render_10(_Config) ->
    log_result("bench_render_10", do_bench_render(10, ?N_RUNS)).

bench_event_1(_Config) ->
    log_result("bench_event_1", do_bench_event(1, ?N_RUNS)).

bench_event_10(_Config) ->
    log_result("bench_event_10", do_bench_event(10, ?N_RUNS)).

do_bench_get(NClients, NRuns, Config) ->
    Port = proplists:get_value(port, Config),
    Coordinator = self(),
    Pids = [
        spawn_link(fun() -> http_client_loop(Coordinator, Port, NRuns) end)
     || _ <- lists:seq(1, NClients)
    ],
    ok = wait_ready(NClients),
    {TimeUs, _} = timer:tc(fun() ->
        lists:foreach(fun(P) -> P ! go end, Pids),
        collect_done(NClients)
    end),
    {TimeUs, NClients * NRuns}.

do_bench_ws(NClients, NRuns, Config) ->
    Port = proplists:get_value(port, Config),
    Coordinator = self(),
    Pids = [
        spawn_link(fun() -> ws_client_loop(Coordinator, Port, NRuns) end)
     || _ <- lists:seq(1, NClients)
    ],
    ok = wait_ready(NClients),
    {TimeUs, _} = timer:tc(fun() ->
        lists:foreach(fun(P) -> P ! go end, Pids),
        collect_done(NClients)
    end),
    {TimeUs, NClients * NRuns}.

do_bench_render(NClients, NRuns) ->
    Coordinator = self(),
    Pids = [
        spawn_link(fun() -> render_client_loop(Coordinator, NRuns) end)
     || _ <- lists:seq(1, NClients)
    ],
    ok = wait_ready(NClients),
    {TimeUs, _} = timer:tc(fun() ->
        lists:foreach(fun(P) -> P ! go end, Pids),
        collect_done(NClients)
    end),
    {TimeUs, NClients * NRuns}.

do_bench_event(NClients, NRuns) ->
    Coordinator = self(),
    Pids = [
        spawn_link(fun() -> socket_event_client_loop(Coordinator, NRuns) end)
     || _ <- lists:seq(1, NClients)
    ],
    ok = wait_ready(NClients),
    {TimeUs, _} = timer:tc(fun() ->
        lists:foreach(fun(P) -> P ! go end, Pids),
        collect_done(NClients)
    end),
    {TimeUs, NClients * NRuns}.

wait_ready(0) ->
    ok;
wait_ready(N) ->
    receive
        {ready, _} -> wait_ready(N - 1)
    after 30000 ->
        error({perf_client_ready_timeout, N})
    end.

collect_done(0) ->
    ok;
collect_done(N) ->
    receive
        {done, _} -> collect_done(N - 1)
    after 60000 ->
        error({perf_client_done_timeout, N})
    end.


http_client_loop(Coordinator, Port, NRuns) ->
    {ok, Sock} = gen_tcp:connect("localhost", Port, [binary, {active, false}, {packet, raw}]),
    Coordinator ! {ready, self()},
    receive
        go -> ok
    after 30000 ->
        error(perf_client_start_timeout)
    end,
    ok = http_run_loop(Sock, Port, NRuns),
    gen_tcp:close(Sock),
    Coordinator ! {done, self()}.

http_run_loop(_Sock, _Port, 0) ->
    ok;
http_run_loop(Sock, Port, N) ->
    Req = [
        "GET / HTTP/1.1\r\n",
        "Host: localhost:",
        integer_to_list(Port),
        "\r\n",
        "Connection: keep-alive\r\n",
        "\r\n"
    ],
    ok = gen_tcp:send(Sock, Req),
    ok = http_recv_response(Sock),
    http_run_loop(Sock, Port, N - 1).

http_recv_response(Sock) ->
    inet:setopts(Sock, [{packet, http}]),
    Len = http_read_headers(Sock, 0),
    inet:setopts(Sock, [{packet, raw}]),
    {ok, _} = gen_tcp:recv(Sock, Len, 5000),
    ok.

http_read_headers(Sock, Len) ->
    case gen_tcp:recv(Sock, 0, 5000) of
        {ok, {http_response, _, 200, _}} ->
            http_read_headers(Sock, Len);
        {ok, {http_header, _, 'Content-Length', _, V}} ->
            http_read_headers(Sock, binary_to_integer(iolist_to_binary(V)));
        {ok, {http_header, _, _, _, _}} ->
            http_read_headers(Sock, Len);
        {ok, http_eoh} ->
            Len
    end.

ws_client_loop(Coordinator, Port, NRuns) ->
    {ok, Sock} = gen_tcp:connect("localhost", Port, [binary, {active, false}, {packet, raw}]),
    ok = ws_handshake(Sock, Port, <<"/">>),
    Coordinator ! {ready, self()},
    receive
        go -> ok
    after 30000 ->
        error(perf_client_start_timeout)
    end,
    ok = ws_run_loop(Sock, NRuns),
    ws_close(Sock),
    Coordinator ! {done, self()}.

ws_run_loop(_Sock, 0) ->
    ok;
ws_run_loop(Sock, N) ->
    ok = ws_send_json(Sock, [~"counter", ~"inc", #{}]),
    {text, _} = ws_recv(Sock),
    ws_run_loop(Sock, N - 1).


render_client_loop(Coordinator, NRuns) ->
    Coordinator ! {ready, self()},
    receive
        go -> ok
    after 30000 ->
        error(perf_client_start_timeout)
    end,
    ok = render_run_loop(NRuns),
    Coordinator ! {done, self()}.

render_run_loop(0) ->
    ok;
render_run_loop(N) ->
    _IOList = arizona_render:render_to_iolist(arizona_counter, #{bindings => #{}}),
    render_run_loop(N - 1).


socket_event_client_loop(Coordinator, NRuns) ->
    {ok, Sock} = arizona_socket:init(arizona_counter, #{}, #{}),
    Coordinator ! {ready, self()},
    receive
        go -> ok
    after 30000 ->
        error(perf_client_start_timeout)
    end,
    ok = socket_event_run_loop(Sock, NRuns),
    Coordinator ! {done, self()}.

socket_event_run_loop(_Sock, 0) ->
    ok;
socket_event_run_loop(Sock, N) ->
    EventData = iolist_to_binary(json:encode([~"counter", ~"inc", #{}])),
    {reply, _Data, Sock1} = arizona_socket:handle_in(EventData, Sock),
    socket_event_run_loop(Sock1, N - 1).


log_result(Label, {TimeUs, TotalOps}) ->
    Throughput = TotalOps * 1_000_000 div max(TimeUs, 1),
    ct:pal("~s: ~b ops in ~b us (~b ops/s)", [Label, TotalOps, TimeUs, Throughput]),
    io:format(user, "~n[perf] ~s: ~b ops in ~b us (~b ops/s)~n", [
        Label, TotalOps, TimeUs, Throughput
    ]).


ws_handshake(Sock, Port, Path) ->
    Key = base64:encode(crypto:strong_rand_bytes(16)),
    Req = [
        "GET /ws?path=",
        Path,
        " HTTP/1.1\r\n",
        "Host: localhost:",
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
    {ok, RespData} = gen_tcp:recv(Sock, 0, 5000),
    case binary:match(RespData, <<"101 Switching Protocols">>) of
        nomatch -> error({ws_upgrade_failed, RespData});
        _ -> ok
    end.

ws_send_json(Sock, Term) ->
    ws_send(Sock, iolist_to_binary(json:encode(Term))).

ws_send(Sock, Payload) ->
    Frame = ws_encode_text(iolist_to_binary(Payload)),
    gen_tcp:send(Sock, Frame).

ws_recv(Sock) ->
    ws_recv(Sock, 5000).

ws_recv(Sock, Timeout) ->
    case gen_tcp:recv(Sock, 0, Timeout) of
        {ok, Data} -> ws_decode(Data);
        {error, timeout} -> timeout;
        {error, closed} -> {error, closed};
        {error, Reason} -> {error, Reason}
    end.

ws_close(Sock) ->
    Mask = crypto:strong_rand_bytes(4),
    Frame = <<1:1, 0:3, 8:4, 1:1, 0:7, Mask/binary>>,
    ok = gen_tcp:send(Sock, Frame),
    ok = gen_tcp:close(Sock).

ws_encode_text(Payload) ->
    ws_encode_frame(1, Payload).

ws_encode_frame(Opcode, Payload) ->
    Len = byte_size(Payload),
    Mask = crypto:strong_rand_bytes(4),
    Masked = ws_mask(Payload, Mask, 0, <<>>),
    case Len of
        L when L < 126 ->
            <<1:1, 0:3, Opcode:4, 1:1, L:7, Mask/binary, Masked/binary>>;
        L when L < 65536 ->
            <<1:1, 0:3, Opcode:4, 1:1, 126:7, L:16, Mask/binary, Masked/binary>>;
        L ->
            <<1:1, 0:3, Opcode:4, 1:1, 127:7, L:64, Mask/binary, Masked/binary>>
    end.

ws_mask(<<>>, _Mask, _I, Acc) ->
    Acc;
ws_mask(<<B, Rest/binary>>, Mask, I, Acc) ->
    <<_:I/binary, M, _/binary>> = <<Mask/binary, Mask/binary, Mask/binary, Mask/binary>>,
    ws_mask(Rest, Mask, (I + 1) rem 4, <<Acc/binary, (B bxor M)>>).

ws_decode(<<_Fin:1, _Rsv:3, 8:4, _M:1, Len:7, Rest/binary>>) when Len < 126 ->
    <<Code:16, Reason/binary>> =
        case Len of
            0 -> <<0:16>>;
            _ -> binary:part(Rest, 0, min(Len, byte_size(Rest)))
        end,
    {close, Code, Reason};
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
