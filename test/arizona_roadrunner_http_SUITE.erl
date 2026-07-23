-module(arizona_roadrunner_http_SUITE).
-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).

-export([handle_returns_the_threaded_raw_request/1]).
-export([handle_returns_the_threaded_raw_request_on_a_render_crash/1]).
-export([manual_body_read_keeps_pipelining_intact/1]).

all() ->
    [
        handle_returns_the_threaded_raw_request,
        handle_returns_the_threaded_raw_request_on_a_render_crash,
        manual_body_read_keeps_pipelining_intact
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(arizona),
    {ok, _} = application:ensure_all_started(roadrunner),
    Listener = arizona_roadrunner_http_test,
    Port = pick_port(),
    Routes = [
        {live, ~"/echo_body", arizona_about, #{
            middlewares => [arizona_middleware:extract([body])]
        }}
    ],
    {ok, _} = arizona_roadrunner_server:start(Listener, #{
        transport_opts => [{port, Port}],
        %% The whole point: in manual mode the conn does not pre-read the body,
        %% so reading it inside a middleware mutates the raw request.
        proto_opts => #{body_buffering => manual},
        routes => Routes
    }),
    [{port, Port}, {listener, Listener} | Config].

end_per_suite(Config) ->
    ok = arizona_roadrunner_server:stop(?config(listener, Config)),
    ok.

pick_port() ->
    15040 + erlang:unique_integer([positive, monotonic]) rem 1000.

%% --------------------------------------------------------------------
%% Request threading
%% --------------------------------------------------------------------

%% A middleware that consumes the body advances the raw request's body_reader.
%% `handle/1` must hand roadrunner back THAT request, not the one it was called
%% with -- roadrunner's finishing phase drains the reader off the returned req.
handle_returns_the_threaded_raw_request(Config) when is_list(Config) ->
    Body = ~"body-bytes",
    Req = manual_body_req(Body, arizona_about, #{}),
    {{Status, _Headers, _RespBody}, ReqOut} = arizona_roadrunner_http:handle(Req),
    ?assertEqual(200, Status),
    ?assertMatch(
        #{body_reader := #{bytes_read := Read}} when Read =:= byte_size(Body),
        ReqOut
    ).

%% Same threading on the error arm: a render crash still has to return the
%% advanced request, or the 500 leaves the connection's reader stale.
handle_returns_the_threaded_raw_request_on_a_render_crash(Config) when is_list(Config) ->
    Body = ~"body-bytes",
    Req = manual_body_req(Body, arizona_crashable, #{crash_on_mount => true}),
    {{Status, _Headers, _RespBody}, ReqOut} = arizona_roadrunner_http:handle(Req),
    ?assertEqual(500, Status),
    ?assertMatch(
        #{body_reader := #{bytes_read := Read}} when Read =:= byte_size(Body),
        ReqOut
    ).

%% --------------------------------------------------------------------
%% Keep-alive framing
%% --------------------------------------------------------------------

%% End-to-end consequence of dropping the threaded request: the stale
%% body_reader still believes the body is unread, so roadrunner's drain pulls
%% `content-length` bytes off the wire -- which are the *next* pipelined
%% request's bytes. The follow-up request then fails to parse (400 + close).
manual_body_read_keeps_pipelining_intact(Config) ->
    Port = ?config(port, Config),
    {ok, Sock} = gen_tcp:connect(
        "127.0.0.1", Port, [binary, {active, false}, {packet, raw}]
    ),
    try
        Body = ~"hello-body-0123456789",
        Head = [
            "GET /echo_body HTTP/1.1\r\nHost: localhost\r\nContent-Length: ",
            integer_to_binary(byte_size(Body)),
            "\r\n\r\n"
        ],
        %% Send the headers alone so the manual body_reader is built with an
        %% empty buffer -- the body then has to come off the socket, which is
        %% what makes a re-read observable.
        ok = gen_tcp:send(Sock, Head),
        ok = timer:sleep(150),
        ok = gen_tcp:send(Sock, Body),
        ok = timer:sleep(150),
        %% Pipelined follow-up on the same keep-alive connection.
        ok = gen_tcp:send(Sock, ~"GET /echo_body HTTP/1.1\r\nHost: localhost\r\n\r\n"),
        Data = recv_until_quiet(Sock, <<>>),
        ?assertEqual([~"HTTP/1.1 200 OK", ~"HTTP/1.1 200 OK"], status_lines(Data))
    after
        ok = gen_tcp:close(Sock)
    end.

%% --------------------------------------------------------------------
%% Helpers
%% --------------------------------------------------------------------

%% A hand-built roadrunner request in manual body mode: the body sits in the
%% reader's buffer, so the body-reading middleware needs no socket.
manual_body_req(Body, Handler, Bindings) ->
    #{
        method => ~"GET",
        path => ~"/echo_body",
        target => ~"/echo_body",
        version => {1, 1},
        headers => [{~"host", ~"localhost"}],
        bindings => #{},
        body_reader => #{
            framing => {content_length, byte_size(Body)},
            buffered => Body,
            bytes_read => 0,
            pending => <<>>,
            done => false,
            recv => fun() -> {error, closed} end,
            max => 1024,
            trailer_limits => {8192, 10240, 100}
        },
        state => #{
            arizona => #{
                handler => Handler,
                bindings => Bindings,
                middlewares => [arizona_middleware:extract([body])]
            }
        }
    }.

%% The status line of every response found in `Data`.
status_lines(Data) ->
    [
        hd(binary:split(binary:part(Data, Pos, min(64, byte_size(Data) - Pos)), ~"\r\n"))
     || {Pos, _Len} <- binary:matches(Data, ~"HTTP/1.1 ")
    ].

%% Read until the peer goes quiet for 500 ms (or closes). Both responses are on
%% the wire well before that; a stale-reader server sends one 200 then a 400.
recv_until_quiet(Sock, Acc) ->
    case gen_tcp:recv(Sock, 0, 500) of
        {ok, Data} -> recv_until_quiet(Sock, <<Acc/binary, Data/binary>>);
        {error, _} -> Acc
    end.
