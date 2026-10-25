-module(arizona_cowboy_server_SUITE).
-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).

-export([tls_serves_pages_over_http1_and_http2/1]).
-export([tls_alone_means_https/1]).
-export([websocket_opens_over_http2/1]).
-export([tls_config_contradictions_are_refused/1]).
-export([max_content_length_caps_a_body/1]).

all() ->
    [
        tls_serves_pages_over_http1_and_http2,
        tls_alone_means_https,
        websocket_opens_over_http2,
        tls_config_contradictions_are_refused,
        max_content_length_caps_a_body
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(arizona),
    {ok, _} = application:ensure_all_started(gun),
    [{tls, test_tls_opts()} | Config].

end_per_suite(Config) when is_list(Config) ->
    ok = application:stop(arizona).

%% --------------------------------------------------------------------
%% Cases
%% --------------------------------------------------------------------

%% A TLS listener negotiates the protocol by ALPN; a page renders on either.
tls_serves_pages_over_http1_and_http2(Config) ->
    with_listener(#{scheme => https, tls => ?config(tls, Config)}, fun(Port) ->
        [
            begin
                Conn = connect(Port, Protocol),
                ?assertEqual({ok, Protocol}, gun:await_up(Conn, 5000)),
                ?assertMatch({200, <<"<", _/binary>>}, get(Conn, ~"/")),
                ok = gun:close(Conn)
            end
         || Protocol <- [http, http2]
        ],
        ok
    end).

%% The certs alone make the listener HTTPS: never their traffic in cleartext.
tls_alone_means_https(Config) ->
    with_listener(#{tls => ?config(tls, Config)}, fun(Port) ->
        Conn = connect(Port, http2),
        ?assertEqual({ok, http2}, gun:await_up(Conn, 5000)),
        ?assertMatch({200, _}, get(Conn, ~"/")),
        ok = gun:close(Conn)
    end).

%% Extended CONNECT (RFC 8441) is on, so a live session opens over HTTP/2 and
%% receives its connect frame.
websocket_opens_over_http2(Config) ->
    with_listener(#{tls => ?config(tls, Config)}, fun(Port) ->
        {ok, Conn} = gun:open("localhost", Port, #{
            transport => tls,
            tls_opts => [{verify, verify_none}],
            protocols => [http2],
            http2_opts => #{notify_settings_changed => true}
        }),
        {ok, http2} = gun:await_up(Conn, 5000),
        %% The server allows extended CONNECT in its SETTINGS; a client may only
        %% use it once those arrived.
        receive
            {gun_notify, Conn, settings_changed, #{enable_connect_protocol := true}} -> ok
        after 5000 -> ct:fail(no_connect_protocol)
        end,
        Stream = gun:ws_upgrade(Conn, ~"/ws?_az_path=/"),
        receive
            {gun_upgrade, Conn, Stream, [~"websocket"], _Headers} -> ok
        after 5000 ->
            ct:fail({no_upgrade, flush()})
        end,
        receive
            {gun_ws, Conn, Stream, {text, Frame}} ->
                ?assertMatch(#{~"a" := _}, json:decode(Frame))
        after 5000 -> ct:fail(no_connect_frame)
        end,
        ok = gun:close(Conn)
    end).

%% Either half of the TLS pair without the other fails the start, before the
%% listener or its routes exist.
tls_config_contradictions_are_refused(Config) ->
    Routes = routes(),
    Cases = [
        {https_requires_tls, #{scheme => https}},
        {tls_with_http_scheme, #{scheme => http, tls => ?config(tls, Config)}},
        {{bad_scheme, ftp}, #{scheme => ftp}}
    ],
    [
        begin
            ?assertError(
                Reason, arizona_cowboy_server:start(cowboy_tls_test, Opts#{routes => Routes})
            ),
            ?assertEqual({error, not_found}, arizona_cowboy_server:stop(cowboy_tls_test))
        end
     || {Reason, Opts} <- Cases
    ],
    ok.

%% A body past `max_content_length` reads as `too_large`, which the MCP
%% endpoint answers `413`; one at the cap is read.
max_content_length_caps_a_body(Config) when is_list(Config) ->
    with_listener(#{max_content_length => 8}, fun(Port) ->
        Post = fun(Body) ->
            {Status, _Headers, _Body} = arizona_wire_client:request(
                Port, ~"POST", ~"/mcp", [{~"content-type", ~"application/json"}], Body
            ),
            Status
        end,
        ?assertEqual(413, Post(~"123456789")),
        ?assertEqual(200, Post(~"12345678"))
    end).

%% --------------------------------------------------------------------
%% Helpers
%% --------------------------------------------------------------------

routes() ->
    [
        {live, ~"/", arizona_crashable, #{}},
        {ws, ~"/ws", #{}},
        {mcp, ~"/mcp", arizona_mcp_test_server, #{}}
    ].

with_listener(Opts, Fun) ->
    Port = arizona_test_port:pick(),
    {ok, _} = arizona_cowboy_server:start(cowboy_tls_test, Opts#{
        routes => routes(), transport_opts => [{port, Port}]
    }),
    try
        Fun(Port)
    after
        ok = arizona_cowboy_server:stop(cowboy_tls_test)
    end.

connect(Port, Protocol) ->
    {ok, Conn} = gun:open("localhost", Port, #{
        transport => tls,
        tls_opts => [{verify, verify_none}],
        protocols => [Protocol]
    }),
    Conn.

get(Conn, Path) ->
    Stream = gun:get(Conn, Path),
    {response, nofin, Status, _Headers} = gun:await(Conn, Stream, 5000),
    {ok, Body} = gun:await_body(Conn, Stream, 5000),
    {Status, Body}.

%% Self-signed server TLS opts; `pkix_test_data/1` returns in-memory cert/key
%% terms, so there is nothing to write or clean up.
test_tls_opts() ->
    Key = fun() -> public_key:generate_key({rsa, 2048, 65537}) end,
    #{server_config := ServerConf} = public_key:pkix_test_data(#{
        server_chain => #{
            root => [{key, Key()}, {digest, sha256}],
            peer => [{key, Key()}, {digest, sha256}]
        },
        client_chain => #{
            root => [{key, Key()}, {digest, sha256}],
            peer => [{key, Key()}, {digest, sha256}]
        }
    }),
    ServerConf.

flush() ->
    receive
        M -> [M | flush()]
    after 0 -> []
    end.
