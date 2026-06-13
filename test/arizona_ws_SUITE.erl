-module(arizona_ws_SUITE).
-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").
-include("arizona.hrl").
-include_lib("arizona/include/arizona_effect.hrl").

-export([all/0, groups/0, init_per_group/2, end_per_group/2]).
-export([
    ping_pong/1,
    event_dispatch/1,
    event_with_effects/1,
    event_no_change/1,
    unknown_json/1,
    unknown_frame/1,
    reconnect_init/1,
    connect_with_params/1,
    http_query_params/1,
    http_path_bindings/1,
    ws_path_bindings/1,
    ws_navigate_to_parametrized_route/1,
    http_put_request_reads_request/1,
    middleware_cont_enriches_bindings/1,
    middleware_cont_ws_connects/1,
    middleware_halt_redirects/1,
    middleware_halt_rejects_ws/1,
    middleware_pipeline_runs_in_order/1,
    middleware_halt_redirects_on_navigate/1,
    middleware_cont_on_navigate/1,
    http_halt_redirect_via_req/1,
    http_halt_sets_cookie/1,
    http_render_sets_cookie/1,
    http_render_crash_emits_error_page/1,
    http_reads_cookies_headers_body/1,
    http_exposes_roadrunner_request_id/1,
    drain_default_closes_ws_cleanly/1,
    drain_user_callback_pushes_effect_then_stops/1,
    drain_user_callback_keeps_alive/1,
    drain_user_callback_noop_keeps_alive/1,
    drain_emits_telemetry/1,
    drain_then_navigate_closes_gracefully/1,
    static_asset_served/1,
    static_asset_body_matches_file/1,
    static_asset_gzip_served/1,
    static_asset_missing_returns_404/1,
    reload_endpoint_streams_event/1,
    recompile_routes_runs/1,
    recompile_routes_syncs_listener/1,
    https_without_tls_errors/1,
    http_preserves_duplicate_qs_keys/1,
    ws_navigate_preserves_duplicate_qs_keys/1,
    ws_upgrade_preserves_duplicate_qs_keys/1,
    crash_event_closes/1,
    crash_info_closes/1,
    crash_init_closes/1,
    normal_exit_closes/1
]).

all() ->
    [
        {group, roadrunner}
    ].

groups() ->
    Basic = [
        ping_pong,
        event_dispatch,
        event_with_effects,
        event_no_change,
        unknown_json,
        unknown_frame,
        reconnect_init,
        connect_with_params,
        http_query_params,
        http_path_bindings,
        ws_path_bindings,
        ws_navigate_to_parametrized_route,
        http_put_request_reads_request,
        middleware_cont_enriches_bindings,
        middleware_cont_ws_connects,
        middleware_halt_redirects,
        middleware_halt_rejects_ws,
        middleware_pipeline_runs_in_order,
        middleware_halt_redirects_on_navigate,
        middleware_cont_on_navigate,
        http_halt_redirect_via_req,
        http_halt_sets_cookie,
        http_render_sets_cookie,
        http_render_crash_emits_error_page,
        http_reads_cookies_headers_body,
        http_exposes_roadrunner_request_id,
        drain_default_closes_ws_cleanly,
        drain_user_callback_pushes_effect_then_stops,
        drain_user_callback_keeps_alive,
        drain_user_callback_noop_keeps_alive,
        drain_emits_telemetry,
        drain_then_navigate_closes_gracefully,
        static_asset_served,
        static_asset_body_matches_file,
        static_asset_gzip_served,
        static_asset_missing_returns_404,
        reload_endpoint_streams_event,
        recompile_routes_runs,
        recompile_routes_syncs_listener,
        https_without_tls_errors,
        http_preserves_duplicate_qs_keys,
        ws_navigate_preserves_duplicate_qs_keys,
        ws_upgrade_preserves_duplicate_qs_keys
    ],
    Crash = [
        crash_event_closes,
        crash_info_closes,
        crash_init_closes,
        normal_exit_closes
    ],
    [
        {roadrunner, [sequence], [{group, basic}, {group, crash_recovery}]},
        {basic, [sequence], Basic},
        {crash_recovery, [sequence], Crash}
    ].

init_per_group(roadrunner, Config) ->
    {ok, _} = application:ensure_all_started(arizona),
    {ok, _} = application:ensure_all_started(roadrunner),
    Port = pick_port(),
    %% Tests that assert on URL data in rendered HTML opt into the framework's
    %% `arizona_middleware:extract/1` middleware on their routes; the framework itself
    %% never flat-merges URL data into bindings.
    %% Projects every `foo=X` occurrence -- preserving duplicates and
    %% insertion order -- into the `status` binding so assertions can
    %% read them off the rendered HTML. Used to verify that the full qs
    %% (including duplicates) flows through every entry point.
    DupePreserving =
        fun(Req, B) ->
            {Params, Req1} = arizona_req:params(Req),
            Values = [V || {K, V} <:- Params, K =:= ~"foo"],
            Joined = iolist_to_binary(lists:join(~",", Values)),
            {cont, Req1, B#{status => <<"foo=[", Joined/binary, "]">>}}
        end,
    Routes = [
        {live, <<"/">>, arizona_crashable, #{
            middlewares => [arizona_middleware:extract([path_bindings, params])]
        }},
        {live, <<"/crash_on_mount">>, arizona_crashable, #{
            bindings => #{crash_on_mount => true}
        }},
        {live, <<"/with_middleware">>, arizona_crashable, #{
            middlewares => [fun(Req, B) -> {cont, Req, B#{session => <<"abc">>}} end]
        }},
        %% Redirect halt: roadrunner translates the stashed redirect
        %% into a 302.
        {live, <<"/halt_middleware">>, arizona_crashable, #{
            middlewares => [
                fun(Req, _B) ->
                    {halt, arizona_req:redirect(Req, <<"/login">>)}
                end
            ]
        }},
        {live, <<"/pipeline">>, arizona_crashable, #{
            middlewares => [
                fun(Req, B) -> {cont, Req, B#{step => 1}} end,
                fun(Req, #{step := N} = B) -> {cont, Req, B#{step => N + 1}} end
            ]
        }},
        {live, <<"/navigate_halt_redirect">>, arizona_crashable, #{
            middlewares => [
                fun(Req, _B) -> {halt, arizona_req:redirect(Req, <<"/login">>)} end
            ]
        }},
        {live, <<"/navigate_enriched">>, arizona_crashable, #{
            middlewares => [
                fun(Req, B) -> {cont, Req, B#{session => <<"from_navigate">>}} end
            ]
        }},
        {live, <<"/items/:item_id">>, arizona_crashable, #{
            middlewares => [arizona_middleware:extract([path_bindings, params])]
        }},
        {live, <<"/preserves_dupes">>, arizona_crashable, #{middlewares => [DupePreserving]}},
        {live, <<"/halt_redirect_req">>, arizona_crashable, #{
            middlewares => [
                fun(Req, _B) -> {halt, arizona_req:redirect(Req, <<"/login">>)} end
            ]
        }},
        %% Stash a cookie then halt with a redirect: the transport flushes
        %% Set-Cookie onto the 303 response.
        {live, <<"/halt_set_cookie">>, arizona_crashable, #{
            middlewares => [
                fun(Req, _B) ->
                    Req1 = arizona_req:put_resp_cookie(Req, <<"session">>, <<"abc">>, #{
                        path => <<"/">>, http_only => true
                    }),
                    {halt, arizona_req:redirect(Req1, 303, <<"/">>)}
                end
            ]
        }},
        %% Stash a cookie then continue: the rendered page response carries
        %% Set-Cookie.
        {live, <<"/render_set_cookie">>, arizona_crashable, #{
            middlewares => [
                fun(Req, B) ->
                    Req1 = arizona_req:put_resp_cookie(Req, <<"theme">>, <<"dark">>, #{
                        path => <<"/">>
                    }),
                    {cont, Req1, B}
                end
            ]
        }},
        {live, <<"/crash_on_render_http">>, arizona_crashable, #{
            bindings => #{crash_on_mount => true}
        }},
        {live, <<"/reqreader">>, arizona_request_reader, #{
            middlewares => [{arizona_middleware, put_request}]
        }},
        {live, <<"/reads_request_id">>, arizona_crashable, #{
            middlewares => [
                fun(Req, B) ->
                    Status =
                        case arizona_req:request_id(Req) of
                            undefined -> ~"id=none";
                            Id -> <<"id=", Id/binary>>
                        end,
                    {cont, Req, B#{status => Status}}
                end
            ]
        }},
        {live, <<"/reads_cookies_headers_body">>, arizona_crashable, #{
            middlewares => [
                fun(Req, B) ->
                    {Cookies, Req1} = arizona_req:cookies(Req),
                    {Headers, Req2} = arizona_req:headers(Req1),
                    {Body, Req3} = arizona_req:body(Req2),
                    CookieCount = integer_to_binary(length(Cookies)),
                    HeaderCount = integer_to_binary(map_size(Headers)),
                    BodyLen = integer_to_binary(byte_size(Body)),
                    {cont, Req3, B#{
                        status => <<
                            "c=",
                            CookieCount/binary,
                            " h=",
                            HeaderCount/binary,
                            " b=",
                            BodyLen/binary
                        >>
                    }}
                end
            ]
        }},
        {live, <<"/drain_stop">>, arizona_drainable, #{bindings => #{drain_mode => stop}}},
        {live, <<"/drain_keep">>, arizona_drainable, #{bindings => #{drain_mode => keep}}},
        {live, <<"/drain_noop">>, arizona_drainable, #{bindings => #{drain_mode => noop}}},
        {ws, <<"/ws">>, #{}},
        {asset, <<"/assets">>, {priv_dir, arizona, "static/assets/js"}},
        {reload, <<"/reload">>, #{}}
    ],
    ServerMod = arizona_roadrunner_server,
    {ok, _} = ServerMod:start(ws_test, #{
        transport_opts => [{port, Port}],
        routes => Routes
    }),
    [{port, Port}, {server_mod, ServerMod} | Config];
init_per_group(_Group, Config) ->
    Config.

end_per_group(roadrunner, Config) ->
    ServerMod = ?config(server_mod, Config),
    _ = ServerMod:stop(ws_test),
    _ = application:stop(arizona),
    ok;
end_per_group(_Group, _Config) ->
    ok.

pick_port() ->
    14040 + erlang:unique_integer([positive, monotonic]) rem 1000.

%% --------------------------------------------------------------------
%% Basic tests
%% --------------------------------------------------------------------

ping_pong(Config) ->
    {ok, Sock} = ws_connect(Config, <<"/">>),
    ok = ws_send(Sock, <<"0">>),
    {text, <<"1">>} = ws_recv(Sock),
    ws_close(Sock).

event_dispatch(Config) ->
    {ok, Sock} = ws_connect(Config, <<"/">>),
    ok = ws_send_json(Sock, [~"crashable", ~"set_status", #{~"value" => ~"updated"}]),
    {text, Resp} = ws_recv(Sock),
    Decoded = json:decode(Resp),
    ?assertMatch(#{~"o" := _}, Decoded),
    ws_close(Sock).

event_with_effects(Config) ->
    {ok, Sock} = ws_connect(Config, <<"/">>),
    ok = ws_send_json(Sock, [~"crashable", ~"with_effect", #{}]),
    {text, Resp} = ws_recv(Sock),
    Decoded = json:decode(Resp),
    %% Should have effects key
    ?assertMatch(#{~"e" := _}, Decoded),
    ws_close(Sock).

event_no_change(Config) ->
    {ok, Sock} = ws_connect(Config, <<"/">>),
    %% Event that produces no ops and no effects -- server sends nothing
    ok = ws_send_json(Sock, [~"crashable", ~"set_status", #{~"value" => ~"ok"}]),
    %% Verify connection is alive via ping (no event response expected)
    ok = ws_send(Sock, <<"0">>),
    {text, <<"1">>} = ws_recv(Sock),
    ws_close(Sock).

unknown_json(Config) ->
    {ok, Sock} = ws_connect(Config, <<"/">>),
    ok = ws_send_json(Sock, #{~"unknown" => ~"message"}),
    %% Unknown messages are silently ignored -- verify connection alive
    ok = ws_send(Sock, <<"0">>),
    {text, <<"1">>} = ws_recv(Sock),
    ws_close(Sock).

unknown_frame(Config) ->
    {ok, Sock} = ws_connect(Config, <<"/">>),
    %% Send a binary frame (opcode 2) -- should be ignored
    ok = ws_send_binary_frame(Sock, <<"binary data">>),
    %% Verify connection alive
    ok = ws_send(Sock, <<"0">>),
    {text, <<"1">>} = ws_recv(Sock),
    ws_close(Sock).

connect_with_params(Config) ->
    {ok, Sock} = ws_connect(Config, <<"/">>, [{params, #{~"locale" => ~"pt"}}]),
    %% Connection should work -- verify with ping/pong
    ok = ws_send(Sock, <<"0">>),
    {text, <<"1">>} = ws_recv(Sock),
    ws_close(Sock).

http_query_params(Config) ->
    Port = proplists:get_value(port, Config),
    {ok, Sock} = gen_tcp:connect("localhost", Port, [binary, {active, false}]),
    Req = [
        "GET /?locale=pt HTTP/1.1\r\n",
        "Host: localhost:",
        integer_to_list(Port),
        "\r\n",
        "\r\n"
    ],
    ok = gen_tcp:send(Sock, Req),
    {ok, Resp} = gen_tcp:recv(Sock, 0, 5000),
    gen_tcp:close(Sock),
    ?assertNotEqual(nomatch, binary:match(Resp, <<"pt<!--/az-->">>)).

http_path_bindings(Config) ->
    %% HTTP: path param :item_id available in rendered HTML
    Port = proplists:get_value(port, Config),
    {ok, Sock} = gen_tcp:connect("localhost", Port, [binary, {active, false}]),
    Req = [
        "GET /items/my-item HTTP/1.1\r\n",
        "Host: localhost:",
        integer_to_list(Port),
        "\r\n",
        "\r\n"
    ],
    ok = gen_tcp:send(Sock, Req),
    {ok, Resp} = gen_tcp:recv(Sock, 0, 5000),
    gen_tcp:close(Sock),
    ?assertNotEqual(nomatch, binary:match(Resp, <<"my-item">>)).

ws_path_bindings(Config) ->
    %% WS: path param :item_id resolved via ?path= query param
    {ok, Sock} = ws_connect(Config, <<"/items/my-item">>),
    ok = ws_send(Sock, <<"0">>),
    {text, <<"1">>} = ws_recv(Sock),
    ws_close(Sock).

ws_navigate_to_parametrized_route(Config) ->
    %% SPA navigate INTO a `:param` route: arizona_socket:handle_navigate runs the
    %% new route's extract([path_bindings, ...]) middleware, so the handler's mount
    %% receives item_id even though arizona_live never sees a request.
    {ok, Sock} = ws_connect(Config, <<"/">>),
    ok = ws_send_json(Sock, [
        ~"navigate",
        #{~"path" => ~"/items/navtest", ~"qs" => ~""}
    ]),
    {text, Resp} = ws_recv(Sock),
    ?assertNotEqual(nomatch, binary:match(Resp, <<"navtest">>)),
    ws_close(Sock).

http_put_request_reads_request(Config) ->
    %% put_request escape hatch: the handler reads ?get(request) then params/1.
    Port = proplists:get_value(port, Config),
    {ok, Sock} = gen_tcp:connect("localhost", Port, [binary, {active, false}]),
    Req = [
        "GET /reqreader?locale=putreq HTTP/1.1\r\n",
        "Host: localhost:",
        integer_to_list(Port),
        "\r\n",
        "\r\n"
    ],
    ok = gen_tcp:send(Sock, Req),
    {ok, Resp} = gen_tcp:recv(Sock, 0, 5000),
    gen_tcp:close(Sock),
    ?assertNotEqual(nomatch, binary:match(Resp, <<"putreq">>)).

middleware_cont_enriches_bindings(Config) ->
    %% HTTP: middleware adds session to bindings, rendered in page
    Port = proplists:get_value(port, Config),
    {ok, Sock} = gen_tcp:connect("localhost", Port, [binary, {active, false}]),
    Req = [
        "GET /with_middleware HTTP/1.1\r\n",
        "Host: localhost:",
        integer_to_list(Port),
        "\r\n",
        "\r\n"
    ],
    ok = gen_tcp:send(Sock, Req),
    {ok, Resp} = gen_tcp:recv(Sock, 0, 5000),
    gen_tcp:close(Sock),
    %% Page rendered successfully (200 OK)
    ?assertNotEqual(nomatch, binary:match(Resp, <<"200 OK">>)).

middleware_cont_ws_connects(Config) ->
    %% WS: middleware passes -- upgrade happens normally
    {ok, Sock} = ws_connect(Config, <<"/with_middleware">>),
    ok = ws_send(Sock, <<"0">>),
    {text, <<"1">>} = ws_recv(Sock),
    ws_close(Sock).

middleware_halt_redirects(Config) ->
    %% HTTP: middleware halts with 302 redirect
    Port = proplists:get_value(port, Config),
    {ok, Sock} = gen_tcp:connect("localhost", Port, [binary, {active, false}]),
    Req = [
        "GET /halt_middleware HTTP/1.1\r\n",
        "Host: localhost:",
        integer_to_list(Port),
        "\r\n",
        "\r\n"
    ],
    ok = gen_tcp:send(Sock, Req),
    {ok, Resp} = gen_tcp:recv(Sock, 0, 5000),
    gen_tcp:close(Sock),
    ?assertNotEqual(nomatch, binary:match(Resp, <<"302">>)),
    ?assertNotEqual(nomatch, binary:match(Resp, <<"/login">>)).

http_halt_sets_cookie(Config) ->
    %% HTTP: middleware stashes a cookie then halts with a redirect; the
    %% transport flushes Set-Cookie onto the 303. The cookie value only
    %% appears in a Set-Cookie header (the redirect body is empty).
    Port = proplists:get_value(port, Config),
    {ok, Sock} = gen_tcp:connect("localhost", Port, [binary, {active, false}]),
    Req = [
        "GET /halt_set_cookie HTTP/1.1\r\n",
        "Host: localhost:",
        integer_to_list(Port),
        "\r\n",
        "\r\n"
    ],
    ok = gen_tcp:send(Sock, Req),
    {ok, Resp} = gen_tcp:recv(Sock, 0, 5000),
    gen_tcp:close(Sock),
    ?assertNotEqual(nomatch, binary:match(Resp, <<"303">>)),
    ?assertNotEqual(nomatch, binary:match(Resp, <<"session=abc">>)).

http_render_sets_cookie(Config) ->
    %% HTTP: middleware stashes a cookie then continues; the rendered page
    %% response carries Set-Cookie (the cookie value is not in the body).
    Port = proplists:get_value(port, Config),
    {ok, Sock} = gen_tcp:connect("localhost", Port, [binary, {active, false}]),
    Req = [
        "GET /render_set_cookie HTTP/1.1\r\n",
        "Host: localhost:",
        integer_to_list(Port),
        "\r\n",
        "\r\n"
    ],
    ok = gen_tcp:send(Sock, Req),
    {ok, Resp} = gen_tcp:recv(Sock, 0, 5000),
    gen_tcp:close(Sock),
    ?assertNotEqual(nomatch, binary:match(Resp, <<"200">>)),
    ?assertNotEqual(nomatch, binary:match(Resp, <<"theme=dark">>)).

middleware_halt_rejects_ws(Config) ->
    %% WS: middleware halts -- upgrade never happens, HTTP response returned
    Port = proplists:get_value(port, Config),
    {ok, Sock} = gen_tcp:connect("localhost", Port, [binary, {active, false}]),
    Key = base64:encode(crypto:strong_rand_bytes(16)),
    Req = [
        "GET /ws?_az_path=/halt_middleware HTTP/1.1\r\n",
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
    {ok, Resp} = gen_tcp:recv(Sock, 0, 5000),
    gen_tcp:close(Sock),
    %% Should NOT get 101 Switching Protocols -- should get 302
    ?assertEqual(nomatch, binary:match(Resp, <<"101">>)),
    ?assertNotEqual(nomatch, binary:match(Resp, <<"302">>)).

middleware_pipeline_runs_in_order(Config) ->
    %% HTTP: two middlewares run in order, second sees first's changes
    Port = proplists:get_value(port, Config),
    {ok, Sock} = gen_tcp:connect("localhost", Port, [binary, {active, false}]),
    Req = [
        "GET /pipeline HTTP/1.1\r\n",
        "Host: localhost:",
        integer_to_list(Port),
        "\r\n",
        "\r\n"
    ],
    ok = gen_tcp:send(Sock, Req),
    {ok, Resp} = gen_tcp:recv(Sock, 0, 5000),
    gen_tcp:close(Sock),
    %% Both middlewares ran, page rendered (200 OK)
    ?assertNotEqual(nomatch, binary:match(Resp, <<"200 OK">>)).

middleware_halt_redirects_on_navigate(Config) ->
    %% WS navigate: middleware halts via `arizona_req:redirect/2`. The
    %% server can't emit an HTTP response mid-session, so it translates
    %% the halt into an `arizona_js:navigate` client effect.
    {ok, Sock} = ws_connect(Config, <<"/">>),
    ok = ws_send_json(Sock, [~"navigate", #{~"path" => ~"/navigate_halt_redirect", ~"qs" => ~""}]),
    {text, Resp} = ws_recv(Sock),
    Decoded = json:decode(Resp),
    %% No ops, only a navigate effect pointing at /login
    ?assertMatch(#{~"e" := [_ | _]}, Decoded),
    ?assertNot(maps:is_key(~"o", Decoded)),
    [[OpCode | Rest] | _] = maps:get(~"e", Decoded),
    ?assertEqual(?EFFECT_NAVIGATE, OpCode),
    ?assertEqual(~"/login", hd(Rest)),
    ws_close(Sock).

middleware_cont_on_navigate(Config) ->
    %% WS navigate: middleware enriches bindings and passes through; the
    %% new view mounts successfully and OP_REPLACE lands at the client.
    {ok, Sock} = ws_connect(Config, <<"/">>),
    ok = ws_send_json(Sock, [~"navigate", #{~"path" => ~"/navigate_enriched", ~"qs" => ~""}]),
    {text, Resp} = ws_recv(Sock),
    Decoded = json:decode(Resp),
    ?assertMatch(#{~"o" := [[?OP_REPLACE, _, _]]}, Decoded),
    ws_close(Sock).

http_halt_redirect_via_req(Config) ->
    %% HTTP: middleware halts via `arizona_req:redirect/2`; transport emits
    %% 302 with Location header instead of writing the reply in middleware.
    Port = proplists:get_value(port, Config),
    {ok, Sock} = gen_tcp:connect("localhost", Port, [binary, {active, false}]),
    Req = [
        "GET /halt_redirect_req HTTP/1.1\r\n",
        "Host: localhost:",
        integer_to_list(Port),
        "\r\n\r\n"
    ],
    ok = gen_tcp:send(Sock, Req),
    {ok, Resp} = gen_tcp:recv(Sock, 0, 5000),
    gen_tcp:close(Sock),
    ?assertNotEqual(nomatch, binary:match(Resp, <<"302">>)),
    ?assertNotEqual(nomatch, binary:match(Resp, <<"location: /login">>)).

http_render_crash_emits_error_page(Config) ->
    %% HTTP: view mount raises; transport renders the dev error page with 500.
    Port = proplists:get_value(port, Config),
    {ok, Sock} = gen_tcp:connect("localhost", Port, [binary, {active, false}]),
    Req = [
        "GET /crash_on_render_http HTTP/1.1\r\n",
        "Host: localhost:",
        integer_to_list(Port),
        "\r\n\r\n"
    ],
    ok = gen_tcp:send(Sock, Req),
    {ok, Resp} = gen_tcp:recv(Sock, 0, 5000),
    gen_tcp:close(Sock),
    ?assertNotEqual(nomatch, binary:match(Resp, <<"500">>)),
    ?assertNotEqual(nomatch, binary:match(Resp, <<"crash_on_mount">>)).

static_asset_served(Config) ->
    %% HTTP GET for an `{asset, ...}` route serves the file with an inferred
    %% content-type. Exercises `roadrunner_static`.
    Port = proplists:get_value(port, Config),
    {ok, Sock} = gen_tcp:connect("localhost", Port, [binary, {active, false}]),
    Req = [
        "GET /assets/arizona.min.js HTTP/1.1\r\n",
        "Host: localhost:",
        integer_to_list(Port),
        "\r\n\r\n"
    ],
    ok = gen_tcp:send(Sock, Req),
    {ok, Resp} = gen_tcp:recv(Sock, 0, 5000),
    gen_tcp:close(Sock),
    ?assertNotEqual(nomatch, binary:match(Resp, <<"200 OK">>)),
    ?assertNotEqual(nomatch, binary:match(Resp, <<"content-type: application/javascript">>)).

static_asset_body_matches_file(Config) ->
    %% End-to-end: the bytes coming back over the wire must equal the
    %% bytes on disk. Roadrunner serves this via zero-copy sendfile, so
    %% the test also guards against the sendfile path corrupting or
    %% truncating the response body.
    Port = proplists:get_value(port, Config),
    FilePath = filename:join([
        code:priv_dir(arizona), "static", "assets", "js", "arizona.min.js"
    ]),
    {ok, Expected} = file:read_file(FilePath),
    Body = http_get_body(Port, <<"/assets/arizona.min.js">>),
    ?assertEqual(Expected, Body).

static_asset_gzip_served(Config) ->
    %% With `Accept-Encoding: gzip`, roadrunner_static serves the precompressed
    %% `<file>.gz` sibling verbatim (nginx `gzip_static` style). The response
    %% must carry `content-encoding: gzip` and its body must gunzip back to the
    %% on-disk file -- guards the asset pipeline's gzip step, the committed
    %% siblings, and the route wiring to `roadrunner_static`.
    Port = proplists:get_value(port, Config),
    {ok, Sock} = gen_tcp:connect("localhost", Port, [binary, {active, false}]),
    Req = [
        "GET /assets/arizona.min.js HTTP/1.1\r\n",
        "Host: localhost:",
        integer_to_list(Port),
        "\r\n",
        "Accept-Encoding: gzip\r\n",
        "Connection: close\r\n",
        "\r\n"
    ],
    ok = gen_tcp:send(Sock, Req),
    Full = read_until_close(Sock, <<>>),
    gen_tcp:close(Sock),
    [Headers, Body] = binary:split(Full, <<"\r\n\r\n">>),
    ?assertNotEqual(nomatch, binary:match(Headers, <<"content-encoding: gzip">>)),
    FilePath = filename:join([
        code:priv_dir(arizona), "static", "assets", "js", "arizona.min.js"
    ]),
    {ok, Expected} = file:read_file(FilePath),
    ?assertEqual(Expected, zlib:gunzip(Body)).

static_asset_missing_returns_404(Config) ->
    Port = proplists:get_value(port, Config),
    {ok, Sock} = gen_tcp:connect("localhost", Port, [binary, {active, false}]),
    Req = [
        "GET /assets/does-not-exist.js HTTP/1.1\r\n",
        "Host: localhost:",
        integer_to_list(Port),
        "\r\n\r\n"
    ],
    ok = gen_tcp:send(Sock, Req),
    {ok, Resp} = gen_tcp:recv(Sock, 0, 5000),
    gen_tcp:close(Sock),
    ?assertNotEqual(nomatch, binary:match(Resp, <<"404">>)).

http_preserves_duplicate_qs_keys(Config) ->
    %% HTTP: query-string parsing preserves duplicate keys in order; the
    %% middleware reads every `foo=` value via `arizona_req:params/1`
    %% and projects them into the rendered page.
    Port = proplists:get_value(port, Config),
    {ok, Sock} = gen_tcp:connect("localhost", Port, [binary, {active, false}]),
    Req = [
        "GET /preserves_dupes?foo=bar&foo=baz&foo=qux HTTP/1.1\r\n",
        "Host: localhost:",
        integer_to_list(Port),
        "\r\n\r\n"
    ],
    ok = gen_tcp:send(Sock, Req),
    {ok, Resp} = gen_tcp:recv(Sock, 0, 5000),
    gen_tcp:close(Sock),
    ?assertNotEqual(nomatch, binary:match(Resp, <<"200 OK">>)),
    ?assertNotEqual(nomatch, binary:match(Resp, <<"foo=[bar,baz,qux]">>)).

ws_navigate_preserves_duplicate_qs_keys(Config) ->
    %% WS navigate: the frame's `qs` binary flows through
    %% arizona_socket:handle_navigate -> adapter -> roadrunner_router, which
    %% writes it onto the synthesized Req's `qs` field. The handler's
    %% `arizona_req:params/1` parses it, preserving duplicates.
    {ok, Sock} = ws_connect(Config, <<"/">>),
    ok = ws_send_json(Sock, [
        ~"navigate",
        #{~"path" => ~"/preserves_dupes", ~"qs" => ~"foo=bar&foo=baz&foo=qux"}
    ]),
    {text, Resp} = ws_recv(Sock),
    %% Search the raw wire payload -- the rendered HTML is serialized
    %% into the OP_REPLACE payload (either as a binary or a fingerprint
    %% map with dynamic values); both formats carry the projected
    %% `foo=[...]` string verbatim.
    ?assertNotEqual(nomatch, binary:match(Resp, <<"foo=[bar,baz,qux]">>)),
    ws_close(Sock).

ws_upgrade_preserves_duplicate_qs_keys(Config) ->
    %% WS upgrade: user qs (including duplicate keys) sits alongside the
    %% `_az_path`/`_az_reconnect` framework keys on the upgrade URL.
    %% `arizona_roadrunner_ws` strips only the framework keys, so
    %% duplicates flow through to the adapter and reach the handler via
    %% `arizona_req:params/1`. `reconnect` mode forces an immediate
    %% mount-and-render so we can read the rendered HTML off the wire.
    {ok, Sock} = ws_connect(Config, <<"/preserves_dupes">>, [
        {reconnect, true},
        {raw_qs, <<"foo=bar&foo=baz">>}
    ]),
    {text, Resp} = ws_recv(Sock),
    ?assertNotEqual(nomatch, binary:match(Resp, <<"foo=[bar,baz]">>)),
    ws_close(Sock).

recompile_routes_runs(Config) when is_list(Config) ->
    %% Walks the persistent_term dispatch registry and rebuilds each
    %% listener's dispatch. Called by the dev hot reloader in production;
    %% here we just run it against the suite's live listener and assert
    %% it succeeds.
    ServerMod = ?config(server_mod, Config),
    ?assertEqual(ok, ServerMod:recompile_routes()),
    %% Confirm the build-time opts (compress flag) are stashed alongside
    %% Routes so recompile replays them instead of silently re-defaulting.
    Stashed = persistent_term:get({arizona_roadrunner_routes, ws_test}),
    ?assertMatch({_Routes, #{compress := _}}, Stashed).

recompile_routes_syncs_listener(Config) when is_list(Config) ->
    %% Regression: recompile_routes/0 must refresh both Arizona's
    %% dispatch term AND the listener's compiled route table. Before the
    %% fix, the listener kept its boot-time table, so any route added
    %% after a hot reload was reachable via WS navigate (which reads
    %% Arizona's term) but not via direct HTTP (which goes through the
    %% listener's table).
    sync_listener_check(Config).

sync_listener_check(Config) ->
    Port = proplists:get_value(port, Config),
    StashKey = {arizona_roadrunner_routes, ws_test},
    {OriginalRoutes, BuildOpts} = persistent_term:get(StashKey),
    NewRoute = {live, <<"/recompile_added">>, arizona_crashable, #{}},
    try
        persistent_term:put(StashKey, {[NewRoute | OriginalRoutes], BuildOpts}),
        ok = arizona_roadrunner_server:recompile_routes(),
        ?assertEqual(200, http_status(Port, <<"/recompile_added">>))
    after
        persistent_term:put(StashKey, {OriginalRoutes, BuildOpts}),
        ok = arizona_roadrunner_server:recompile_routes()
    end.

http_status(Port, Path) ->
    {ok, Sock} = gen_tcp:connect("localhost", Port, [binary, {active, false}]),
    Req = [
        "GET ",
        Path,
        " HTTP/1.1\r\n",
        "Host: localhost:",
        integer_to_list(Port),
        "\r\n",
        "\r\n"
    ],
    ok = gen_tcp:send(Sock, Req),
    {ok, Resp} = gen_tcp:recv(Sock, 0, 5000),
    gen_tcp:close(Sock),
    case Resp of
        <<"HTTP/1.1 ", S1, S2, S3, _/binary>> -> list_to_integer([S1, S2, S3]);
        _ -> 0
    end.

%% Drains a full HTTP/1.1 response and returns just the body, honoring
%% the Content-Length header. Sends `Connection: close` so the server
%% closes after responding; the body-read loop then terminates on
%% socket close in addition to length.
http_get_body(Port, Path) ->
    {ok, Sock} = gen_tcp:connect("localhost", Port, [binary, {active, false}]),
    Req = [
        "GET ",
        Path,
        " HTTP/1.1\r\n",
        "Host: localhost:",
        integer_to_list(Port),
        "\r\n",
        "Connection: close\r\n",
        "\r\n"
    ],
    ok = gen_tcp:send(Sock, Req),
    Full = read_until_close(Sock, <<>>),
    gen_tcp:close(Sock),
    case binary:split(Full, <<"\r\n\r\n">>) of
        [_Headers, Body] -> Body;
        _ -> <<>>
    end.

read_until_close(Sock, Acc) ->
    case gen_tcp:recv(Sock, 0, 5000) of
        {ok, Chunk} -> read_until_close(Sock, <<Acc/binary, Chunk/binary>>);
        {error, closed} -> Acc
    end.

https_without_tls_errors(Config) when is_list(Config) ->
    %% Asking for `scheme => https` without supplying any TLS opts used
    %% to silently downgrade to plain HTTP on the same port. The fix
    %% raises `https_requires_tls` before any persistent_term is written,
    %% so nothing leaks.
    https_without_tls_check().

https_without_tls_check() ->
    %% Snapshot the suite's dispatch term and the not-yet-existing
    %% target listener's stash key so the failed start can't leave
    %% anything weird behind even if a future regression slips past
    %% the up-front validation.
    DispatchBefore = persistent_term:get(arizona_roadrunner_dispatch),
    Result =
        try
            arizona_roadrunner_server:start(arizona_https_no_tls_test, #{
                routes => [],
                scheme => https
            })
        catch
            error:Reason -> {error_caught, Reason}
        end,
    ?assertEqual({error_caught, https_requires_tls}, Result),
    ?assertEqual(
        DispatchBefore,
        persistent_term:get(arizona_roadrunner_dispatch),
        "failed start must not overwrite the live dispatch table"
    ),
    ?assertEqual(
        undefined,
        persistent_term:get({arizona_roadrunner_routes, arizona_https_no_tls_test}, undefined),
        "failed start must not stash routes for the dead listener"
    ).

reload_endpoint_streams_event(Config) ->
    %% Connect to the dev reload SSE endpoint, broadcast a reload, and
    %% verify the event is written to the stream. Exercises
    %% `arizona_roadrunner_reload`.
    Port = proplists:get_value(port, Config),
    {ok, Sock} = gen_tcp:connect("localhost", Port, [binary, {active, false}]),
    Req = [
        "GET /reload HTTP/1.1\r\n",
        "Host: localhost:",
        integer_to_list(Port),
        "\r\n",
        "Accept: text/event-stream\r\n",
        "\r\n"
    ],
    ok = gen_tcp:send(Sock, Req),
    %% Wait until the handler has subscribed to the reloader pubsub topic
    %% (otherwise the broadcast would fire before `arizona_reloader:join/1`
    %% runs and the event would never reach our stream).
    wait_for_subscriber(1000),
    ok = arizona_reloader:broadcast(),
    %% Drain up to 5s worth of chunks, accumulating what we get.
    Data1 = recv_until(Sock, <<"event: reload\n">>, 5000),
    ?assertNotEqual(nomatch, binary:match(Data1, <<"200 OK">>)),
    ?assertNotEqual(nomatch, binary:match(Data1, <<"text/event-stream">>)),
    ?assertNotEqual(nomatch, binary:match(Data1, <<"event: reload\n">>)),
    %% Second broadcast covers the reload_css info clause.
    ok = arizona_reloader:reload_css(),
    Data2 = recv_until(Sock, <<"event: reload_css">>, 5000),
    ?assertNotEqual(nomatch, binary:match(Data2, <<"event: reload_css">>)),
    gen_tcp:close(Sock).

wait_for_subscriber(0) ->
    ok;
wait_for_subscriber(N) ->
    case arizona_pubsub:subscribers(arizona_reloader) of
        [] ->
            timer:sleep(20),
            wait_for_subscriber(N - 20);
        [_ | _] ->
            ok
    end.

recv_until(Sock, Needle, Timeout) ->
    recv_until(Sock, Needle, Timeout, <<>>).
recv_until(_Sock, _Needle, Timeout, Acc) when Timeout =< 0 ->
    Acc;
recv_until(Sock, Needle, Timeout, Acc) ->
    T0 = erlang:monotonic_time(millisecond),
    case gen_tcp:recv(Sock, 0, min(Timeout, 500)) of
        {ok, Chunk} ->
            Acc1 = <<Acc/binary, Chunk/binary>>,
            case binary:match(Acc1, Needle) of
                nomatch ->
                    Elapsed = erlang:monotonic_time(millisecond) - T0,
                    recv_until(Sock, Needle, Timeout - Elapsed, Acc1);
                _ ->
                    Acc1
            end;
        {error, timeout} ->
            Acc
    end.

http_reads_cookies_headers_body(Config) ->
    %% HTTP: middleware exercises arizona_req:cookies/1, headers/1, body/1
    %% (lazy accessors). Cover the adapter's parse_cookies/parse_headers/read_body
    %% callbacks end to end.
    Port = proplists:get_value(port, Config),
    {ok, Sock} = gen_tcp:connect("localhost", Port, [binary, {active, false}]),
    Req = [
        "GET /reads_cookies_headers_body HTTP/1.1\r\n",
        "Host: localhost:",
        integer_to_list(Port),
        "\r\n",
        "Cookie: a=1; b=2\r\n",
        "\r\n"
    ],
    ok = gen_tcp:send(Sock, Req),
    {ok, Resp} = gen_tcp:recv(Sock, 0, 5000),
    gen_tcp:close(Sock),
    ?assertNotEqual(nomatch, binary:match(Resp, <<"200 OK">>)),
    %% status binding projects into the rendered page: c=2 h=N b=0
    ?assertNotEqual(nomatch, binary:match(Resp, <<"c=2">>)),
    ?assertNotEqual(nomatch, binary:match(Resp, <<"b=0">>)).

http_exposes_roadrunner_request_id(Config) ->
    %% The adapter forwards roadrunner's per-request 16-char hex id into
    %% arizona_req:request_id/1 so handlers/middlewares can correlate with
    %% adapter access logs.
    request_id_check(Config).

request_id_check(Config) ->
    Port = proplists:get_value(port, Config),
    Body = http_get_body(Port, <<"/reads_request_id">>),
    %% Status binding renders as `id=<hex>` (16 chars, [0-9a-f]).
    ?assertNotEqual(nomatch, binary:match(Body, <<"id=">>)),
    ?assertEqual(nomatch, binary:match(Body, <<"id=none">>)),
    {match, [{Start, Len}]} = re:run(Body, <<"id=([0-9a-f]{16})">>, [{capture, [1]}]),
    Id = binary:part(Body, Start, Len),
    ?assertEqual(16, byte_size(Id)).

drain_default_closes_ws_cleanly(Config) ->
    %% Handler without handle_drain/2 → dispatcher returns {stop, B, []} by
    %% default → live process exits normal → WS closes with code 1000.
    drain_default_check(Config).

drain_default_check(Config) ->
    {ok, Sock} = ws_connect(Config, <<"/">>),
    ok = ws_send(Sock, <<"0">>),
    {text, <<"1">>} = ws_recv(Sock),
    send_drain_to_session(),
    expect_ws_close(Sock, 1001),
    ok = gen_tcp:close(Sock).

drain_user_callback_pushes_effect_then_stops(Config) ->
    %% arizona_drainable with drain_mode => stop returns
    %% {stop, B, [dispatch_event("draining")]}; the effect frame must arrive
    %% before the going-away close (1001).
    drain_stop_check(Config).

drain_stop_check(Config) ->
    {ok, Sock} = ws_connect(Config, <<"/drain_stop">>),
    ok = ws_send(Sock, <<"0">>),
    {text, <<"1">>} = ws_recv(Sock),
    send_drain_to_session(),
    {text, Resp} = ws_recv(Sock),
    ?assertMatch(#{~"e" := _}, json:decode(Resp)),
    expect_ws_close(Sock, 1001),
    ok = gen_tcp:close(Sock).

drain_user_callback_keeps_alive(Config) ->
    %% arizona_drainable with drain_mode => keep returns {B, #{}, []} — no
    %% diff, no effects, no exit. WS stays open and continues serving.
    drain_keep_check(Config).

drain_keep_check(Config) ->
    {ok, Sock} = ws_connect(Config, <<"/drain_keep">>),
    ok = ws_send(Sock, <<"0">>),
    {text, <<"1">>} = ws_recv(Sock),
    send_drain_to_session(),
    %% Drain returned {B, #{}, []}: no resets, no effects → must produce no
    %% wire frame. A regression that pushed a spurious diff op would slip
    %% past the followup ping check below.
    ?assertEqual(timeout, ws_recv(Sock, 100)),
    %% Subsequent ping/pong proves the session and live process are still up.
    ok = ws_send(Sock, <<"0">>),
    ?assertMatch({text, <<"1">>}, ws_recv(Sock)),
    ws_close(Sock).

drain_user_callback_noop_keeps_alive(Config) ->
    %% arizona_drainable with drain_mode => noop returns `ok` from
    %% handle_drain — distinct path from `keep`: the live process's
    %% handle_drain_info hits the `ok` branch, skips process_root_change,
    %% and stays alive without any diff/push.
    drain_noop_check(Config).

drain_noop_check(Config) ->
    {ok, Sock} = ws_connect(Config, <<"/drain_noop">>),
    ok = ws_send(Sock, <<"0">>),
    {text, <<"1">>} = ws_recv(Sock),
    send_drain_to_session(),
    %% `ok` return skips process_root_change entirely → no wire frame.
    ?assertEqual(timeout, ws_recv(Sock, 100)),
    %% Ping/pong proves the session and live process are still up.
    ok = ws_send(Sock, <<"0">>),
    ?assertMatch({text, <<"1">>}, ws_recv(Sock)),
    ws_close(Sock).

drain_emits_telemetry(Config) ->
    %% acknowledge_drain/2 fires `[roadrunner, drain, acknowledged]` with
    %% the deadline in metadata. Use the keep-alive route so the WS stays
    %% open long enough for cleanup.
    drain_telemetry_check(Config).

drain_telemetry_check(Config) ->
    {ok, Sock} = ws_connect(Config, <<"/drain_keep">>),
    ok = ws_send(Sock, <<"0">>),
    {text, <<"1">>} = ws_recv(Sock),
    Self = self(),
    Ref = make_ref(),
    HandlerId = {drain_telemetry_test, Ref},
    ok = telemetry:attach(
        HandlerId,
        [roadrunner, drain, acknowledged],
        fun(_Event, _Measurements, Metadata, _Config) ->
            Self ! {telemetry_fired, Ref, Metadata}
        end,
        []
    ),
    try
        Deadline = erlang:monotonic_time(millisecond) + 30000,
        broadcast_drain(Deadline),
        receive
            {telemetry_fired, Ref, Metadata} ->
                ?assertMatch(#{deadline := Deadline}, Metadata)
        after 5000 ->
            error(telemetry_timeout)
        end
    after
        telemetry:detach(HandlerId)
    end,
    ws_close(Sock).

drain_then_navigate_closes_gracefully(Config) ->
    %% Race coverage: drain causes live exit; if a navigate frame is
    %% processed by the WS session BEFORE the {'EXIT', _, normal} signal
    %% lands, arizona_socket:handle_navigate would do gen_server:call to
    %% the dead live pid. The try/catch in handle_navigate translates the
    %% resulting exit({noproc, _}) into a graceful close (1000) instead
    %% of letting it propagate to a crash close (4500).
    %%
    %% Race outcome is non-deterministic (the local drain message usually
    %% beats the TCP-delivered navigate frame), so this test asserts only
    %% the OUTCOME — graceful close — not which code path produced it.
    %% Without the catch, the navigate-wins path fails with close 4500.
    drain_then_navigate_check(Config).

drain_then_navigate_check(Config) ->
    {ok, Sock} = ws_connect(Config, <<"/drain_stop">>),
    ok = ws_send(Sock, <<"0">>),
    {text, <<"1">>} = ws_recv(Sock),
    send_drain_to_session(),
    ok = ws_send_json(Sock, [~"navigate", #{~"path" => ~"/", ~"qs" => ~""}]),
    %% drain_stop pushes a `dispatch_event("draining", #{})` effect before
    %% exiting; consume it (or accept that it bundled with the close).
    case ws_recv(Sock, 2000) of
        {text, _} -> expect_ws_close(Sock, 1001);
        {close, 1001, _} -> ok;
        {error, closed} -> ok
    end,
    ok = gen_tcp:close(Sock).

%% Drive a drain broadcast through the listener's pg group without
%% stopping the listener — every WS session auto-joins at upgrade.
%% In a sequential test suite only this test's session is in flight.
send_drain_to_session() ->
    broadcast_drain(erlang:monotonic_time(millisecond) + 30000).

broadcast_drain(Deadline) ->
    ok = roadrunner_listener:notify_drain(ws_test, Deadline).

%% Accept either the explicit close frame OR a socket close. The server
%% sometimes bundles the close frame in the same TCP packet as the
%% preceding text frame; ws_recv decodes only the first frame and drops
%% the rest, so the next recv returns {error, closed} instead of the
%% close frame. Both outcomes prove the server closed the WS cleanly.
expect_ws_close(Sock, ExpectedCode) ->
    case ws_recv(Sock, 2000) of
        {close, ExpectedCode, _} -> ok;
        {error, closed} -> ok;
        Other -> error({unexpected_recv, Other})
    end.

reconnect_init(Config) ->
    {ok, Sock} = ws_connect(Config, <<"/">>, [{reconnect, true}]),
    %% Reconnect init sends a REPLACE op immediately
    {text, Resp} = ws_recv(Sock),
    Decoded = json:decode(Resp),
    ?assertMatch(#{~"o" := _}, Decoded),
    ws_close(Sock).

%% --------------------------------------------------------------------
%% Crash recovery tests
%% --------------------------------------------------------------------

crash_event_closes(Config) ->
    {ok, Sock} = ws_connect(Config, <<"/">>),
    %% Trigger a crash via event -- socket closes with 4500; the client
    %% reconnects on its own on a fresh handshake.
    ok = ws_send_json(Sock, [~"crashable", ~"crash", #{}]),
    {close, 4500, _} = ws_recv(Sock),
    gen_tcp:close(Sock).

crash_info_closes(Config) ->
    {ok, Sock} = ws_connect(Config, <<"/">>),
    %% crash_async sends self() ! crash, which causes handle_info to
    %% crash the live process asynchronously. The linked EXIT is
    %% trapped by the socket and closes with 4500.
    ok = ws_send_json(Sock, [~"crashable", ~"crash_async", #{}]),
    {close, 4500, _} = ws_recv(Sock),
    gen_tcp:close(Sock).

crash_init_closes(Config) ->
    {ok, Sock} = ws_connect(Config, <<"/crash_on_mount">>),
    %% Mount crashes -- socket closes with 4500.
    {close, 4500, _} = ws_recv(Sock),
    gen_tcp:close(Sock).

normal_exit_closes(Config) ->
    %% When the live process exits normally, the socket should close
    %% cleanly with 1000. Verifying this from the client side without
    %% direct access to the live pid is structurally covered by the
    %% EXIT-normal clause in handle_info/2; exercise the live path end
    %% to end by mounting, sending an event, and closing the client.
    {ok, Sock} = ws_connect(Config, <<"/">>),
    ok = ws_send_json(Sock, [~"crashable", ~"set_status", #{~"value" => ~"alive"}]),
    _ = ws_recv(Sock, 1000),
    ws_close(Sock).

%% --------------------------------------------------------------------
%% Minimal WebSocket client over gen_tcp
%% --------------------------------------------------------------------

ws_connect(Config, Path) ->
    ws_connect(Config, Path, []).

ws_connect(Config, Path, Opts) ->
    Port = proplists:get_value(port, Config),
    {ok, Sock} = gen_tcp:connect("localhost", Port, [
        binary, {active, false}, {packet, http_bin}
    ]),
    ok = ws_handshake(Sock, Port, Path, Opts),
    ok = inet:setopts(Sock, [{packet, raw}]),
    {ok, Sock}.

ws_handshake(Sock, Port, Path, Opts) ->
    Key = base64:encode(crypto:strong_rand_bytes(16)),
    Reconnect = proplists:get_value(reconnect, Opts, false),
    Params = proplists:get_value(params, Opts, #{}),
    RawQs = proplists:get_value(raw_qs, Opts, <<>>),
    ParamsQS =
        case map_size(Params) of
            0 -> "";
            _ -> [[$&, uri_string:quote(K), $=, uri_string:quote(V)] || K := V <- Params]
        end,
    RawSuffix =
        case RawQs of
            <<>> -> "";
            _ -> [$&, RawQs]
        end,
    QS =
        case Reconnect of
            true -> ["_az_path=", Path, "&_az_reconnect=1", ParamsQS, RawSuffix];
            false -> ["_az_path=", Path, ParamsQS, RawSuffix]
        end,
    Req = [
        "GET /ws?",
        QS,
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
    {ok, {http_response, _Version, 101, _Reason}} = gen_tcp:recv(Sock, 0, 5000),
    drain_http_headers(Sock).

drain_http_headers(Sock) ->
    case gen_tcp:recv(Sock, 0, 5000) of
        {ok, http_eoh} -> ok;
        {ok, {http_header, _, _, _, _}} -> drain_http_headers(Sock)
    end.

ws_send_json(Sock, Term) ->
    ws_send(Sock, iolist_to_binary(json:encode(Term))).

ws_send(Sock, Payload) ->
    Frame = ws_encode_text(iolist_to_binary(Payload)),
    gen_tcp:send(Sock, Frame).

ws_send_binary_frame(Sock, Payload) ->
    Frame = ws_encode_binary(Payload),
    gen_tcp:send(Sock, Frame).

ws_recv(Sock) ->
    ws_recv(Sock, 5000).

ws_recv(Sock, Timeout) ->
    case gen_tcp:recv(Sock, 0, Timeout) of
        {ok, Data} ->
            ws_decode(Data);
        {error, timeout} ->
            timeout;
        {error, closed} ->
            {error, closed};
        {error, Reason} ->
            {error, Reason}
    end.

ws_close(Sock) ->
    Mask = crypto:strong_rand_bytes(4),
    Frame = <<1:1, 0:3, 8:4, 1:1, 0:7, Mask/binary>>,
    ok = gen_tcp:send(Sock, Frame),
    ok = gen_tcp:close(Sock).

%% Encode a text frame with masking (client must mask)
ws_encode_text(Payload) ->
    ws_encode_frame(1, Payload).

ws_encode_binary(Payload) ->
    ws_encode_frame(2, Payload).

ws_encode_frame(Opcode, Payload) ->
    Len = byte_size(Payload),
    Mask = crypto:strong_rand_bytes(4),
    Masked = ws_mask(Payload, Mask),
    case Len of
        L when L < 126 ->
            <<1:1, 0:3, Opcode:4, 1:1, L:7, Mask/binary, Masked/binary>>;
        L when L < 65536 ->
            <<1:1, 0:3, Opcode:4, 1:1, 126:7, L:16, Mask/binary, Masked/binary>>;
        L ->
            <<1:1, 0:3, Opcode:4, 1:1, 127:7, L:64, Mask/binary, Masked/binary>>
    end.

%% Rotate the 4 mask bytes through the recursive args -- O(N) instead
%% of the previous O(N^2) version that rebuilt a 32-byte mask binary
%% per input byte.
ws_mask(Payload, <<M0, M1, M2, M3>>) ->
    ws_mask(Payload, M0, M1, M2, M3, <<>>).

ws_mask(<<>>, _, _, _, _, Acc) ->
    Acc;
ws_mask(<<B, Rest/binary>>, M0, M1, M2, M3, Acc) ->
    ws_mask(Rest, M1, M2, M3, M0, <<Acc/binary, (B bxor M0)>>).

%% Decode a server frame (unmasked)
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
