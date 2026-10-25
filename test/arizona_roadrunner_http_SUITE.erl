-module(arizona_roadrunner_http_SUITE).
-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).

-export([handle_returns_the_threaded_raw_request/1]).
-export([handle_returns_the_threaded_raw_request_on_a_render_crash/1]).
-export([manual_body_read_keeps_pipelining_intact/1]).
-export([manual_body_over_the_cap_halts_with_413/1]).
-export([manual_body_cut_short_halts_with_400/1]).
-export([manual_body_oversized_trailer_halts_with_431/1]).
-export([body_read_across_recvs_is_a_binary/1]).
-export([transforms_apply_over_the_wire/1]).
-export([page_carries_its_targets_content_type/1]).
-export([cors_answers_a_preflight/1]).
-export([cors_route_answers_a_plain_options/1]).
-export([cors_route_keeps_its_verbs_and_405/1]).
-export([cors_plain_options_allow_follows_pattern_overlaps/1]).
-export([cors_beside_default_origin_check/1]).
-export([asset_route_answers_get_and_head_only/1]).
-export([stopped_dev_listener_leaves_no_dev_error_page/1]).

all() ->
    [
        handle_returns_the_threaded_raw_request,
        handle_returns_the_threaded_raw_request_on_a_render_crash,
        manual_body_read_keeps_pipelining_intact,
        manual_body_over_the_cap_halts_with_413,
        manual_body_cut_short_halts_with_400,
        manual_body_oversized_trailer_halts_with_431,
        body_read_across_recvs_is_a_binary,
        transforms_apply_over_the_wire,
        page_carries_its_targets_content_type,
        cors_answers_a_preflight,
        cors_route_answers_a_plain_options,
        cors_route_keeps_its_verbs_and_405,
        cors_plain_options_allow_follows_pattern_overlaps,
        cors_beside_default_origin_check,
        asset_route_answers_get_and_head_only,
        stopped_dev_listener_leaves_no_dev_error_page
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(arizona),
    {ok, _} = application:ensure_all_started(roadrunner),
    Listener = arizona_roadrunner_http_test,
    Port = arizona_test_port:pick(),
    AssetDir = filename:join(?config(priv_dir, Config), "assets"),
    ok = filelib:ensure_path(AssetDir),
    ok = file:write_file(filename:join(AssetDir, "app.txt"), ~"asset-bytes"),
    Routes = [
        {asset, ~"/files", {dir, AssetDir}},
        {asset, ~"/cors_files", {dir, AssetDir}, #{
            middlewares => [arizona_middleware:cors(#{origins => [~"https://other.example"]})]
        }},
        {live, ~"/echo_body", arizona_about, #{
            middlewares => [arizona_middleware:extract([body])]
        }},
        %% Response-transform wire fixtures: a controller with a deterministic
        %% body, and a live page big enough (>= roadrunner_compress's 860-byte
        %% threshold) that the compress/etag composition is real over the wire.
        {get, ~"/etag_api", arizona_transform_probe_controller, #{
            middlewares => [arizona_middleware:etag()]
        }},
        {live, ~"/etag_page", arizona_about, #{
            bindings => #{
                tags => [<<"tag-", (integer_to_binary(I))/binary>> || I <- lists:seq(1, 100)]
            },
            middlewares => [arizona_middleware:etag()]
        }},
        {live, ~"/native_page", arizona_native_counter_demo, #{}},
        {live, ~"/crash_page", arizona_crashable, #{bindings => #{crash_on_mount => true}}},
        %% CORS wire fixtures: a POST controller sharing its path with a
        %% cors-less DELETE sibling, and a live page.
        {post, ~"/cors_api", arizona_transform_probe_controller, #{
            check_origin => false, middlewares => [cors_transform()]
        }},
        {delete, ~"/cors_api", arizona_transform_probe_controller, #{}},
        {live, ~"/cors_page", arizona_about, #{
            check_origin => false, middlewares => [cors_transform()]
        }},
        %% Siblings that match the same URLs through a different pattern: a
        %% param under another name, a param beside a literal, a wildcard.
        {post, ~"/ov/:id", arizona_transform_probe_controller, #{
            check_origin => false, middlewares => [cors_transform()]
        }},
        {delete, ~"/ov/:name", arizona_transform_probe_controller, #{}},
        {post, ~"/ovl/me", arizona_transform_probe_controller, #{
            check_origin => false, middlewares => [cors_transform()]
        }},
        {delete, ~"/ovl/:id", arizona_transform_probe_controller, #{}},
        {post, ~"/ovw/x", arizona_transform_probe_controller, #{
            check_origin => false, middlewares => [cors_transform()]
        }},
        {delete, ~"/ovw/*rest", arizona_transform_probe_controller, #{}},
        %% cors without opting out of the default Origin check.
        {post, ~"/cors_guarded", arizona_transform_probe_controller, #{
            middlewares => [cors_transform()]
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
    %% Started here, so stopped here: `arizona_app_SUITE` boots the application
    %% with its own `server` env, which a still-running one never reads.
    application:stop(arizona).

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
%% Manual-mode body read failures
%% --------------------------------------------------------------------

%% In manual mode the body is read on demand, so the cap roadrunner enforces
%% up front in auto mode surfaces only when a middleware reads it. Auto mode
%% answers an oversized body 413; the on-demand read must too, not crash.
manual_body_over_the_cap_halts_with_413(Config) when is_list(Config) ->
    Body = ~"0123456789",
    Req = framed_body_req(Body, Body, byte_size(Body) - 1, arizona_about, #{}),
    ?assertMatch({{413, _, _}, _}, arizona_roadrunner_http:handle(Req)).

%% The client declared more bytes than it sent and the connection closed: a
%% malformed request, answered 400 like auto mode answers it.
manual_body_cut_short_halts_with_400(Config) when is_list(Config) ->
    Req = framed_body_req(~"0123456789", ~"01234", 1024, arizona_about, #{}),
    ?assertMatch({{400, _, _}, _}, arizona_roadrunner_http:handle(Req)).

%% A chunked body's trailer block obeys the request-header limits; auto mode
%% answers an oversized trailer field 431, like an oversized header.
manual_body_oversized_trailer_halts_with_431(Config) when is_list(Config) ->
    #{body_reader := Reader} =
        Raw0 = framed_body_req(<<>>, <<>>, 1024, arizona_about, #{}),
    Raw = Raw0#{
        body_reader := Reader#{
            framing := chunked,
            buffered := ~"0\r\nx-trailer: far-longer-than-the-line-limit\r\n\r\n",
            trailer_limits := {16, 10240, 100}
        }
    },
    ?assertMatch({{431, _, _}, _}, arizona_roadrunner_http:handle(Raw)).

%% Roadrunner hands back a body that arrived in several socket reads as
%% iodata; `arizona_req:body()` is a binary, so the adapter flattens it.
body_read_across_recvs_is_a_binary(Config) when is_list(Config) ->
    #{body_reader := Reader} =
        Raw0 = framed_body_req(~"0123456789", ~"01234", 1024, arizona_about, #{}),
    Raw = Raw0#{body_reader := Reader#{recv := fun() -> {ok, ~"56789"} end}},
    ?assertMatch(
        {ok, ~"0123456789", _}, arizona_req:body(arizona_roadrunner_req:new(Raw))
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
    framed_body_req(Body, Body, 1024, Handler, Bindings).

%% `Declared` sizes the content-length framing; `Buffered` is what actually
%% arrived (the socket then reports closed); `Max` is the body cap.
framed_body_req(Declared, Buffered, Max, Handler, Bindings) ->
    #{
        method => ~"GET",
        path => ~"/echo_body",
        target => ~"/echo_body",
        version => {1, 1},
        headers => [{~"host", ~"localhost"}],
        bindings => #{},
        body_reader => #{
            framing => {content_length, byte_size(Declared)},
            buffered => Buffered,
            bytes_read => 0,
            pending => <<>>,
            done => false,
            recv => fun() -> {error, closed} end,
            max => Max,
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

%% --------------------------------------------------------------------
%% HTTP response transforms
%% --------------------------------------------------------------------

%% The router-suite tests pin the COMPILED shape; this one proves the wire
%% behavior the docs promise. Controller leg: the etag transform tags a
%% deterministic body and answers the conditional with 304. Live leg: the
%% compress-outermost composition -- the identity and gzip responses carry
%% the SAME weak tag (the etag hashed the pre-compression body), the gzip
%% body really is the encoded identity body, and the conditional still 304s
%% through compression.
transforms_apply_over_the_wire(Config) when is_list(Config) ->
    Port = ?config(port, Config),
    {200, ApiHeaders, ApiBody} = raw_get(Port, ~"/etag_api", []),
    #{~"etag" := ApiTag} = ApiHeaders,
    ?assertMatch(<<"W/\"", _/binary>>, ApiTag),
    ?assertEqual(~"stable-transform-probe-body", ApiBody),
    {304, _, <<>>} = raw_get(Port, ~"/etag_api", [{~"if-none-match", ApiTag}]),
    {200, PlainHeaders, PlainBody} = raw_get(Port, ~"/etag_page", []),
    #{~"etag" := PageTag} = PlainHeaders,
    ?assertNot(is_map_key(~"content-encoding", PlainHeaders)),
    {200, GzipHeaders, GzipBody} = raw_get(Port, ~"/etag_page", [
        {~"accept-encoding", ~"gzip"}
    ]),
    ?assertEqual(~"gzip", maps:get(~"content-encoding", GzipHeaders)),
    ?assertEqual(PageTag, maps:get(~"etag", GzipHeaders)),
    ?assertEqual(PlainBody, zlib:gunzip(GzipBody)),
    {304, _, <<>>} = raw_get(Port, ~"/etag_page", [
        {~"accept-encoding", ~"gzip"},
        {~"if-none-match", PageTag}
    ]).

%% A `?native` route answers GET and HEAD with its JSON document under
%% `application/json`, an HTML route with `text/html`, and a render crash's error
%% page (HTML) keeps `text/html`.
page_carries_its_targets_content_type(Config) when is_list(Config) ->
    Port = ?config(port, Config),
    {200, NativeHeaders, NativeBody} = raw_get(Port, ~"/native_page", []),
    ?assertEqual(~"application/json", maps:get(~"content-type", NativeHeaders)),
    ?assertMatch(#{~"type" := ~"Column"}, json:decode(NativeBody)),
    {200, HeadHeaders, <<>>} = raw_request(Port, ~"HEAD", ~"/native_page", []),
    ?assertEqual(~"application/json", maps:get(~"content-type", HeadHeaders)),
    {200, HtmlHeaders, _} = raw_get(Port, ~"/etag_page", []),
    ?assertEqual(~"text/html; charset=utf-8", maps:get(~"content-type", HtmlHeaders)),
    Req = manual_body_req(~"", arizona_crashable, #{crash_on_mount => true}),
    {{500, CrashHeaders, _}, _} = arizona_roadrunner_http:handle(Req),
    ?assertEqual(
        {~"content-type", ~"text/html; charset=utf-8"},
        lists:keyfind(~"content-type", 1, CrashHeaders)
    ).

%% --------------------------------------------------------------------
%% CORS
%% --------------------------------------------------------------------

%% roadrunner gates a route's method allowlist before any per-route middleware,
%% so the router must open OPTIONS on a cors route or the transform never sees
%% the preflight -- it would be a 405 on both route kinds.
cors_answers_a_preflight(Config) when is_list(Config) ->
    Port = ?config(port, Config),
    Preflight = [
        {~"origin", ~"https://other.example"},
        {~"access-control-request-method", ~"POST"}
    ],
    ?assertMatch(
        {204,
            #{
                ~"access-control-allow-origin" := ~"https://other.example",
                ~"access-control-allow-methods" := ~"POST",
                ~"vary" := ~"Origin"
            },
            <<>>},
        raw_request(Port, ~"OPTIONS", ~"/cors_api", Preflight)
    ),
    ?assertMatch(
        {204, #{~"access-control-allow-origin" := ~"https://other.example"}, <<>>},
        raw_request(Port, ~"OPTIONS", ~"/cors_page", Preflight)
    ),
    %% A disallowed origin still gets the transform's answer (no
    %% allow-origin), not a 405.
    {204, Denied, <<>>} = raw_request(Port, ~"OPTIONS", ~"/cors_api", [
        {~"origin", ~"https://evil.example"},
        {~"access-control-request-method", ~"POST"}
    ]),
    ?assertNot(is_map_key(~"access-control-allow-origin", Denied)).

%% An OPTIONS that is not a preflight passes through the transform; it must be
%% answered with the path's Allow set, never run the controller action or
%% render the live page, which declared no OPTIONS.
cors_route_answers_a_plain_options(Config) when is_list(Config) ->
    Port = ?config(port, Config),
    {204, ApiHeaders, <<>>} = raw_request(Port, ~"OPTIONS", ~"/cors_api", []),
    %% The DELETE sibling shares the path, so it is listed too.
    ?assertEqual(~"DELETE, OPTIONS, POST", maps:get(~"allow", ApiHeaders)),
    ?assertNot(is_map_key(~"content-type", ApiHeaders)),
    {204, PageHeaders, <<>>} = raw_request(Port, ~"OPTIONS", ~"/cors_page", []),
    ?assertEqual(~"GET, HEAD, OPTIONS", maps:get(~"allow", PageHeaders)),
    %% With an Origin but no Access-Control-Request-Method it is an actual
    %% CORS request: same answer, decorated by the transform.
    ?assertMatch(
        {204,
            #{
                ~"allow" := ~"DELETE, OPTIONS, POST",
                ~"access-control-allow-origin" := ~"https://other.example"
            },
            <<>>},
        raw_request(Port, ~"OPTIONS", ~"/cors_api", [{~"origin", ~"https://other.example"}])
    ).

%% Opening OPTIONS changes nothing else: the declared verbs still reach their
%% handlers (decorated when cross-origin), and a 405 advertises OPTIONS now
%% that the path answers it.
cors_route_keeps_its_verbs_and_405(Config) when is_list(Config) ->
    Port = ?config(port, Config),
    Len0 = [{~"content-length", ~"0"}],
    ?assertMatch(
        {200, #{~"access-control-allow-origin" := ~"https://other.example"},
            ~"stable-transform-probe-body"},
        raw_request(Port, ~"POST", ~"/cors_api", [{~"origin", ~"https://other.example"} | Len0])
    ),
    ?assertMatch(
        {200, _, ~"stable-transform-probe-body"}, raw_request(Port, ~"DELETE", ~"/cors_api", Len0)
    ),
    ?assertMatch(
        {405, #{~"allow" := ~"DELETE, OPTIONS, POST"}, _},
        raw_request(Port, ~"PUT", ~"/cors_api", Len0)
    ),
    ?assertMatch(
        {405, #{~"allow" := ~"GET, HEAD, OPTIONS"}, _},
        raw_request(Port, ~"POST", ~"/cors_page", Len0)
    ),
    ?assertMatch({200, _, _}, raw_get(Port, ~"/cors_page", [])).

%% The Allow set is the one a 405 on that URL carries, so a sibling matching
%% the same URL through a different pattern is listed too.
cors_plain_options_allow_follows_pattern_overlaps(Config) when is_list(Config) ->
    Port = ?config(port, Config),
    [
        ?assertMatch(
            {204, #{~"allow" := ~"DELETE, OPTIONS, POST"}, <<>>},
            raw_request(Port, ~"OPTIONS", Path, [])
        )
     || Path <- [~"/ov/42", ~"/ovl/me", ~"/ovw/x"]
    ],
    ok.

%% The transform answers the preflight, but the default Origin check still
%% refuses the cross-origin request that follows: an origin a cors route
%% serves must also be trusted (csrf_origins) or the route opt out.
cors_beside_default_origin_check(Config) when is_list(Config) ->
    Port = ?config(port, Config),
    ?assertMatch(
        {204, #{~"access-control-allow-origin" := ~"https://other.example"}, <<>>},
        raw_request(Port, ~"OPTIONS", ~"/cors_guarded", [
            {~"origin", ~"https://other.example"},
            {~"access-control-request-method", ~"POST"}
        ])
    ),
    ?assertMatch(
        {403, _, _},
        raw_request(Port, ~"POST", ~"/cors_guarded", [
            {~"origin", ~"https://other.example"}, {~"content-length", ~"0"}
        ])
    ).

cors_transform() ->
    arizona_middleware:cors(#{origins => [~"https://other.example"], methods => [~"POST"]}).

%% An asset route serves files: GET and HEAD. roadrunner_static ignores the
%% method, so without an allowlist any other verb would be served the file
%% too; the router gates it to a 405 with the Allow set.
asset_route_answers_get_and_head_only(Config) when is_list(Config) ->
    Port = ?config(port, Config),
    ?assertMatch({200, _, ~"asset-bytes"}, raw_get(Port, ~"/files/app.txt", [])),
    ?assertMatch({200, _, <<>>}, raw_request(Port, ~"HEAD", ~"/files/app.txt", [])),
    Len0 = [{~"content-length", ~"0"}],
    [
        ?assertMatch(
            {405, #{~"allow" := ~"GET, HEAD"}, _},
            raw_request(Port, Method, ~"/files/app.txt", Len0)
        )
     || Method <- [~"POST", ~"DELETE", ~"OPTIONS"]
    ],
    %% A cors asset route still lets the preflight through to the transform.
    ?assertMatch(
        {204, #{~"access-control-allow-origin" := ~"https://other.example"}, <<>>},
        raw_request(Port, ~"OPTIONS", ~"/cors_files/app.txt", [
            {~"origin", ~"https://other.example"},
            {~"access-control-request-method", ~"GET"}
        ])
    ),
    %% A plain OPTIONS there is answered with the Allow set, not the file.
    ?assertMatch(
        {204, #{~"allow" := ~"GET, HEAD, OPTIONS"}, <<>>},
        raw_request(Port, ~"OPTIONS", ~"/cors_files/app.txt", [])
    ),
    ?assertMatch(
        {405, #{~"allow" := ~"GET, HEAD, OPTIONS"}, _},
        raw_request(Port, ~"POST", ~"/cors_files/app.txt", Len0)
    ).

%% --------------------------------------------------------------------
%% Dev state
%% --------------------------------------------------------------------

%% The reload URL is the node's dev signal: while set, a render crash serves the
%% rich error page (class, reason, stack). A dev listener stopped in the same
%% node must take it along, or this production listener keeps leaking internals.
stopped_dev_listener_leaves_no_dev_error_page(Config) when is_list(Config) ->
    undefined = persistent_term:get(arizona_reload_url, undefined),
    Dev = arizona_dev_listener_test,
    {ok, _} = arizona_roadrunner_server:start(Dev, #{
        routes => [{reload, ~"/dev-reload", #{}}],
        transport_opts => [{port, arizona_test_port:pick()}]
    }),
    ok = arizona_roadrunner_server:stop(Dev),
    ?assertEqual(undefined, persistent_term:get(arizona_reload_url, undefined)),
    {Status, _Headers, Body} = raw_get(?config(port, Config), ~"/crash_page", []),
    ?assertEqual(500, Status),
    ?assertEqual(nomatch, binary:match(Body, ~"crash_on_mount")).

raw_get(Port, Path, ExtraHeaders) ->
    raw_request(Port, ~"GET", Path, ExtraHeaders).

raw_request(Port, Method, Path, ExtraHeaders) ->
    {ok, Sock} = gen_tcp:connect(
        "127.0.0.1", Port, [binary, {active, false}, {packet, raw}]
    ),
    try
        HeaderLines = [[K, ~": ", V, ~"\r\n"] || {K, V} <- ExtraHeaders],
        ok = gen_tcp:send(Sock, [
            Method,
            ~" ",
            Path,
            ~" HTTP/1.1\r\nhost: localhost\r\nconnection: close\r\n",
            HeaderLines,
            ~"\r\n"
        ]),
        parse_response(recv_until_closed(Sock, <<>>))
    after
        gen_tcp:close(Sock)
    end.

recv_until_closed(Sock, Acc) ->
    case gen_tcp:recv(Sock, 0, 5000) of
        {ok, Data} -> recv_until_closed(Sock, <<Acc/binary, Data/binary>>);
        {error, closed} -> Acc
    end.

parse_response(Raw) ->
    [Head, Body] = binary:split(Raw, ~"\r\n\r\n"),
    [StatusLine | HeaderLines] = binary:split(Head, ~"\r\n", [global]),
    <<"HTTP/1.1 ", StatusBin:3/binary, _/binary>> = StatusLine,
    Headers = maps:from_list([
        {string:lowercase(Name), Value}
     || Line <- HeaderLines,
        [Name, Value] <- [binary:split(Line, ~": ")]
    ]),
    {binary_to_integer(StatusBin), Headers, Body}.
