-module(arizona_http_transforms_SUITE).
-include_lib("stdlib/include/assert.hrl").

-export([all/0]).

-export([etag_revalidates_safe_requests_only/1]).
-export([compress_honours_quality_values/1]).
-export([cors_grants_only_allowed_origins/1]).
-export([cors_preflight_answers_with_the_policy/1]).
-export([security_headers_take_overrides_and_hsts_over_https/1]).
-export([stream_bodies_get_header_transforms_only/1]).

all() ->
    [
        etag_revalidates_safe_requests_only,
        compress_honours_quality_values,
        cors_grants_only_allowed_origins,
        cors_preflight_answers_with_the_policy,
        security_headers_take_overrides_and_hsts_over_https,
        stream_bodies_get_header_transforms_only
    ].

etag_revalidates_safe_requests_only(Config) when is_list(Config) ->
    Etag = [arizona_middleware:etag()],
    {200, [{~"etag", Tag} | _], ~"body"} = wrap(Etag, ~"GET", #{}, {200, [], ~"body"}),
    ?assertMatch(<<"W/\"", _/binary>>, Tag),
    ?assertMatch(
        {304, [{~"etag", Tag} | _], <<>>},
        wrap(Etag, ~"HEAD", #{~"if-none-match" => Tag}, {200, [], ~"body"})
    ),
    %% `W/` is ignored in the comparison.
    <<"W/", Strong/binary>> = Tag,
    ?assertMatch(
        {304, _, <<>>}, wrap(Etag, ~"GET", #{~"if-none-match" => Strong}, {200, [], ~"body"})
    ),
    %% A conditional POST still runs: no validator, no 304.
    ?assertEqual(
        {200, [], ~"body"}, wrap(Etag, ~"POST", #{~"if-none-match" => Tag}, {200, [], ~"body"})
    ).

compress_honours_quality_values(Config) when is_list(Config) ->
    Compress = [arizona_middleware:compress()],
    Big = binary:copy(~"a", 2000),
    Gzipped = fun(AcceptEncoding) ->
        {200, Headers, _} = wrap(Compress, ~"GET", #{~"accept-encoding" => AcceptEncoding}, {
            200, [], Big
        }),
        lists:keymember(~"content-encoding", 1, Headers)
    end,
    ?assert(Gzipped(~"gzip")),
    ?assert(Gzipped(~"br, GZIP;q=0.5")),
    ?assert(Gzipped(~"*")),
    ?assertNot(Gzipped(~"gzip;q=0")),
    ?assertNot(Gzipped(~"gzip;q=0.0, br")),
    ?assertNot(Gzipped(~"identity")),
    %% Too small to pay for the encoding.
    ?assertEqual(
        {200, [], ~"tiny"},
        wrap(Compress, ~"GET", #{~"accept-encoding" => ~"gzip"}, {200, [], ~"tiny"})
    ).

cors_grants_only_allowed_origins(Config) when is_list(Config) ->
    Cors = fun(Policy) -> [arizona_middleware:cors(Policy)] end,
    Trusted = ~"https://trusted.example",
    {200, Granted, _} = wrap(
        Cors(#{origins => [Trusted]}), ~"GET", #{~"origin" => Trusted}, {200, [], <<>>}
    ),
    ?assertEqual(Trusted, proplists:get_value(~"access-control-allow-origin", Granted)),
    {200, Denied, _} = wrap(
        Cors(#{origins => [Trusted]}),
        ~"GET",
        #{~"origin" => ~"https://evil.example"},
        {200, [], <<>>}
    ),
    ?assertEqual([{~"vary", ~"origin"}], Denied),
    %% `any` with credentials echoes the origin: a browser rejects `*` there.
    {200, Echoed, _} = wrap(
        Cors(#{origins => any, credentials => true}),
        ~"GET",
        #{~"origin" => Trusted},
        {200, [], <<>>}
    ),
    ?assertEqual(Trusted, proplists:get_value(~"access-control-allow-origin", Echoed)),
    {200, Star, _} = wrap(
        Cors(#{origins => any}), ~"GET", #{~"origin" => Trusted}, {200, [], <<>>}
    ),
    ?assertEqual(~"*", proplists:get_value(~"access-control-allow-origin", Star)).

cors_preflight_answers_with_the_policy(Config) when is_list(Config) ->
    Transforms = [
        arizona_middleware:cors(#{origins => [~"https://a.example"], methods => [~"POST"]})
    ],
    Preflight = #{~"origin" => ~"https://a.example", ~"access-control-request-method" => ~"POST"},
    {204, Headers, <<>>} = arizona_http_transforms:preflight(Transforms, ~"OPTIONS", Preflight),
    ?assertEqual(~"POST", proplists:get_value(~"access-control-allow-methods", Headers)),
    %% Not a preflight: no Access-Control-Request-Method, or not OPTIONS.
    ?assertEqual(
        none,
        arizona_http_transforms:preflight(Transforms, ~"OPTIONS", #{
            ~"origin" => ~"https://a.example"
        })
    ),
    ?assertEqual(none, arizona_http_transforms:preflight(Transforms, ~"POST", Preflight)),
    ?assertEqual(none, arizona_http_transforms:preflight([], ~"OPTIONS", Preflight)).

security_headers_take_overrides_and_hsts_over_https(Config) when is_list(Config) ->
    Default = [arizona_middleware:security_headers()],
    {200, Headers, _} = wrap(Default, ~"GET", #{}, {200, [], <<>>}),
    ?assertEqual(~"nosniff", proplists:get_value(~"x-content-type-options", Headers)),
    ?assertEqual(~"SAMEORIGIN", proplists:get_value(~"x-frame-options", Headers)),
    Tuned = [arizona_middleware:security_headers(#{frame_options => false, hsts => true})],
    {200, Plain, _} = wrap(Tuned, ~"GET", #{}, {200, [], <<>>}),
    ?assertNot(lists:keymember(~"x-frame-options", 1, Plain)),
    ?assertNot(lists:keymember(~"strict-transport-security", 1, Plain)),
    {200, Secure, _} = arizona_http_transforms:wrap(
        Tuned, #{method => ~"GET", headers => #{}, scheme => https}, {200, [], <<>>}
    ),
    ?assert(lists:keymember(~"strict-transport-security", 1, Secure)).

stream_bodies_get_header_transforms_only(Config) when is_list(Config) ->
    Transforms = [
        arizona_middleware:compress(),
        arizona_middleware:etag(),
        arizona_middleware:security_headers()
    ],
    {200, Headers, stream} = wrap(
        Transforms, ~"GET", #{~"accept-encoding" => ~"gzip"}, {200, [], stream}
    ),
    ?assertNot(lists:keymember(~"content-encoding", 1, Headers)),
    ?assertNot(lists:keymember(~"etag", 1, Headers)),
    ?assert(lists:keymember(~"x-content-type-options", 1, Headers)).

wrap(Transforms, Method, ReqHeaders, Response) ->
    arizona_http_transforms:wrap(
        Transforms, #{method => Method, headers => ReqHeaders, scheme => http}, Response
    ).
