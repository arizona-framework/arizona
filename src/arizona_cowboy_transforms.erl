-module(arizona_cowboy_transforms).
-moduledoc """
The HTTP response transforms (`arizona_route:transforms/2`) for the Cowboy
adapter, which has none of them: etag, compress, cors, and security headers,
each a function from a response to a response.

A response here is `{Status, Headers, Body}`, the header lines in send order;
`Body` is `stream` for a response whose body is not known up front (stream,
sendfile, loop, a static file), which only gets header-level transforms (cors,
security headers). Cowboy's own `cowboy_compress_h` is not used: it skips any
response that carries an etag, which would leave every `etag()` route
uncompressed.
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([preflight/3]).
-export([wrap/3]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------

%% Below this, gzip's own overhead outweighs the saving.
-define(COMPRESS_MIN, 860).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([response/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-nominal response() :: {pos_integer(), [{binary(), iodata()}], iodata() | stream}.

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc """
The CORS preflight answer, when the request is one (`OPTIONS` with `Origin` and
`Access-Control-Request-Method`) and the route carries a cors transform: a
`204` with the `Access-Control-*` set for an allowed origin, and only
`vary: origin` for any other. `none` otherwise.
""".
-spec preflight(Transforms, Method, RequestHeaders) -> response() | none when
    Transforms :: [arizona_middleware:http_transform()],
    Method :: binary(),
    RequestHeaders :: #{binary() => binary()}.
preflight(Transforms, ~"OPTIONS", #{~"origin" := Origin, ~"access-control-request-method" := _}) ->
    case [Config || {http_transform, {cors, Config}} <- Transforms] of
        [Config | _] ->
            Allowed =
                case allowed_origin(Origin, Config) of
                    {true, Value} ->
                        [{~"access-control-allow-origin", Value}] ++
                            credentials(Config) ++
                            [
                                {
                                    ~"access-control-allow-methods",
                                    join(maps:get(methods, Config, [~"GET", ~"HEAD", ~"POST"]))
                                }
                            ] ++
                            allow_headers(Config) ++ max_age(Config);
                    false ->
                        []
                end,
            {204, Allowed ++ [{~"vary", ~"origin"}], <<>>};
        [] ->
            none
    end;
preflight(_Transforms, _Method, _RequestHeaders) ->
    none.

-doc """
Applies `Transforms` (first outermost) to a response for a request with
`RequestHeaders` and `Scheme`.
""".
-spec wrap(Transforms, Request, Response) -> Response when
    Transforms :: [arizona_middleware:http_transform()],
    Request :: #{headers := #{binary() => binary()}, scheme := http | https},
    Response :: response().
wrap(Transforms, Request, Response) ->
    lists:foldr(fun(T, R) -> transform(T, Request, R) end, Response, Transforms).

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

transform({http_transform, etag}, #{headers := ReqHeaders}, {200, Headers, Body}) when
    Body =/= stream
->
    Tag = <<"W/\"", (integer_to_binary(erlang:phash2(Body, 1 bsl 32), 36))/binary, "\"">>,
    case maps:get(~"if-none-match", ReqHeaders, undefined) of
        undefined -> {200, [{~"etag", Tag} | Headers], Body};
        IfNoneMatch -> revalidate(IfNoneMatch, Tag, Headers, Body)
    end;
transform({http_transform, compress}, #{headers := ReqHeaders}, {Status, Headers, Body}) when
    Body =/= stream
->
    case
        accepts_gzip(maps:get(~"accept-encoding", ReqHeaders, <<>>)) andalso
            iolist_size(Body) >= ?COMPRESS_MIN andalso
            not lists:keymember(~"content-encoding", 1, Headers)
    of
        true ->
            {Status, Headers ++ [{~"content-encoding", ~"gzip"}, {~"vary", ~"accept-encoding"}],
                zlib:gzip(Body)};
        false ->
            {Status, Headers, Body}
    end;
transform({http_transform, {cors, Config}}, #{headers := #{~"origin" := Origin}}, {S, H, B}) ->
    Allowed =
        case allowed_origin(Origin, Config) of
            {true, Value} -> [{~"access-control-allow-origin", Value}] ++ credentials(Config);
            false -> []
        end,
    {S, H ++ Allowed ++ [{~"vary", ~"origin"}], B};
transform({http_transform, security_headers}, Request, Response) ->
    transform({http_transform, {security_headers, #{}}}, Request, Response);
transform({http_transform, {security_headers, Config}}, #{scheme := Scheme}, {S, H, B}) ->
    Defaults = [
        {~"x-content-type-options", content_type_options, ~"nosniff"},
        {~"x-frame-options", frame_options, ~"SAMEORIGIN"},
        {~"referrer-policy", referrer_policy, ~"strict-origin-when-cross-origin"}
    ],
    %% `false` drops a header; any other value overrides its default.
    Set = [
        {Name, Value}
     || {Name, Key, Default} <- Defaults,
        Value <- [maps:get(Key, Config, Default)],
        is_binary(Value)
    ],
    {S, H ++ Set ++ csp(Config) ++ hsts(Config, Scheme), B};
transform(_Transform, _Request, Response) ->
    Response.

%% A weak comparison (RFC 9110 §13.1.2): the `W/` prefix is ignored.
revalidate(IfNoneMatch, Tag, Headers, Body) ->
    Candidates = [weak(C) || C <- binary:split(IfNoneMatch, ~",", [global])],
    case lists:member(~"*", Candidates) orelse lists:member(weak(Tag), Candidates) of
        true -> {304, [{~"etag", Tag} | Headers], <<>>};
        false -> {200, [{~"etag", Tag} | Headers], Body}
    end.

weak(Tag) ->
    case string:trim(Tag) of
        <<"W/", Opaque/binary>> -> Opaque;
        Opaque -> Opaque
    end.

accepts_gzip(AcceptEncoding) ->
    lists:any(
        fun(Coding) ->
            case [string:trim(P) || P <- binary:split(Coding, ~";", [global])] of
                [~"gzip" | Params] -> not lists:member(~"q=0", Params);
                _Other -> false
            end
        end,
        binary:split(AcceptEncoding, ~",", [global])
    ).

allowed_origin(Origin, #{origins := any} = Config) ->
    case maps:get(credentials, Config, false) of
        true -> {true, Origin};
        false -> {true, ~"*"}
    end;
allowed_origin(Origin, #{origins := Origins}) when is_list(Origins) ->
    case lists:member(Origin, Origins) of
        true -> {true, Origin};
        false -> false
    end;
allowed_origin(Origin, #{origins := Allow}) when is_function(Allow, 1) ->
    case Allow(Origin) of
        true -> {true, Origin};
        _Other -> false
    end.

credentials(#{credentials := true}) -> [{~"access-control-allow-credentials", ~"true"}];
credentials(_Config) -> [].

allow_headers(#{headers := [_ | _] = Headers}) ->
    [{~"access-control-allow-headers", join(Headers)}];
allow_headers(_Config) ->
    [].

max_age(#{max_age := Seconds}) -> [{~"access-control-max-age", integer_to_binary(Seconds)}];
max_age(_Config) -> [].

csp(#{content_security_policy := Policy}) when is_binary(Policy) ->
    [{~"content-security-policy", Policy}];
csp(_Config) ->
    [].

hsts(#{hsts := true}, https) -> [{~"strict-transport-security", ~"max-age=31536000"}];
hsts(_Config, _Scheme) -> [].

join(Values) -> iolist_to_binary(lists:join(~", ", Values)).
