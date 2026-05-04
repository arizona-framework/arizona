-module(arizona_roadrunner_req).
-moduledoc """
Roadrunner adapter for the `arizona_req` behaviour.

Implements the parsing callbacks by delegating each to the matching
`roadrunner_req` primitive. `new/1` wraps a raw roadrunner request in
an `arizona_req:request()` with `method` and `path` eagerly populated
and everything else lazy-loaded on first access.
""".

-behaviour(arizona_req).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([new/1]).

%% --------------------------------------------------------------------
%% arizona_req callbacks
%% --------------------------------------------------------------------

-export([parse_bindings/1]).
-export([parse_params/1]).
-export([parse_cookies/1]).
-export([parse_headers/1]).
-export([read_body/1]).
-export([resolve_route/3]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------

-define(DISPATCH_KEY, arizona_roadrunner_dispatch).

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc """
Wraps a roadrunner request in an `arizona_req:request()` with `method`
and `path` eagerly populated.
""".
-spec new(RoadrunnerReq) -> arizona_req:request() when
    RoadrunnerReq :: roadrunner_http1:request().
new(RoadrunnerReq) ->
    arizona_req:new(?MODULE, RoadrunnerReq, #{
        method => roadrunner_req:method(RoadrunnerReq),
        path => roadrunner_req:path(RoadrunnerReq)
    }).

%% --------------------------------------------------------------------
%% arizona_req behaviour callbacks
%% --------------------------------------------------------------------

-spec parse_bindings(RoadrunnerReq) -> arizona_req:bindings() when
    RoadrunnerReq :: roadrunner_http1:request().
parse_bindings(RoadrunnerReq) ->
    Bindings = roadrunner_req:bindings(RoadrunnerReq),
    maps:fold(fun(K, V, Acc) -> Acc#{binary_to_atom(K, utf8) => V} end, #{}, Bindings).

-spec parse_params(RoadrunnerReq) -> arizona_req:params() when
    RoadrunnerReq :: roadrunner_http1:request().
parse_params(RoadrunnerReq) ->
    roadrunner_req:parse_qs(RoadrunnerReq).

-spec parse_cookies(RoadrunnerReq) -> arizona_req:cookies() when
    RoadrunnerReq :: roadrunner_http1:request().
parse_cookies(RoadrunnerReq) ->
    roadrunner_req:parse_cookies(RoadrunnerReq).

-spec parse_headers(RoadrunnerReq) -> arizona_req:headers() when
    RoadrunnerReq :: roadrunner_http1:request().
parse_headers(RoadrunnerReq) ->
    maps:from_list(roadrunner_req:headers(RoadrunnerReq)).

-spec read_body(RoadrunnerReq) -> {arizona_req:body(), RoadrunnerReq} when
    RoadrunnerReq :: roadrunner_http1:request().
read_body(RoadrunnerReq) ->
    {ok, Body, RoadrunnerReq1} = roadrunner_req:read_body(RoadrunnerReq),
    {Body, RoadrunnerReq1}.

-doc """
Resolves a path to `{Handler, RouteOpts, Request}` by running the
roadrunner router against the compiled routes stashed in
`persistent_term`.

Returns the handler's static route options (including its `bindings`
config) untouched, plus a navigate-scoped `arizona_req:request()`
synthesized from the stored upgrade request with the new path,
target, and matched bindings applied.
""".
-spec resolve_route(Path, Qs, Req) -> {Handler, RouteOpts, ArzReq} when
    Path :: arizona_req:path(),
    Qs :: arizona_req:qs(),
    Req :: roadrunner_http1:request(),
    Handler :: module(),
    RouteOpts :: arizona_live:route_opts(),
    ArzReq :: arizona_req:request().
resolve_route(Path, Qs, Req) ->
    Compiled = persistent_term:get(?DISPATCH_KEY),
    {ok, _Handler, RawBindings, Opts} = roadrunner_router:match(Path, Compiled),
    %% Arizona's per-route opts are namespaced under the `arizona` key
    %% inside route_opts so roadrunner's pipeline does not interpret
    %% arizona's middleware list as its own (incompatible signatures).
    #{arizona := ArzOpts} = Opts,
    Target =
        case Qs of
            <<>> -> Path;
            _ -> <<Path/binary, "?", Qs/binary>>
        end,
    NavReq = Req#{target => Target, bindings => RawBindings},
    ArzReq = new(NavReq),
    {maps:get(handler, ArzOpts), maps:without([handler], ArzOpts), ArzReq}.
