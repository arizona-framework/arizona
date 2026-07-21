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
Wraps a roadrunner request in an `arizona_req:request()` with `method`,
`path`, and roadrunner's `request_id` eagerly populated. The
request_id is the same 16-char hex token roadrunner mirrors into its
logger metadata, so Arizona-side handlers and crash reports can be
correlated with the adapter's access logs.
""".
-spec new(RoadrunnerReq) -> arizona_req:request() when
    RoadrunnerReq :: roadrunner_req:request().
new(RoadrunnerReq) ->
    arizona_req:new(?MODULE, RoadrunnerReq, #{
        method => roadrunner_req:method(RoadrunnerReq),
        path => roadrunner_req:path(RoadrunnerReq),
        request_id => roadrunner_req:request_id(RoadrunnerReq)
    }).

%% --------------------------------------------------------------------
%% arizona_req behaviour callbacks
%% --------------------------------------------------------------------

-spec parse_bindings(RoadrunnerReq) -> arizona_req:bindings() when
    RoadrunnerReq :: roadrunner_req:request().
parse_bindings(RoadrunnerReq) ->
    %% Roadrunner returns binary-keyed bindings already, matching the
    %% "binary keys for wire-derived data" rule. Pass through unchanged.
    roadrunner_req:bindings(RoadrunnerReq).

-spec parse_params(RoadrunnerReq) -> arizona_req:params() when
    RoadrunnerReq :: roadrunner_req:request().
parse_params(RoadrunnerReq) ->
    roadrunner_req:parse_qs(RoadrunnerReq).

-spec parse_cookies(RoadrunnerReq) -> arizona_req:cookies() when
    RoadrunnerReq :: roadrunner_req:request().
parse_cookies(RoadrunnerReq) ->
    roadrunner_req:parse_cookies(RoadrunnerReq).

-spec parse_headers(RoadrunnerReq) -> arizona_req:headers() when
    RoadrunnerReq :: roadrunner_req:request().
parse_headers(RoadrunnerReq) ->
    maps:from_list(roadrunner_req:headers(RoadrunnerReq)).

-spec read_body(RoadrunnerReq) -> {arizona_req:body(), RoadrunnerReq} when
    RoadrunnerReq :: roadrunner_req:request().
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
    Req :: roadrunner_req:request(),
    Handler :: module(),
    RouteOpts :: arizona_live:route_opts(),
    ArzReq :: arizona_req:request().
resolve_route(Path, Qs, Req) ->
    %% Roadrunner exposes the matched route's handler + user-attached state as
    %% elements 2 and 5 of `match/3`'s return; arizona stashes its per-route
    %% metadata under the state's `arizona` namespace. Only a **live** (page)
    %% route can back a WS upgrade or an SPA navigate/patch, and roadrunner tags
    %% those with the `arizona_roadrunner_http` handler. Anything else -- a
    %% controller/asset/ws route, a method mismatch (the upgrade is a GET so a
    %% POST-only controller path is `method_not_allowed`), or no match at all --
    %% resolves to `error`. `Path` is attacker-controllable via `_az_path`, so a
    %% badmatch crash here would be a remote 500 / log-spam vector; the callers
    %% translate `error` to a 404 upgrade rejection or a full-page navigate.
    Compiled = persistent_term:get(?DISPATCH_KEY),
    case roadrunner_router:match(roadrunner_req:method(Req), Path, Compiled) of
        {ok, arizona_roadrunner_http, RawBindings, _Pipeline, #{arizona := ArzOpts}} ->
            Target =
                case Qs of
                    <<>> -> Path;
                    _ -> <<Path/binary, "?", Qs/binary>>
                end,
            %% Overlay the resolved route `Path` (not the transport `/ws`) so
            %% `roadrunner_req:path/1` -- and thus `arizona_req:path/1` -- reports
            %% the logical route on a WS handshake/navigate, matching a plain HTTP
            %% GET to the same route. Without it the stale `/ws` upgrade path would
            %% leak to any middleware reading `path/1` for a routing decision
            %% (return-to capture, path-based gates).
            NavReq = Req#{target => Target, path => Path, bindings => RawBindings},
            ArzReq = new(NavReq),
            {ok, maps:get(handler, ArzOpts), maps:without([handler], ArzOpts), ArzReq};
        _ ->
            error
    end.
